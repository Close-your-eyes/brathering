#' Stitch image files into a regular grid
#'
#' Reads images from disk, normalizes each one to a common cell, and assembles
#' the cells into a single image. Cropping and resizing are controlled
#' independently so that resolution is never changed unless requested.
#'
#' @param paths Character vector of image paths, in display order.
#' @param nrow,ncol Number of grid rows and columns. If both are `NULL`, a
#'   two-column layout is used (or one column for a single image). If one is
#'   `NULL`, it is calculated from the number of images.
#' @param fit How images occupy their cells: `"contain"` preserves the complete
#'   image and pads unused space; `"crop"` fills the cell and crops overflow.
#' @param orientation Keep `"all"` input images, or only `"landscape"`,
#'   `"portrait"`, or `"square"` images. Classification uses the selected
#'   frame's dimensions after EXIF auto-orientation and optional trimming.
#' @param resize Resolution policy: `"shrink"` permits downscaling only,
#'   `"both"` permits down- or upscaling, and `"none"` never resamples pixels.
#' @param cell_width,cell_height Cell size in pixels. With neither supplied,
#'   `"contain"` uses a canvas large enough for the largest input, while
#'   `"crop"` finds the largest common crop that every input can supply without
#'   upscaling. Supplying one dimension uses `target_aspect` (or the median
#'   input aspect ratio) to calculate the other.
#' @param target_aspect Desired cell width divided by height. `NULL` infers it.
#'   When both cell dimensions are supplied, their ratio takes precedence.
#' @param background Cell padding color, such as `"white"`, `"#202020"`, or
#'   `"none"` for transparency.
#' @param canvas_background Color of gaps, margins, and unused grid cells.
#'   Defaults to `background`.
#' @param gravity Placement/crop anchor. Common values include `"center"`,
#'   `"north"`, `"south"`, `"east"`, `"west"`, and corner combinations such
#'   as `"northwest"`.
#' @param gap Space between cells in pixels.
#' @param margin Space around the outside of the grid in pixels.
#' @param fill_by Fill the grid by `"row"` or by `"column"`.
#' @param aspect_group Group similarly shaped images in `"row"`s or
#'   `"column"`s, or use `"none"` for a regular grid in the original order.
#'   Grouping sorts images from widest to tallest and gives grouped rows
#'   different heights or grouped columns different widths. This can reduce
#'   padding with `fit = "contain"` and cropping with `fit = "crop"`.
#'   It overrides `fill_by` while active.
#' @param labels Optional character vector with one label per image. `TRUE`
#'   labels images with their filenames; `NULL` or `FALSE` disables labels.
#' @param label_gravity,label_location,label_size,label_color,label_box,label_font
#'   Label styling passed to [magick::image_annotate()]. `label_box = NULL`
#'   draws no box.
#' @param frame Frame/page to use from multi-frame files. A single positive
#'   integer is recycled, or supply one integer per path.
#' @param auto_orient Apply EXIF orientation before layout.
#' @param trim Remove uniform edge regions before measuring images.
#' @param strip Remove profiles and comments from the final image.
#' @param output Optional output path. If omitted, no file is written.
#' @param format Optional output format such as `"png"`, `"jpeg"`, or `"webp"`.
#'   Normally inferred from `output`.
#' @param quality Optional encoder quality (typically 1--100).
#' @param density Optional output density string, for example `"300x300"`.
#' @param max_output_pixels Maximum number of pixels allowed in the complete
#'   output canvas. The default is 50 million. Use `Inf` to disable this limit.
#' @param max_output_dimension Maximum allowed output width or height in pixels.
#'   The default is 16,384. Use `Inf` to disable this limit.
#' @param oversize What to do when the planned canvas exceeds either safety
#'   limit: `"shrink"` proportionally reduces cell dimensions and warns, while
#'   `"error"` stops before allocating the canvas. Automatic shrinking is not
#'   allowed for `fit = "contain", resize = "none"`, because it would violate
#'   the request to preserve every input pixel.
#'
#' @return A `magick-image` object, invisibly if it was also written to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' # Default: two columns, no crop, no upscaling, padded with white.
#' collage <- stitch_images(list.files("photos", full.names = TRUE))
#'
#' # Uniform 4:3 thumbnails, filling each cell by cropping.
#' stitch_images(
#'   c("a.jpg", "b.jpg", "c.jpg", "d.jpg"),
#'   nrow = 2, ncol = 2, fit = "crop", resize = "both",
#'   cell_width = 1200, cell_height = 900,
#'   gap = 20, margin = 30, background = "#181818",
#'   output = "contact-sheet.jpg", quality = 92
#' )
#'
#' # Keep every original pixel; smaller images are padded transparently.
#' stitch_images(
#'   c("wide.png", "tall.png"), fit = "contain", resize = "none",
#'   background = "none", output = "combined.png"
#' )
#'
#' # Build a contact sheet using only landscape images.
#' stitch_images(
#'   list.files("photos", full.names = TRUE),
#'   orientation = "landscape", output = "landscapes.jpg"
#' )
#'
#' # Put similar aspect ratios in the same row and reduce letterboxing.
#' stitch_images(
#'   list.files("photos", full.names = TRUE),
#'   fit = "contain", aspect_group = "row", output = "grouped.jpg"
#' )
#' }
stitch_images <- function(
        paths,
        nrow = NULL,
        ncol = NULL,
        fit = c("crop", "contain"),
        orientation = c("all", "landscape", "portrait", "square"),
        resize = c("shrink", "both", "none"),
        cell_width = NULL,
        cell_height = NULL,
        target_aspect = NULL,
        background = "none",
        canvas_background = background,
        gravity = "center",
        gap = 0L,
        margin = 0L,
        fill_by = c("row", "column"),
        aspect_group = c("none", "row", "column"),
        labels = NULL,
        label_gravity = "southwest",
        label_location = "+12+12",
        label_size = 24,
        label_color = "white",
        label_box = "#00000099",
        label_font = NULL,
        frame = 1L,
        auto_orient = TRUE,
        trim = FALSE,
        strip = FALSE,
        output = NULL,
        format = NULL,
        quality = NULL,
        density = NULL,
        max_output_pixels = 5e7,
        max_output_dimension = 16384L,
        oversize = c("shrink", "error")) {

    brathering:::.ensure_package("magick")

    fit <- match.arg(fit)
    orientation <- match.arg(orientation)
    resize <- match.arg(resize)
    fill_by <- match.arg(fill_by)
    aspect_group <- match.arg(aspect_group)
    oversize <- match.arg(oversize)

    scalar_whole <- function(x, name, allow_zero = FALSE) {
        lower <- if (allow_zero) 0 else 1
        if (length(x) != 1L || is.na(x) || !is.numeric(x) || is.complex(x) ||
            !is.finite(x) || x < lower || x != floor(x)) {
            stop(sprintf("`%s` must be one whole number >= %d.", name, lower),
                 call. = FALSE)
        }
        as.integer(x)
    }

    positive_limit <- function(x, name, whole = FALSE) {
        if (length(x) != 1L || is.na(x) || !is.numeric(x) || is.complex(x) || x <= 0 ||
            (!is.finite(x) && !is.infinite(x)) ||
            (whole && is.finite(x) && x != floor(x))) {
            suffix <- if (whole) "a positive whole number or Inf" else "positive or Inf"
            stop(sprintf("`%s` must be %s.", name, suffix), call. = FALSE)
        }
        as.numeric(x)
    }

    if (!is.character(paths) || length(paths) < 1L || anyNA(paths) ||
        any(!nzchar(paths))) {
        stop("`paths` must be a non-empty character vector of file paths.",
             call. = FALSE)
    }
    paths <- path.expand(paths)
    missing_paths <- paths[!file.exists(paths)]
    if (length(missing_paths)) {
        stop("Image file(s) not found: ", paste(missing_paths, collapse = ", "),
             call. = FALSE)
    }

    gap <- scalar_whole(gap, "gap", allow_zero = TRUE)
    margin <- scalar_whole(margin, "margin", allow_zero = TRUE)
    max_output_pixels <- positive_limit(max_output_pixels, "max_output_pixels")
    max_output_dimension <- positive_limit(
        max_output_dimension, "max_output_dimension", whole = TRUE
    )
    if (!is.null(cell_width)) cell_width <- scalar_whole(cell_width, "cell_width")
    if (!is.null(cell_height)) cell_height <- scalar_whole(cell_height, "cell_height")
    if (!is.null(target_aspect) &&
        (length(target_aspect) != 1L || is.na(target_aspect) ||
         !is.numeric(target_aspect) || !is.finite(target_aspect) ||
         target_aspect <= 0)) {
        stop("`target_aspect` must be one finite number greater than zero.",
             call. = FALSE)
    }

    input_count <- length(paths)
    if (is.character(labels) && length(labels) != input_count) {
        stop("Character `labels` must have one value per input path.", call. = FALSE)
    }
    if (length(frame) == 1L) frame <- rep(frame, input_count)
    if (length(frame) != input_count || anyNA(frame) || !is.numeric(frame) ||
        any(!is.finite(frame)) || any(frame < 1) || any(frame != floor(frame))) {
        stop("`frame` must be a positive integer or one per input image.",
             call. = FALSE)
    }
    frame <- as.integer(frame)

    images <- Map(function(path, page) {
        img <- magick::image_read(path)
        if (length(img) < page) {
            stop(sprintf("'%s' has fewer than %d frame(s)/page(s).", path, page),
                 call. = FALSE)
        }
        img <- img[page]
        if (isTRUE(auto_orient)) img <- magick::image_orient(img)
        if (isTRUE(trim)) img <- magick::image_trim(img)
        img
    }, paths, frame)

    info <- lapply(images, magick::image_info)
    widths <- vapply(info, function(x) x$width[[1L]], numeric(1))
    heights <- vapply(info, function(x) x$height[[1L]], numeric(1))

    orientation_class <- ifelse(
        widths > heights,
        "landscape",
        ifelse(heights > widths, "portrait", "square")
    )
    keep <- orientation == "all" | orientation_class == orientation
    if (!any(keep)) {
        stop(sprintf("No %s input images were found.", orientation), call. = FALSE)
    }
    if (is.character(labels)) labels <- labels[keep]
    paths <- paths[keep]
    frame <- frame[keep]
    images <- images[keep]
    info <- info[keep]
    widths <- widths[keep]
    heights <- heights[keep]
    image_count <- length(images)
    input_aspects <- widths / heights

    if (is.null(nrow) && is.null(ncol)) {
        ncol <- min(2L, image_count)
        nrow <- ceiling(image_count / ncol)
    } else if (is.null(nrow)) {
        ncol <- scalar_whole(ncol, "ncol")
        nrow <- ceiling(image_count / ncol)
    } else if (is.null(ncol)) {
        nrow <- scalar_whole(nrow, "nrow")
        ncol <- ceiling(image_count / nrow)
    } else {
        nrow <- scalar_whole(nrow, "nrow")
        ncol <- scalar_whole(ncol, "ncol")
    }
    nrow <- as.integer(nrow)
    ncol <- as.integer(ncol)
    if (nrow * ncol < image_count) {
        stop("The requested grid has fewer cells than selected images.", call. = FALSE)
    }

    if (aspect_group != "none") {
        aspect_order <- order(input_aspects, decreasing = TRUE)
        paths <- paths[aspect_order]
        images <- images[aspect_order]
        widths <- widths[aspect_order]
        heights <- heights[aspect_order]
        input_aspects <- input_aspects[aspect_order]
        if (is.character(labels)) labels <- labels[aspect_order]
    }

    effective_fill <- if (aspect_group == "row") {
        "row"
    } else if (aspect_group == "column") {
        "column"
    } else {
        fill_by
    }
    if (effective_fill == "row") {
        row_indices <- (seq_len(image_count) - 1L) %/% ncol
        col_indices <- (seq_len(image_count) - 1L) %% ncol
    } else {
        row_indices <- (seq_len(image_count) - 1L) %% nrow
        col_indices <- (seq_len(image_count) - 1L) %/% nrow
    }

    if (!is.null(cell_width) && !is.null(cell_height)) {
        aspect <- cell_width / cell_height
        if (!is.null(target_aspect) &&
            abs(log(aspect / target_aspect)) > sqrt(.Machine$double.eps)) {
            warning("Both cell dimensions were supplied; `target_aspect` was ignored.",
                    call. = FALSE)
        }
    } else {
        aspect <- if (is.null(target_aspect)) {
            stats::median(input_aspects)
        } else {
            target_aspect
        }

        if (is.null(cell_width) && is.null(cell_height)) {
            if (fit == "crop") {
                # Largest ratio-conforming rectangle shared by all inputs. This makes
                # the default crop path safe with resize = "shrink" or "none".
                possible_widths <- pmin(widths, heights * aspect)
                cell_width <- floor(min(possible_widths))
                cell_height <- floor(cell_width / aspect)
                cell_width <- floor(cell_height * aspect)
            } else if (is.null(target_aspect)) {
                # No crop and no implicit resolution loss: use the union of dimensions.
                cell_width <- max(widths)
                cell_height <- max(heights)
            } else {
                # Smallest canvas of the requested ratio that contains every input at
                # original resolution.
                cell_height <- ceiling(max(c(heights, widths / aspect)))
                cell_width <- ceiling(cell_height * aspect)
            }
        } else if (is.null(cell_height)) {
            cell_height <- round(cell_width / aspect)
        } else {
            cell_width <- round(cell_height * aspect)
        }
    }

    cell_width <- scalar_whole(cell_width, "calculated cell_width")
    cell_height <- scalar_whole(cell_height, "calculated cell_height")

    canvas_dimensions <- function(width, height) {
        c(
            width = 2 * as.double(margin) + ncol * as.double(width) +
                (ncol - 1) * as.double(gap),
            height = 2 * as.double(margin) + nrow * as.double(height) +
                (nrow - 1) * as.double(gap)
        )
    }
    within_output_limits <- function(dimensions) {
        dimensions[["width"]] <= max_output_dimension &&
            dimensions[["height"]] <= max_output_dimension &&
            dimensions[["width"]] * dimensions[["height"]] <= max_output_pixels
    }
    describe_canvas <- function(dimensions) {
        sprintf(
            "%s x %s pixels (%.1f megapixels)",
            base::format(dimensions[["width"]], scientific = FALSE, trim = TRUE),
            base::format(dimensions[["height"]], scientific = FALSE, trim = TRUE),
            dimensions[["width"]] * dimensions[["height"]] / 1e6
        )
    }

    planned_dimensions <- canvas_dimensions(cell_width, cell_height)
    if (!within_output_limits(planned_dimensions)) {
        limit_description <- sprintf(
            "maximum dimension %s and %.1f megapixels",
            base::format(max_output_dimension, scientific = FALSE, trim = TRUE),
            max_output_pixels / 1e6
        )
        problem <- sprintf(
            "Planned canvas is %s; safeguards allow %s.",
            describe_canvas(planned_dimensions), limit_description
        )

        cannot_shrink <- oversize == "error" ||
            (fit == "contain" && resize == "none")
        if (cannot_shrink) {
            advice <- if (fit == "contain" && resize == "none") {
                paste0(
                    " Automatic shrinking would conflict with `resize = 'none'`. ",
                    "Use smaller cells, `resize = 'shrink'`, or raise the safety limits."
                )
            } else {
                " Use smaller cells, choose `oversize = 'shrink'`, or raise the safety limits."
            }
            stop(problem, advice, call. = FALSE)
        }

        # Find the largest proportional cell scale that satisfies both limits.
        if (!within_output_limits(canvas_dimensions(1, 1))) {
            stop(
                problem,
                " Gaps, margins, or grid dimensions alone exceed the safety limits.",
                call. = FALSE
            )
        }
        original_cell <- c(width = cell_width, height = cell_height)
        lower <- 0
        upper <- 1
        best_cell <- c(width = 1L, height = 1L)
        for (iteration in seq_len(60L)) {
            scale <- (lower + upper) / 2
            candidate <- c(
                width = max(1L, floor(original_cell[["width"]] * scale)),
                height = max(1L, floor(original_cell[["height"]] * scale))
            )
            if (within_output_limits(canvas_dimensions(candidate[["width"]],
                                                       candidate[["height"]]))) {
                lower <- scale
                best_cell <- candidate
            } else {
                upper <- scale
            }
        }
        cell_width <- as.integer(best_cell[["width"]])
        cell_height <- as.integer(best_cell[["height"]])
        planned_dimensions <- canvas_dimensions(cell_width, cell_height)
        warning(
            problem,
            sprintf(
                " Cell size was reduced from %.0f x %.0f to %d x %d; final canvas is %s.",
                original_cell[["width"]], original_cell[["height"]],
                cell_width, cell_height, describe_canvas(planned_dimensions)
            ),
            call. = FALSE
        )
    }

    allocate_tracks <- function(weights, total) {
        track_count <- length(weights)
        total <- as.integer(total)
        if (is.na(total) || total < track_count) {
            stop("Calculated track dimensions are outside the supported integer range.",
                 call. = FALSE)
        }
        remaining <- total - track_count
        shares <- weights / sum(weights) * remaining
        result <- rep(1L, track_count) + floor(shares)
        leftover <- total - sum(result)
        if (leftover > 0) {
            priority <- order(shares - floor(shares), decreasing = TRUE)
            result[priority[seq_len(leftover)]] <-
                result[priority[seq_len(leftover)]] + 1L
        }
        as.integer(result)
    }

    column_widths <- rep(cell_width, ncol)
    row_heights <- rep(cell_height, nrow)
    if (aspect_group == "row") {
        row_aspects <- vapply(seq_len(nrow), function(index) {
            values <- input_aspects[row_indices == index - 1L]
            if (length(values)) stats::median(values) else cell_width / cell_height
        }, numeric(1))
        row_heights <- allocate_tracks(
            1 / row_aspects, as.double(nrow) * cell_height
        )
    } else if (aspect_group == "column") {
        column_aspects <- vapply(seq_len(ncol), function(index) {
            values <- input_aspects[col_indices == index - 1L]
            if (length(values)) stats::median(values) else cell_width / cell_height
        }, numeric(1))
        column_widths <- allocate_tracks(
            column_aspects, as.double(ncol) * cell_width
        )
    }
    target_widths <- column_widths[col_indices + 1L]
    target_heights <- row_heights[row_indices + 1L]

    normalize_one <- function(img, width, height, path,
                              target_width, target_height) {
        if (fit == "crop") {
            if (resize == "none") {
                if (width < target_width || height < target_height) {
                    stop(sprintf(
                        "'%s' is smaller than the crop cell. Use resize = 'both' or smaller cells.",
                        path), call. = FALSE)
                }
                scaled <- img
            } else {
                scale <- max(target_width / width, target_height / height)
                if (resize == "shrink" && scale > 1 + sqrt(.Machine$double.eps)) {
                    stop(sprintf(
                        "'%s' would need upscaling to fill the crop cell. Use resize = 'both' or smaller cells.",
                        path), call. = FALSE)
                }
                if (resize == "shrink") scale <- min(scale, 1)
                if (abs(scale - 1) <= sqrt(.Machine$double.eps)) {
                    scaled <- img
                } else {
                    new_width <- max(target_width, ceiling(width * scale))
                    new_height <- max(target_height, ceiling(height * scale))
                    scaled <- magick::image_resize(
                        img, sprintf("%dx%d!", new_width, new_height), filter = "Lanczos"
                    )
                }
            }
            magick::image_crop(
                scaled,
                geometry = sprintf("%dx%d+0+0", target_width, target_height),
                gravity = gravity,
                repage = TRUE
            )
        } else {
            if (resize == "none") {
                if (width > target_width || height > target_height) {
                    stop(sprintf(
                        "'%s' is larger than the cell. Use resize = 'shrink' or larger cells.",
                        path), call. = FALSE)
                }
                scaled <- img
            } else {
                scale <- min(target_width / width, target_height / height)
                if (resize == "shrink") scale <- min(scale, 1)
                if (abs(scale - 1) <= sqrt(.Machine$double.eps)) {
                    scaled <- img
                } else {
                    new_width <- max(1L, round(width * scale))
                    new_height <- max(1L, round(height * scale))
                    scaled <- magick::image_resize(
                        img, sprintf("%dx%d!", new_width, new_height), filter = "Lanczos"
                    )
                }
            }
            magick::image_extent(
                scaled,
                geometry = sprintf("%dx%d", target_width, target_height),
                gravity = gravity,
                color = background
            )
        }
    }

    cells <- Map(
        normalize_one,
        images, widths, heights, paths, target_widths, target_heights
    )

    if (isTRUE(labels)) {
        labels <- basename(paths)
    } else if (isFALSE(labels)) {
        labels <- NULL
    }
    if (!is.null(labels)) {
        if (!is.character(labels) || length(labels) != image_count || anyNA(labels)) {
            stop("`labels` must be TRUE or one character label per image.", call. = FALSE)
        }
        cells <- Map(function(cell, label) {
            annotate_args <- list(
                image = cell,
                text = label,
                gravity = label_gravity,
                location = label_location,
                size = label_size,
                color = label_color
            )
            if (!is.null(label_box)) annotate_args$boxcolor <- label_box
            if (!is.null(label_font)) annotate_args$font <- label_font
            do.call(magick::image_annotate, annotate_args)
        }, cells, labels)
    }

    canvas_width <- as.integer(planned_dimensions[["width"]])
    canvas_height <- as.integer(planned_dimensions[["height"]])
    result <- magick::image_blank(
        width = canvas_width,
        height = canvas_height,
        color = canvas_background
    )

    for (i in seq_len(image_count)) {
        row_index <- row_indices[[i]]
        col_index <- col_indices[[i]]
        x <- as.integer(
            margin + sum(column_widths[seq_len(col_index)]) + col_index * gap
        )
        y <- as.integer(
            margin + sum(row_heights[seq_len(row_index)]) + row_index * gap
        )
        result <- magick::image_composite(
            result,
            cells[[i]],
            operator = "over",
            offset = sprintf("+%d+%d", x, y),
            gravity = "northwest"
        )
    }

    if (isTRUE(strip)) result <- magick::image_strip(result)

    if (!is.null(output)) {
        if (!is.character(output) || length(output) != 1L || is.na(output) ||
            !nzchar(output)) {
            stop("`output` must be one non-empty file path.", call. = FALSE)
        }
        output <- path.expand(output)
        output_dir <- dirname(output)
        if (!dir.exists(output_dir)) {
            stop("Output directory does not exist: ", output_dir, call. = FALSE)
        }
        write_args <- list(image = result, path = output)
        if (!is.null(format)) write_args$format <- format
        if (!is.null(quality)) write_args$quality <- quality
        if (!is.null(density)) write_args$density <- density
        do.call(magick::image_write, write_args)
        return(invisible(result))
    }

    result
}


