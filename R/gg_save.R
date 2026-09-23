#' ggsave with defaults
#'
#' filename on disk derives from plot var name and format (device) by default.
#' width derive from height and aspect ratio by default - easy scaling.
#' overwrite management.
#' path derives from global var by default - reduced redundancy.
#'
#' set im_path global option: options(im_path = file.path(...))
#'
#' @param plot A plot, or a list of plots. Named lists use their names for the
#'   default filenames; unnamed elements use the list expression plus an index.
#' @param filename Filename, or one filename per plot when `plot` is a list.
#' @param device device
#' @param path filepath
#' @param width w
#' @param height h
#' @param ... other arg to ggplot2::ggsave
#' @param aspect_ratio set height relative to width: height = width/aspect_ratio
#' @param overwrite overwrite existing file on disk?
#' @param append_filename when overwrite = F, change filename to save plot?
#' @param w width shorthand
#' @param h height shorthand
#' @param ar aspect_ratio shorthand
#'
#' @returns Invisibly, a character vector containing the paths that were saved.
#' @export
#'
#' @examples
#' tt <- data.frame(x = rnorm(20), y = rnorm(20))
#' options(im_path = "~/Desktop")
#'
#' pp <- ggplot2::ggplot(tt, ggplot2::aes(x,y)) +
#'     ggplot2::geom_point()
#'
#' gg_save(pp)
#'
#' gg_save(list(points = pp, points_again = pp))
gg_save <- function(plot = ggplot2::last_plot(),
                    filename = paste0(deparse(substitute(plot)), ".", deparse(substitute(device))),
                    device = grDevices::png, #ragg::agg_png,
                    path = getOption("im_path", default = getwd()),
                    width = 8,
                    w = NULL,
                    height = NULL,
                    h = NULL,
                    aspect_ratio = 4/3,
                    ar = NULL,
                    overwrite = TRUE,
                    append_filename = TRUE,
                    ...) {
    brathering:::.ensure_packages(c("ggplot2", "ragg"))

    plot_expression <- paste(deparse(substitute(plot)), collapse = "")
    device_expression <- paste(deparse(substitute(device)), collapse = "")
    plot_is_list <- is.list(plot) && !inherits(plot, c("ggplot", "grob"))
    plots <- if (plot_is_list) plot else list(plot)

    if (length(plots) == 0L) {
        return(invisible(character()))
    }

    normalise_extension <- function(x) {
        x <- gsub("cairo_pdf$", "pdf", x)
        x <- gsub("ragg::agg_png$", "png", x)
        gsub("grDevices::png$", "png", x)
    }

    if (missing(filename)) {
        extension <- normalise_extension(device_expression)

        if (plot_is_list) {
            plot_names <- names(plots)
            if (is.null(plot_names)) {
                plot_names <- rep("", length(plots))
            }
            unnamed <- is.na(plot_names) | !nzchar(plot_names)
            plot_names[unnamed] <- paste0(plot_expression, "_", which(unnamed))
            filename <- paste0(plot_names, ".", extension)
        } else {
            filename <- paste0(plot_expression, ".", extension)
        }
    }

    if (!is.character(filename) ||
        length(filename) != length(plots) ||
        anyNA(filename) ||
        any(!nzchar(filename))) {
        stop("`filename` must contain exactly one non-empty filename per plot.", call. = FALSE)
    }

    filename <- normalise_extension(filename)
    if (anyDuplicated(file.path(path, filename))) {
        stop("Each plot must have a unique output filename.", call. = FALSE)
    }

    if (!is.null(h)) {
        height <- h
    }
    if (!is.null(w)) {
        width <- w
    }
    if (!is.null(ar)) {
        aspect_ratio <- ar
    }


    if (!is.null(height) && !is.null(width)) {
        aspect_ratio <- width/height
    }

    if (is.null(height) && is.null(width)) {
        width <- 8
    }

    if (is.null(height)) {
        height <- width/aspect_ratio
    }

    if (is.null(width)) {
        width <- height*aspect_ratio
    }
    message("w: ", round(width,1), " h: ", round(height,1), " ar: ", round(aspect_ratio,1))


    save_plot <- function(plot, filename) {
        target <- file.path(path, filename)

        if (file.exists(target)) {
            if (overwrite) {
                # nothing; append_filename is irrelevant
            } else if (append_filename) {
                target <- make_filepath_unique(target)
                filename <- basename(target)
            } else {
                message("gg_save: cannot overwrite ", target)
                return(NULL)
            }
        }

        ggplot2::ggsave(
          filename = filename,
          plot = plot,
          device = device,
          path = path,
          width = width,
          height = height,
          create.dir = TRUE,
          ...
        )

        message(file.path(path, filename))
        file.path(path, filename)
    }

    saved_files <- unlist(Map(save_plot, plots, filename), use.names = FALSE)
    invisible(saved_files)
}
