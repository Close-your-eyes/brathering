#' Interactively select polygons to keep in color
#'
#' Click vertices in order around each polygon, then use the graphics device's
#' finish action (for example, Esc on macOS). Finish again without clicking
#' to end selection. Requires an interactive graphics device.
#'
#' @param image A path to an image or a single-frame magick image.
#'
#' @return A magick image with original colors inside the selected polygons
#'   and transparency outside. The original dimensions are preserved.
#'   If no polygons are selected, the result is fully transparent.
#'
#' @export
select_color_part <- function(image) {
    img <- if (base::is.character(image)) {
        magick::image_read(image)
    } else {
        image
    }

    if (!base::inherits(img, "magick-image") || base::length(img) != 1L) {
        base::stop("Use a single-frame magick image or an image path.")
    }

    info <- magick::image_info(img)
    w <- info$width
    h <- info$height
    polygons <- base::list()

    graphics::plot(grDevices::as.raster(img), interpolate = FALSE)

    repeat {
        base::message(
            "Click polygon vertices, then Esc/Finish. ",
            "Finish without clicks to stop."
        )

        p <- graphics::locator(type = "o", col = "red", pch = 16)

        if (base::is.null(p) || base::length(p$x) == 0L) break

        if (base::length(p$x) < 3L) {
            base::message("Polygon discarded: select at least three vertices.")
            next
        }

        graphics::polygon(p$x, p$y, border = "red", lwd = 2)

        # Convert plot coordinates to image coordinates from the top-left.
        polygons[[base::length(polygons) + 1L]] <- base::list(
            x = base::pmax(0, base::pmin(w, p$x)),
            y = base::pmax(0, base::pmin(h, h - p$y))
        )
    }

    mask <- magick::image_draw(
        magick::image_blank(w, h, color = "none")
    )

    base::tryCatch(
        {
            for (p in polygons) {
                graphics::polygon(p$x, p$y, col = "white", border = NA)
            }
        },
        finally = grDevices::dev.off()
    )

    # Enable transparency before applying the mask.
    img <- magick::image_convert(img, format = "png", matte = TRUE)
    mask <- magick::image_convert(mask, format = "png", matte = TRUE)

    magick::image_composite(img, mask, operator = "DstIn")
}


#' Composite selected color pieces onto a background and save
#'
#' Pieces are placed at the top-left of the background using their original
#' canvas dimensions. Later pieces appear over earlier pieces.
#'
#' @param background A path to an image or a single-frame magick image.
#' @param pieces A list of single-frame magick images, typically returned by
#'   \code{select_color_part()}. A single magick image is also accepted.
#'   Every piece must have the same dimensions as the background.
#' @param file Output file path. The extension determines the image format.
#'   Use PNG when transparency must be preserved.
#' @param grayscale Logical. Convert the background to grayscale before
#'   compositing? Defaults to \code{TRUE}.
#'
#' @return The composited magick image, invisibly. Also saves it to \code{file}.
#'
#' @examples
#' \dontrun{
#' original <- magick::image_read("input.jpg")
#' part1 <- select_color_part(original)
#' part2 <- select_color_part(original)
#'
#' result <- composite_and_save(
#'   background = original,
#'   pieces = list(part1, part2),
#'   file = "selective-color.png"
#' )
#'
#' graphics::plot(grDevices::as.raster(result))
#' }
#'
#' @export
composite_and_save <- function(background,
                               pieces,
                               file,
                               grayscale = TRUE) {
    result <- if (base::is.character(background)) {
        magick::image_read(background)
    } else {
        background
    }

    if (!base::inherits(result, "magick-image") ||
        base::length(result) != 1L) {
        base::stop("Use a single-frame magick image or a background image path.")
    }

    if (base::inherits(pieces, "magick-image")) {
        pieces <- base::list(pieces)
    }

    if (!base::is.list(pieces)) {
        base::stop("Supply pieces as a list of magick images.")
    }

    if (grayscale) {
        result <- magick::image_convert(result, colorspace = "gray")
    }

    result <- magick::image_convert(
        result,
        colorspace = "sRGB",
        matte = TRUE
    )
    size <- magick::image_info(result)

    for (piece in pieces) {
        if (!base::inherits(piece, "magick-image") ||
            base::length(piece) != 1L) {
            base::stop("Each piece must be a single-frame magick image.")
        }

        piece_size <- magick::image_info(piece)

        if (piece_size$width != size$width ||
            piece_size$height != size$height) {
            base::stop(
                "Each piece must have the same dimensions as the background."
            )
        }

        result <- magick::image_composite(
            result,
            piece,
            operator = "Over",
            offset = "+0+0",
            gravity = "northwest"
        )
    }

    magick::image_write(result, path = file)
    base::invisible(result)
}
