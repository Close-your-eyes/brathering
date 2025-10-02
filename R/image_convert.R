#' Convert image between formats
#'
#' Converted image is saved in same location as the original (default),
#' under same name but with new file extension.
#'
#' @param path file path on disk
#' @param format target format, see magick::image_write
#' @param ... arguments to magick::image_write
#' @param path_save save path of converted image, if NuLL saved to same
#' folder as the original
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' image_convert(path/to/image.heic, format = "png")
#' }
image_convert <- function(path, format = "jpeg", path_save = NULL, ...) {

    if (!requireNamespace("magick", quietly = T)) {
        utils::install.packages("magick")
    }

    # pathnew <- file.path(dirname(path), paste0(strsplit2(basename(path), pattern = "\\.", inds = "last")[[1]][1], ".", format))
    if (is.null(path_save)) {
        pathnew <- tools::file_path_sans_ext(path, compression = T)
    } else {
        pathnew <- file.path(path_save, tools::file_path_sans_ext(basename(path), compression = T))
    }
    pathnew <- paste0(pathnew, ".", gsub("jpeg", "jpg", format))
    magick::image_write(magick::image_read(path), path = pathnew, format = format, ...)
}
