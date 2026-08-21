#' Format byte counts as human-readable file sizes
#'
#' Converts numeric byte counts to human-readable IEC file sizes using binary
#' units such as KiB, MiB, and GiB.
#'
#' @param x A numeric vector containing file sizes in bytes.
#'
#' @return A character vector of the same length as `x`, containing formatted
#'   file sizes with automatically selected IEC units.
#'
#' @export
#'
#' @examples
#' format_bytes(1024)
#' format_bytes(c(1024, 1024^2, 1024^3))
#'
#' files <- tempfile()
#' writeLines("example", files)
#' format_bytes(file.info(files)$size)
format_bytes <- function(x) {
    vapply(
        x,
        function(bytes) {
            format(
                structure(bytes, class = "object_size"),
                units = "auto",
                standard = "IEC",
                digits = 2
            )
        },
        character(1)
    )
}
