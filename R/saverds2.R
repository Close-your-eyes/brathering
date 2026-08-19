#' Save an R Object as a Gzip-Compressed RDS File
#'
#' Serializes an R object using RDS format version 3 and gzip compression,
#' then reports the output file path.
#'
#' @param x Any R object to serialize.
#' @param file A character string specifying the output file path.
#' @param compression An integer from `0` to `9` controlling gzip compression.
#'   Higher values generally produce smaller files but take longer. Defaults
#'   to `1`.
#'
#' @return `x`, invisibly.
#' @export
#'
#' @examples
#' path <- tempfile(fileext = ".rds")
#' saverds2(mtcars, path)
#' restored <- readr::read_rds(path)
#' unlink(path)
saverds2 <- function(x, file, compression = 1) {
    readr::write_rds(
        x = x,
        file = file,
        compress = "gz",
        version = 3,
        compression = compression
    )
    message(file)
    invisible(x)
}
