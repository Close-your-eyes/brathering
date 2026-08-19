#' Add a suffix to file names
#'
#' Inserts a suffix immediately before the final file extension while
#' preserving directory paths. Files without an extension remain free of a
#' trailing period.
#'
#' @param x A character vector of file paths.
#' @param suffix A single character string to append to each file name.
#' @param sep A single character string inserted between the original file
#'   name and `suffix`.
#'
#' @return A character vector with the same length as `x`.
#' @export
#'
#' @examples
#' suffix_add("report.csv", "final", "_")
#' # "report_final.csv"
#'
#' suffix_add(c("data/input.csv", "README"), "backup", "-")
#' # "data/input-backup.csv" "README-backup"
#'
#' suffix_add(NA_character_, "final", "_")
#' # NA
suffix_add <- function(x, suffix = "", sep = "") {
    stopifnot(
        is.character(x),
        is.character(suffix), length(suffix) == 1L, !is.na(suffix),
        is.character(sep), length(sep) == 1L, !is.na(sep)
    )

    ext <- tools::file_ext(x)
    stem <- tools::file_path_sans_ext(x)

    addition <- if (nzchar(suffix)) paste0(sep, suffix) else ""
    extension <- ifelse(nzchar(ext), paste0(".", ext), "")

    result <- paste0(stem, addition, extension)
    result[is.na(x)] <- NA_character_
    return(result)
}
