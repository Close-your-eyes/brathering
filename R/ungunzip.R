#' Unzip gunzip archive on disk
#'
#' Simply calls gunzip system command. May only work on unix system.
#' Unpacked archives appear in same folder by default.
#'
#' @param file .gz file
#' @param out_path optional output path
#'
#' @return nothing, unpacked files on disk
#' @export
#'
#' @examples
#' \dontrun{
#' ungunzip(file/to/gunzip/file.gz)
#' }
ungunzip <- function(file,
                     out_path = NULL) {

    if (!file.exists(file)) {
        stop("file not found.")
    }
    if (is.null(out_path)) {
        out <- gsub("\\.gz$", "", file)
        cmd <- paste0("gunzip -k ", file)
    } else {
        out <- file.path(out_path, gsub("\\.gz$", "", basename(file)))
        cmd <- paste0("gunzip -k -c ", file, " > ", out)
    }

    if (!dir.exists(out_path)) {
        dir.create(out_path, showWarnings = F, recursive = T)
    }

    # -k flag tells gzip to keep the original .gz file
    system(cmd, intern = T)

    return(out)
}
