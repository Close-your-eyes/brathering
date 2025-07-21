#' Unzip gunzip archive on disk
#'
#' Simply calls gunzip system command. May only work on unix system.
#' Unpacked archives appear in same folder.
#'
#' @param path path to .gz file
#'
#' @return nothing, unpacked files on disk
#' @export
#'
#' @examples
#' \dontrun{
#' ungunzip(path/to/gunzip/file.gz)
#' }
ungunzip <- function(path) {
    cmd <- paste0("gunzip -k ", path)
    # -k flag tells gzip to keep the original .gz file
    system(cmd, intern = T)
}
