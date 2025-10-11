#' Unzip gunzip archive on disk
#'
#' Simply calls gunzip system command. May only work on unix system.
#' Unpacked archives appear in same folder by default.
#'
#' @param file .gz file
#' @param out_dir optional output directory (folder)
#' @param out_file optional output file name
#'
#' @return output file path
#' @export
#'
#' @examples
#' \dontrun{
#' ungunzip(file/to/gunzip/file.gz)
#' }
ungunzip <- function(file,
                     out_file = NULL,
                     out_dir = NULL) {

    if (!file.exists(file)) {
        stop("file not found.")
    }
    if (is.null(out_dir)) {
        out_dir <- dirname(file)
    }
    if (fs::is_file(out_dir)) {
        message("out_dir is file. using its dirname.")
        out_dir <- dirname(out_dir)
    }
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, showWarnings = F, recursive = T)
    }
    if (is.null(out_file)) {
        out_file <- gsub("\\.gz$", "", basename(file))
    }
    if (fs::is_dir(out_file)) {
        message("out_file is dir using its basename.")
        out_file <- basename(out_file)
    }
    out <- file.path(out_dir, out_file)


    cmd <- paste0("gunzip -k -c ", file, " > ", out)

    # -k: keep original file
    # -c: write to stdout, keep original files
    # if (is.null(out_dir)) {
    #     out <- gsub("\\.gz$", "", file)
    #     cmd <- paste0("gunzip -k ", file)
    # } else {
    #     out <- file.path(out_dir, gsub("\\.gz$", "", basename(file)))
    #     cmd <- paste0("gunzip -k -c ", file, " > ", out)
    # }

    system(cmd, intern = T)

    return(out)
}
