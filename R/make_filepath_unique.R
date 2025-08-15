#' Compare a file path against disk and make it unique by trailing number
#'
#' The resulting path can be used to generate a file on disk without overwriting
#' an existing file. The trailing integer is incremented from 1 to Inf
#'
#' @param path path/on/disk/text.txt
#' @param sep separator before trailing integer
#'
#' @returns new path
#' @export
#'
#' @examples
#' make_filepath_unique(list.files(getwd(), full.names = T)[1])
make_filepath_unique <- function(path, sep = "_") {
    if (!file.exists(path)) {
        return(path)
    }
    filewoext <- tools::file_path_sans_ext(basename(path), compression = T)
    ext <- sub(paste0("^", filewoext), "", basename(path))
    incr <- 1
    path <- file.path(dirname(path), paste0(filewoext, sep, incr, ext))
    while (file.exists(path)) {
        incr <- incr + 1
        path <- file.path(dirname(path), paste0(filewoext, sep, incr, ext))
    }
    return(path)
}
