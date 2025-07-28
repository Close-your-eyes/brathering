#' Make file paths portable
#'
#' This a wrapper around make_portable_filename. dirname(x) and basename(x)
#' are treated separately. dirnames may be subject to url decoding only.
#'
#' @param x character vector of file paths
#' @param allow values to allow besides alnum
#' @param repl replacement for all not permitted values
#' @param urldecode also decode URL encoding
#'
#' @return character vector of modified paths
#' @export
#'
#' @examples
#' make_portable_filepath(x = file.path("my", "file", "path",
#'                                      c("näme1",
#'                                        "næme2",
#'                                        "name~3!",
#'                                        "name!!4",
#'                                        "...name§5___",
#'                                        "name/\\6",
#'                                        "name_βγ7",
#'                                        "name{(8)}",
#'                                        "name_9%")))
#' make_portable_filepath("path%20withspace/file!!1")
#' make_portable_filepath("path%20withspace/file!!1", urldecode = F)
make_portable_filepath <- function(x,
                                   allow = c("-", "."),
                                   repl = "_",
                                   urldecode = T) {
    file_names <- basename(x)
    file_paths <- dirname(x)
    # file path: only remove url decoding
    if (urldecode) {
        file_paths <- suppressWarnings(utils::URLdecode(file_paths))
    }
    file_names <- make_portable_filename(file_names)
    return(file.path(file_paths, file_names))
}

#' Make file names portable
#'
#' That means to make them compatible with many systems by replacing
#' special non-numeric and non-character values.
#'
#' @param x character vector of file names
#' @param allow values to allow besides alnum
#' @param repl replacement for all not permitted values
#' @param urldecode also decode URL encoding - the will replace %num
#' with respective chars
#'
#' @return character vector of modified names
#' @export
#'
#' @examples
#' make_portable_filename(x = c(
#'   "näme1",
#'   "næme2",
#'   "name~3!",
#'   "name!!4",
#'   "...name§5___",
#'   "name/\\6",
#'   "name_βγ7",
#'   "name{(8)}",
#'   "name_9%"))
#' make_portable_filename(x = c("name%20_%9"))
#' make_portable_filename(x = c("name%20_%9"), urldecode = F)
make_portable_filename <- function(x,
                                   allow = c("-", "."),
                                   repl = "_",
                                   urldecode = T) {
    pattern <- make_regex_pattern(x = allow)
    x <- make_portable(
        x,
        pattern = pattern,
        repl = repl,
        urldecode = urldecode
    )
    return(x)
}


make_regex_pattern <- function(x = c(".", "-")) {
    # de‑duplicate and coerce to character
    x <- unique(as.character(x))
    # if hyphen is one of them, pull it out to the end
    if ("-" %in% x) {
        x <- c(setdiff(x, "-"), "-")
    }
    x_collapsed <- paste0(x, collapse = "")
    pattern <- paste0("[^[:alnum:]", x_collapsed, "]")
    return(pattern)
}


make_portable <- function(x,
                          pattern = "[^[:alnum:]]",
                          repl = "_",
                          urldecode = T) {

    if (urldecode) {
        x <- suppressWarnings(utils::URLdecode(x))
    }

    file_names <- tools::file_path_sans_ext(x, compression = T)
    file_exts <- stringi::stri_replace_all_fixed(x, file_names, "")
    file_exts <- gsub("[^[:alnum:].]", "", file_exts) # allow all numeric and char and dot

    file_names <- fs::path_sanitize(file_names, replacement = repl)
    # turn everything into plain ASCII
    #file_names <- stringi::stri_trans_general(file_names, "Greek-Latin")
    file_names <- stringi::stri_trans_general(file_names, "Any-Latin")
    file_names <- stringi::stri_trans_general(file_names, "Latin-ASCII")
    # replace runs of pattern with one repl
    file_names <- gsub(pattern, repl, file_names)
    # replace stretches of punctuation
    file_names <- gsub("[[:punct:]]{2,}", repl, file_names)
    # collapse multiple repls and trim
    #file_names <- gsub(paste0(repl, "{2,}"), repl, file_names)
    file_names <- gsub(paste0("^[[:punct:]]+|[[:punct:]]+$"), "", file_names)
    return(paste0(file_names, file_exts))
}
