#' Make file paths portable
#'
#' This a wrapper around make_portable_filename. Dirname(x) and basename(x)
#' are treated separately. Dirnames may be subject to url decoding only.
#'
#' @param x vector of file paths
#' @param allow values to allow besides alnum
#' @param repl replacement for all not permitted values
#' @param urldecode also decode URL encoding
#'
#' @return vector of modified paths
#' @export
#'
#' @examples
make_portable_filepath <- function(x,
                                   allow = c("-", "."),
                                   repl = "_",
                                   urldecode = T) {
    file_names <- basename(x)
    file_paths <- dirname(x)
    # file path: only remove urldecoding
    if (urldecode) {
        file_paths <- utils::URLdecode(file_paths)
    }
    file_names <- make_portable_filename(file_names)
    return(file.path(file_paths, file_names))
}

#' Make file names portable
#'
#' That means to make them compatible with many systems by replacing
#' special non-numeric and non-character values.
#'
#' @param x vector of file paths
#' @param allow values to allow besides alnum
#' @param repl replacement for all not permitted values
#' @param urldecode also decode URL encoding
#'
#' @return
#' @export
#'
#' @examples
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
        x <- utils::URLdecode(x)
    }

    file_names <- tools::file_path_sans_ext(x, compression = T)
    file_exts <- stringi::stri_replace_all_fixed(x, file_names, "")
    file_exts <- gsub("[^[:alnum:].]", "", file_exts) # allow all numeric and char and dot

    file_names <- fs::path_sanitize(file_names, replacement = repl)
    # turn everything into plain ASCII
    file_names <- stringi::stri_trans_general(file_names, "Latin-ASCII")
    # replace runs of pattern with one repl
    file_names <- gsub(pattern, repl, file_names)
    # collapse multiple repls and trim
    file_names <- gsub(paste0(repl, "{2,}"), repl, file_names)
    file_names <- sub(paste0("^", repl, "+|", repl, "+$"), "", file_names)
    return(paste0(file_names, file_exts))
}
