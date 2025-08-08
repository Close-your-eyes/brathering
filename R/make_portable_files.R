#' Make file paths portable
#'
#' This a wrapper around make_portable_filename. dirname(x) and basename(x)
#' are treated separately.
#'
#' @param x character vector of file paths
#' @param allow values to allow besides alnum
#' @param repl replacement for all not permitted values
#' @param urldecode also decode URL encoding
#' @param make_unique make filnames unique?
#' @param repl_empty replacement for elements that become empty strings during
#' the procedure
#' @param try_deunicode try the deunicode crate from Rust which handles
#' more characters
#' @param make_path_portable if FALSE only basename(x) is treated; if TRUE
#' also dirname(x) is treated
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
                                   urldecode = T,
                                   make_unique = F,
                                   repl_empty = "x",
                                   try_deunicode = F,
                                   make_path_portable = F) {

    file_names <- basename(x)
    file_paths <- dirname(x)
    file_names <- make_portable_filename(x = file_names,
                                         allow = allow,
                                         repl = repl,
                                         urldecode = urldecode,
                                         make_unique = make_unique,
                                         repl_empty = repl_empty,
                                         try_deunicode = try_deunicode)

    if (make_path_portable) {
        # treat every folder separately
        file_paths <- purrr::map_chr(file_paths, function(y) {
            y <- gsub("^'", "", y)
            y <- gsub("'$", "", y)
            add_slash <- F
            if (grepl("^/", y)) {
                y <- sub("^/", "", y)
                add_slash <- T
            }
            z <- strsplit(y, "/", fixed = T)[[1]]
            z <- make_portable_filename(x = z,
                                        allow = allow,
                                        repl = repl,
                                        urldecode = urldecode,
                                        make_unique = F,
                                        repl_empty = repl_empty,
                                        try_deunicode = try_deunicode)
            z <- paste(z, collapse = "/")
            if (add_slash) {
                z <- paste0("/", z)
            }
            return(z)
        })
    }

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
#' @param make_unique make filnames unique?
#' @param repl_empty replacement for elements that become empty strings during
#' the procedure
#' @param try_deunicode try the deunicode crate from Rust which handles
#' more characters
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
#'   "name_9%",
#'   "",
#'   "",
#'   NA))
#' make_portable_filename(x = c("name%20_%9"))
#' make_portable_filename(x = c("name%20_%9"), urldecode = F)
#' # not everything works
#' make_portable_filename(x = c("北亰","げんまい茶","🦄☣","…","ᔕᓇᓇ"))
#' # but with deunicode it does
#' make_portable_filename(x = c("北亰","げんまい茶","🦄☣","…","ᔕᓇᓇ"), try_deunicode = T)
make_portable_filename <- function(x,
                                   allow = c("-", "."),
                                   repl = "_",
                                   urldecode = T,
                                   make_unique = F,
                                   repl_empty = "x",
                                   try_deunicode = F) {

    pattern <- make_regex_pattern(x = allow)
    x <- make_portable(
        x = x,
        pattern = pattern,
        repl = repl,
        urldecode = urldecode,
        make_unique = make_unique,
        repl_empty = repl_empty,
        try_deunicode = try_deunicode
    )
    return(x)
}


make_regex_pattern <- function(x = c(".", "-")) {
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
                          urldecode = T,
                          make_unique = F,
                          repl_empty = "x",
                          try_deunicode = F) {

    if (try_deunicode) {
        check_and_prompt_rust()
        rust_path <- Sys.which("rustc")
        if (nzchar(rust_path)) {
            if (!requireNamespace("deunicode", quietly = T)) {
                devtools::install_github("Close-your-eyes/deunicode")
            }
        } else {
            message("deunicode not available.")
            try_deunicode <- F
        }
    }

    if (urldecode) {
        x <- suppressWarnings(utils::URLdecode(x))
    }


    # rm shQuote which would disturb file_path_sans_ext detection
    x <- gsub("'$", "", x)

    file_names <- tools::file_path_sans_ext(x, compression = T)
    file_names[which(file_names == "")] <- repl_empty

    file_exts <- stringi::stri_replace_all_fixed(x, file_names, "")
    file_exts <- gsub("[^[:alnum:].]", "", file_exts) # allow all numeric and char and dot

    file_names <- fs::path_sanitize(file_names, replacement = repl)
    # turn everything into plain ASCII
    #file_names <- stringi::stri_trans_general(file_names, "Greek-Latin")
    file_names <- stringi::stri_trans_general(file_names, "Any-Latin")
    file_names <- stringi::stri_trans_general(file_names, "Latin-ASCII")
    if (try_deunicode) {
        file_names <- deunicode:::deunicode(file_names)
    }

    # replace runs of pattern with one repl
    file_names <- gsub(pattern, repl, file_names)
    # replace stretches of punctuation
    file_names <- gsub("[[:punct:]]{2,}", repl, file_names)
    # collapse multiple repls and trim
    #file_names <- gsub(paste0(repl, "{2,}"), repl, file_names)
    file_names <- gsub(paste0("^[[:punct:]]+|[[:punct:]]+$"), "", file_names)

    if (make_unique) {
        file_names <- make.unique(file_names)
        file_names <- gsub(pattern, repl, file_names)
    }

    return(paste0(file_names, file_exts))
}


check_and_prompt_rust <- function() {
    rust_path <- Sys.which("rustc")

    if (!nzchar(rust_path)) {
        message("Rust compiler not found.")
        ans <- tolower(readline("Would you like to install Rust now? [y/N]: "))
        if (ans %in% c("y", "yes")) {
            install_rust(os = Sys.info()[["sysname"]])
        } else {
            message("Okay, Rust will not be installed.")
        }
    }
}

install_rust <- function(os) {
    message("Installing Rust for ", os, "…")

    if (os %in% c("Darwin", "Linux")) {
        # macOS and Linux: use rustup installer script
        cmd <- "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y"
        system(cmd)
    } else if (os == "Windows") {
        # Windows: download rustup-init.exe and run it
        tmp <- tempfile(fileext = ".exe")
        download.file("https://win.rustup.rs/", tmp, mode = "wb")
        # -y = default installation options
        shell(sprintf('"%s" -y', tmp), wait = TRUE)
    } else {
        stop("Unsupported OS: ", os)
    }

    message("Installation complete. You may need to restart your R session.")
}


