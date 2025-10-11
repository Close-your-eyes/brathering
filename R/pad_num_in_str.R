#' Pad one or more numbers in strings to equal length(s)
#'
#' Intended for subsequent sorting.
#'
#' @param x character vector
#' @param pad pad to stringr::str_pad
#' @param len how to pad numbers: overall_max = all to the longest number in x,
#' indwise_max: index wise longest number, one fixed number to pad all numbers to,
#' a vector of lengths for each common index of numbers across x
#' @param len_indwise_remain how to pad non-common (remaining) numbers in y
#'
#' @returns modified x
#' @export
#'
#' @examples
#' x <- c("w344r5t6", "w3r1t69", "w3r3t7q98")
#' pad_num_in_str(x)
#' pad_num_in_str(x, pad = "1")
#' pad_num_in_str(x, len = 2)
#' pad_num_in_str(x, len = 5)
#' pad_num_in_str(x, len = "indwise_max")
#' pad_num_in_str(x, len = "indwise_max", len_indwise_remain = 5)
pad_num_in_str <- function(x,
                           pad = "0",
                           # indwise_max or numeric or overall_max
                           len = "overall_max",
                           len_indwise_remain = 1) {

    # complicated to pad arbitrary number of numerics in strings
    # one intention: proper ordering

    parts <- strsplit_at_num(x)
    ## different n of numerics across x?
    suppressWarnings(num_positions <- purrr::map(parts, ~which(!is.na(as.numeric(.x)))))
    num_parts <- purrr::map2(parts, num_positions, ~.x[.y])

    # lens: all the same, or each ind to max, or all the same fixed
    if (!is.null(len) && ((is.numeric(len) && length(len) == 1) || len == "overall_max")) {
        len2 <- ifelse(len == "overall_max", max(nchar(unlist(num_parts))), len)
        num_parts_pad <- purrr::map(num_parts,
                                    ~stringr::str_pad(
                                        .x,
                                        pad = pad,
                                        width = len2))
    } else if (len == "indwise_max" || (is.numeric(len) && length(len) > 1)) {
        # this is a bit complicated
        min_n <- min(lengths(num_parts))

        if (len == "indwise_max") {
            num_parts_pad <- purrr::map(1:min_n, function(y) {
                z <- sapply(num_parts, "[", y)
                len2 <- max(nchar(z))
                return(stringr::str_pad(z, pad = pad, width = len2))
            })
        } else if (is.numeric(len)) {
            if (length(len) != length(min_n)) {
                stop("len must be of length min_n.")
            }
            num_parts_pad <- purrr::map2(1:min_n, len, function(y, len2) {
                z <- sapply(num_parts, "[", y)
                return(stringr::str_pad(z, pad = pad, width = len2))
            })
        } else {
            stop("len must be indwise_max, overall_max, a numeric or a vector of numerics.")
        }
        num_parts_pad <- list_transpose(num_parts_pad)
        # handle numerics at larger n as min_n
        # here len_indwise_remain is used
        num_parts_pad <- purrr::map2(num_parts, num_parts_pad, function(x,y) {
            if (length(y) < length(x)) {
                y <- c(y, stringr::str_pad(
                    x[(length(y)+1):length(x)],
                    pad = pad,
                    width = len_indwise_remain
                ))
            }
            return(y)
        })
    } else {
        stop("len must be indwise_max, overall_max, a numeric or a vector of numerics.")
    }

    # insert padded numbers to original strings
    parts_with_num_parts_pad <- purrr::pmap(list(parts, num_positions, num_parts_pad), function(x,y,z) {
        x[y] <- z
        return(x)
    })
    y <- purrr::map_chr(parts_with_num_parts_pad, paste0, collapse = "")

    return(y)
}
