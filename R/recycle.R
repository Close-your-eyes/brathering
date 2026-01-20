#' Recycle vectors or lists
#'
#' Recycle short element to length of long element.
#'
#' @param short shorter element, vector or list
#' @param long longer element
#' @param one_or_equal shorter element must be length one or equal to longer
#' element
#'
#' @return vector or list of length(long) and class(x)
#' @export
#'
#' @examples
#' # vectors
#' recycle(letters[1:5], LETTERS[1:15]) # 15/5 = integer
#' recycle(letters[1:5], LETTERS[1:13]) # 13/5 = double
#' # vector and list
#' recycle(list(a = letters[1:5], b = letters[1:3]), LETTERS[1:4])
#' # lists
#' recycle(list(a = letters[1:5], b = letters[1:3]), as.list(LETTERS[1:5]))
recycle <- function(short,
                    long,
                    one_or_equal = F) {

    if (length(long) < length(short)) {
        message("recycling: short is longer than long and will be cut to length(long).")
    } else if (length(long) %% length(short) != 0) {
        message("recycling: longer element is not a multiple of the shorter.")
    }

    if (one_or_equal) {
        if (length(short) != 1 && length(short) != length(long)) {
            stop("recycling: shorter element should be length one or of length(long).")
        }
    }

    return(rep(short, length.out = length(long)))
}



