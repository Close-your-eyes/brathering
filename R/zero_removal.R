#' Count number of leading elements of one kind
#'
#' Use it to count number of leading zero in a time series, e.g. To count
#' trailing use rev(vec).
#'
#' @param x vector of whatever
#' @param what element to count
#'
#' @return integer of count of leading occurrences of what
#' @export
#'
#' @examples
#' lead_count(c(0,0,1,2,3,4,5))
#' lead_count(c("r","r","r","s","t","u","d","i","o"), "r")
#' lead_count(rev(c(1,2,3,4,0,0,0,0)))
#' lead_count(strsplit("111110001110011001","")[[1]],"1")
#' lead_count(c(NA, NA, NA, 0,0,1,1,2,"a"), NA_character_)
lead_count <- function(x, what = 0) {
    stopifnot("what has to have length 1" = length(what) == 1)
    comparefun <- ifelse(is.numeric(x), dplyr::near, Vectorize(identical, "x"))
    # NA in between handled
    # NA at beginning avoid counting
    return(as.integer(sum(na.omit(cumprod(comparefun(x, what))))))
}

#' Get indices of leading elements of one kind
#'
#' Trivial but included as analogy to trail_inds. Originally intended to
#' count zeros. So, what = 0 by default.
#'
#' @param x vector of whatever
#' @param what element to count
#'
#' @return vector of indices starting at 1
#' @export
#'
#' @examples
#' lead_inds(c(0,0,1,2,3,4,5))
#' lead_inds(c("r","r","r","s","t","u","d","i","o"), "r")
#' lead_inds(rev(c(1,2,3,4,0,0,0,0)))
#' lead_inds(strsplit("111110001110011001","")[[1]],"1")
#' lead_inds(c(NA, NA, NA, 0,0,1,1,2,"a"), NA_character_)
lead_inds <- function(x, what = 0) {
    ind <- lead_count(x, what = what)
    if (ind>0) {
        return(1:ind)
    } else {
        return(NULL)
    }
}

#' Get indices of trailing elements of one kind
#'
#' Originally intended to count zeros. So, what = 0 by default.
#'
#' @param x vector of whatever
#' @param what element to count
#'
#' @return vector of indices
#' @export
#'
#' @examples
#' trail_inds(rev(c(0,0,1,2,3,4,5)))
#' trail_inds(rev(c("r","r","r","s","t","u","d","i","o")), "r")
#' trail_inds(c(1,2,3,4,0,0,0,0))
#' trail_inds(rev(strsplit("111110001110011001","")[[1]]),"1")
#' trail_inds(rev(c(NA, NA, NA, 0,0,1,1,2,"a")), NA_character_)
trail_inds <- function(x, what = 0) {
    len <- length(x)
    ind <- lead_inds(x = rev(x), what = what)
    if (!is.null(ind)) {
        return(len-ind+1)
    } else {
        return(NULL)
    }
}

#' Trim anything from a vector
#'
#' Leading and trailing 'what' is removed from x. Respective counts are
#' recorded as attributes lead_rm and trail_rm.
#'
#' @param x vector of whatever
#' @param what what to trim
#'
#' @return trimmed vector with attributes
#' @export
#'
#' @examples
#' trim_any(c(1,1,1,1,1,1,1), what = 1)
#' trim_any(c(0,0,0,1,2,3,0,0,0,4,5,0,0))
#' trim_any(c(NA,NA,NA,1,1,1,NA,NA,2,2,2,"a"), what = NA_character_)
#' trim_any(as.integer(c(NA,NA,NA,1,1,1,NA,NA,2,2,2)), what = NA_integer_) # not working
trim_any <- function(x, what = 0) {
    lead <- lead_inds(x, what = what)
    trail <- trail_inds(x, what = what)
    trail <- setdiff(trail, lead) # when x is what only
    if (length(c(lead,trail))) {
        x <- x[-c(lead,trail)]
    }
    attr(x, "lead_rm") <- length(lead)
    attr(x, "trail_rm") <- length(trail)
    return(x)
}


#' Title
#'
#' @param df
#' @param cols
#' @param which
#'
#' @return
#' @export
#'
#' @examples
trim_zeros_df_cols <- function(df, cols, which = list(c("lead", "trail"))) {

    # no checks yet


    # make long df and split by em ex first
    # then rm zeros
    # then rbind
    # then wide df

    df <- tidyr::pivot_longer(df, cols = dplyr::all_of(cols))
    df <- split(df, df$name)
    which <- brathering::recycle(which, df)
    df <- purrr::map2_dfr(.x = df, .y = which, function(x,y) {
        rminds1 <- NULL
        rminds2 <- NULL
        x <- tidyr::drop_na(x, value)
        if ("lead" %in% y) {
            rminds1 <- lead_inds(x[["value"]])
        }
        if ("trail" %in% y) {
            rminds2 <- trail_inds(x[["value"]])
        }
        rminds <- unique(c(rminds1, rminds2))
        if (is.null(rminds)) {
            return(x)
        }
        return(x[-rminds,])
    })

    df <- tidyr::pivot_wider(df) |> dplyr::arrange(nm)
    return(df)
}
