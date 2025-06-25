#' Title
#'
#' @param vec
#' @param what
#'
#' @return
#' @export
#'
#' @examples
leading_zeros_count <- function(vec, what = 0) {
    stopifnot("what has to have length 1" = length(what) == 1)
    # NA in between handled
    # NA at beginning avoid counting zeros
    return(sum(na.omit(cumprod(vec == what))))
}

#' Title
#'
#' @param vec
#' @param what
#'
#' @return
#' @export
#'
#' @examples
leading_zero_inds <- function(vec, what = 0) {
    ind <- leading_zeros_count(vec, what = what)
    if (ind>0) {
        return(1:ind)
    } else {
        return(NULL)
    }
}

#' Title
#'
#' @param vec
#' @param what
#'
#' @return
#' @export
#'
#' @examples
trailing_zero_inds <- function(vec, what = 0) {
    len <- length(vec)
    ind <- leading_zero_inds(vec = rev(vec), what = what)
    if (!is.null(ind)) {
        return(len-ind+1)
    } else {
        return(NULL)
    }
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
trim_zeros <- function(x) {
    lead <- leading_zero_inds(x)
    trail <- trailing_zero_inds(x)
    trail <- setdiff(trail, lead)
    if (length(c(lead,trail))) {
        x <- x[-c(lead,trail)]
    }
    attr(x, "lead0") <- length(lead)
    attr(x, "trail0") <- length(trail)
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
            rminds1 <- leading_zero_inds(x[["value"]])
        }
        if ("trail" %in% y) {
            rminds2 <- trailing_zero_inds(x[["value"]])
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
