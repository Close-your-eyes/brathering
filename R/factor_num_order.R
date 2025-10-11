#' Order character by contained numerics
#'
#' @param x character vector numbers in it
#' @param return type to return. levels: ordered levels of x. factor: x as factor
#' with ordered levels
#'
#' @returns character or factor
#' @export
#'
#' @examples
#' # one num per character
#' factor_num_order(c("C3", "C1", "C2"))
#' # multiple nums
#' factor_num_order(c("C9C3", "C9C1", "C9C2"))
#' factor_num_order(c("C9x8C3", "C9x8C1", "C9x8C2"))
#' # diff numbers of nums across x
#' factor_num_order(c("C9x8C3", "C9x8C1", "C9x8C2v7"))
factor_num_order <- function(x, return = c("levels", "factor")) {
    return <- rlang::arg_match(return)
    y <- unique(x)

    # try to handle multiple numerics in strings
    nums <- stringr::str_extract_all(y, "[0-9]+")
    if (length(unique(lengths(nums))) > 1) {
        message("different number of numerics found in strings. using common first n for ordering.")
        min_n <- min(lengths(nums))
        nums <- purrr::map(nums, ~.x[seq_len(min_n)])
    }
    nums <- purrr::map(nums, as.numeric)

    order <- as.data.frame(t(as.data.frame(nums, col.names = seq_along(nums)))) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(is.na(.x), Inf, .x))) %>%
        dplyr::mutate(!!random_varname(names(.)) := dplyr::row_number()) %>%
        dplyr::arrange(dplyr::pick(dplyr::everything())) %>%
        dplyr::pull()

    # ynum <- as.numeric(nums)
    # ynum[is.na(ynum)] <- Inf
    # y <- y[order(ynum)]
    y <- y[order]

    if (return == "levels") {
        return(y)
    } else if (return == "factor") {
        factor(x, levels = y)
    }
}
