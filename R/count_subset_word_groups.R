#' Count common word groups from a vector of characters
#'
#' @param x character vector
#' @param split where to split x
#' @param collapse how to collapse subset groups
#'
#' @returns data frame
#' @export
#'
#' @examples
#' x <- c("this is a vector", "this is a list", "this is a vector group", "a list is what")
#' df1 <- count_subset_word_groups(x)
#' df2 <- count_subset_word_groups(x, collapse = "_")
count_subset_word_groups <- function(x,
                                     split = " ",
                                     collapse = split,
                                     min_len = 1,
                                     consecutive_only = T) {

    xspl <- purrr::map(x, strsplit, split = split)
    xspl <- purrr::list_flatten(xspl)
    all_subs <- unlist(purrr::map(
      xspl,
      make_subsets,
      collapse = collapse,
      min_len = min_len,
      consecutive_only = consecutive_only
    ))
    tab <- sort(table(all_subs), decreasing = T)
    tab <- utils::stack(tab)
    names(tab) <- c("count", "subset")
    tab$subset <- as.character(tab$subset)
    return(tab)
}

make_subsets <- function(x,
                         collapse = "_",
                         min_len = 1,
                         consecutive_only = T) {
    y <- combnn(
      x = x,
      order_matters = F,
      min_len = min_len,
      consecutive_only = consecutive_only
    )[["combs"]]
    subcombs <- purrr::list_flatten(y)
    subcombsp <- purrr::map_chr(subcombs, paste, collapse = collapse)
    return(unname(subcombsp))
}



