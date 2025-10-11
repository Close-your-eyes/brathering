#' Reorganize a nested list by switching inner and outer layer
#'
#' @param list a nested list
#' @param non_inner_list how to handle non-list elements
#'
#' @return list with outer and first inner layer swapped
#' @export
#'
#' @examples
#' exam <- list(
#'   a = list(
#'     df  = 1,
#'     vec = 2,
#'     str = 3
#'   ),
#'   b = list(
#'     df  = 4,
#'     vec = 5,
#'     str = 6
#'   ),
#'   c = list(
#'     df  = 7,
#'     vec = 8,
#'     str = 9
#'   ),
#'   d = list(
#'     df  = "x",
#'     vec = "y",
#'     str = "z",
#'     num = "w"
#'   ),
#'   e = "no_list"
#' )
#' inv_exam1 <- list_invert(exam, non_inner_list = "keep")
#' inv_exam2 <- list_invert(exam, non_inner_list = "drop")
list_invert <- function(list,
                        non_inner_list = c("keep", "drop")) {

    stopifnot("list must be a list" = is.list(list))

    if (is.null(names(list))) {
        names(list) <- as.character(seq_along(list))
    }
    non_inner_list <- rlang::arg_match(non_inner_list)

    depth <- list_depth(list)
    if (depth < 2) {
        message("min depth of nested list is 2. your list seem not nested.")
        return(list)
    }
    inner_depths <- purrr::map_int(list, list_depth)

    # remove elements which are not list at inner layer
    nonlist <- list[which(inner_depths == 0)]
    list <- list[which(inner_depths > 0)]

    all_names <- c(names(list), unique(unlist(purrr::map(list, names))))
    sep <- find_sep(all_names)
    list_flat <- purrr::list_flatten(list, name_spec = paste0("{outer}", sep, "{inner}"))
    inner_groups <- sapply(strsplit(names(list_flat), split = sep), "[", 2)
    inner_groups_lvl <- unique(inner_groups)

    # aggregate_inner_lists
    invert_list <- purrr::map(stats::setNames(inner_groups_lvl, inner_groups_lvl), function(lvl) {
        inds <- which(inner_groups == lvl)
        inner_list <- list_flat[inds]
        names(inner_list) <- sapply(strsplit(names(inner_list), split = sep), "[", 1)
        return(inner_list)
    })

    if (non_inner_list == "keep") {
        invert_list <- c(invert_list, nonlist)
    }

    return(invert_list)
}


