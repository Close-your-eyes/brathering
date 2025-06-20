#' Title
#'
#' See package gcplyr.
#'
#' @param x
#' @param threshold
#' @param min_gap
#' @param close_extr
#' @param type
#'
#' @return
#' @export
#'
#' @examples
findlocmax <- function(x,
                       min_gap = 0,
                       threshold = -Inf,
                       type = c("max", "min"),
                       min_mono_width = 1,
                       sizes = c(1,2,3,4,5),
                       strictly_mono = F) {
    # how to handle NA

    type <- rlang::arg_match(type)
    if (min_mono_width <= 0) {
        message("min_mono_width must be >=1")
        min_mono_width <- 1
    }
    if (min_mono_width <= 0) {
        message("min_gap must be >=0")
        min_mono_width <- 0
    }

    #lefindsign <- ifelse(type == "max", 1, -1)
    minmaxfun <- ifelse(type == "max", brathering::which.max2, brathering::which.min2)
    comparefun <- ifelse(type == "max", `>=`, `<=`)

    xna <- brathering::na_rm(x)
    x <- xna[["x"]]

    xtrmlist <- purrr::map(stats::setNames(sizes, as.character(sizes)), function(size) {

        ## remain unchanged
        winsize <- size
        size <- size+min_mono_width*2
        starts <- seq(1, length(x)-size, 1)
        inds <- brathering::seq2(starts, starts+size-1)
        left <- purrr::map(inds, ~x[.x[1:(min_mono_width+1)]])
        right <- purrr::map(inds, ~x[.x[(length(.x)-min_mono_width):length(.x)]])
        win <- purrr::map(inds, ~x[.x[(min_mono_width+1):(length(.x)-min_mono_width)]])
        wininds <- purrr::map(inds, ~.x[(min_mono_width+1):(length(.x)-min_mono_width)])
        win <- purrr::map2(win, wininds, ~setNames(.x,.y))

        # subject to filter below
        winxtrmrelind <- purrr::map(win, minmaxfun)
        #winindsxtrm <- purrr::map(winxtrmrelind, ~as.numeric(names(.x)))
        #names(winindsxtrm) <- seq_along(winindsxtrm) # names are window indices
        winxtrmval <- purrr::map2(win, winxtrmrelind, ~.x[.y])

        #extremes, min_mono_width included
        is_xtrm <- purrr::map2_lgl(
            .x = left,
            .y = right,
            ~check_xtrm(leftvec = .x,
                        rightvec = .y,
                        strictly_mono = strictly_mono,
                        fun = comparefun,
                        type = type)
        )
        #winxtrmrelind <- winxtrmrelind[is_xtrm]
        #winindsxtrm <- winindsxtrm[is_xtrm]
        winxtrmval <- winxtrmval[is_xtrm]

        #threshold
        is_below_thresh <- purrr::map_lgl(winxtrmval, ~any(comparefun(.x, threshold)))
        #winxtrmrelind <- winxtrmrelind[is_below_thresh]
        #winindsxtrm <- winindsxtrm[is_below_thresh]
        winxtrmval <- winxtrmval[is_below_thresh]

        #close extremes
        vec <- unlist(winxtrmval)
        is_duplicate <- which(duplicated(names(vec)))
        if (length(is_duplicate)) {
            vec <- vec[-is_duplicate]
        }
        vec <- vec[order(as.numeric(names(vec)))]
        group_id <- cumsum(c(TRUE, diff(as.numeric(names(vec))) >= min_gap)) # consecutive groups
        vecgrouped <- split(vec, group_id)
        xtrmpergroup <- purrr::map(vecgrouped, ~as.numeric(names(.x[minmaxfun(.x)]))) # max per group, continue with indices only

        xtrmpergroup <- sort(purrr::map_int(unname(xtrmpergroup), ~selectmidvalue(.x))) # which xtrm to select? first, last, mid, random

        browser()
        # reinsert NA
        x2 <- brathering::na_insert(xna)
        # alter xtrmpergroup
        xtrmpergroup <- brathering::na_fix_ind(x, x2, xtrmpergroup)
        xtrmfinal <- data.frame(
            index = seq_along(x2),
            x = x2,
            extrm = seq_along(x2) %in% xtrmpergroup,
            type = type,
            type2 = ifelse(seq_along(x2) %in% xtrmpergroup, "local", NA)
        )
        return(xtrmfinal)
    })

    ## fallback to global extreme when no local was found
    return(xtrmlist)
}


check_xtrm <- function(leftvec,
                       rightvec,
                       strictly_mono = F,
                       fun = comparefun,
                       type = "max") {
    #browser()
    val <- ifelse(strictly_mono, 1, 0)
    if (type == "max") {
        rdiff <- diff(rightvec)*-1
        ldiff <- diff(leftvec)
    } else {
        rdiff <- diff(rightvec)
        ldiff <- diff(leftvec)*-1
    }
    all(fun(ldiff,val)) && all(fun(rdiff,val))
}

selectmidvalue <- function(y) {
    y <- as.numeric(y)
    mid <- round(median(y))
    midind <- y[which.min(abs(mid-y))]
    return(midind)
}


res <- findlocmax(x = c(1,2,3,4,5,4,3,2,1,1,1,1,2,3,4,5,5,5,4,3,2,1,1,1,1,1,2,3,4,5,4,5,4,3,2,1,1,1,2,3,4,3,2,1,1,1,2,3,4,5,6,6,3,2,1,1,1,5,4,1),
                  min_gap = 3,
                  threshold = 3,
                  type = "max",
                  min_mono_width = 3,
                  sizes = 3,
                  strictly_mono = T)[[1]]
brathering::plot2(res, color = "extrm", cex = 10, legend = NULL)



# catgpt:
# find_local_extrema_inds <- function(
#         x,
#         threshold = -Inf,
#         min_gap = 0,
#         close_extremes = c("random", "both"),
#         type = c("maxima", "minima"),
#         min_mono_width = 0,
#         adaptive_window_sizes = NULL
# ) {
#     close_extremes <- rlang::arg_match(close_extremes)
#     type <- rlang::arg_match(type)
#
#     # Flip sign for minima search
#     x_proc <- if (type == "minima") -x else x
#
#     original_indices <- seq_along(x_proc)
#     na_mask <- !is.na(x_proc)
#     x_clean <- x_proc[na_mask]
#     index_clean <- original_indices[na_mask]
#     x_orig_clean <- x[na_mask]
#
#     n <- length(x_clean)
#     extrema_candidates <- integer(0)
#
#     i <- 2
#     while (i < n) {
#         if (x_clean[i] > x_clean[i - 1]) {
#             start <- i
#             while (i < n && x_clean[i] == x_clean[i + 1]) {
#                 i <- i + 1
#             }
#             if (i < n && x_clean[i] > x_clean[i + 1]) {
#                 mid <- round((start + i) / 2)
#                 value_at_mid <- x_orig_clean[mid]
#
#                 # Threshold condition
#                 if ((type == "maxima" && value_at_mid > threshold) ||
#                     (type == "minima" && value_at_mid < threshold)) {
#
#                     # Semi-monotonic trend check
#                     pass_prominence <- TRUE
#                     if (min_mono_width > 0) {
#                         left_start <- max(1, mid - min_mono_width)
#                         right_end <- min(n, mid + min_mono_width)
#
#                         left_trend <- all(diff(x_clean[left_start:mid]) >= 0)
#                         right_trend <- all(diff(x_clean[mid:right_end]) <= 0)
#
#                         if (!(left_trend && right_trend)) {
#                             pass_prominence <- FALSE
#                         }
#                     }
#
#                     # Adaptive window check
#                     pass_window <- FALSE
#                     if (!is.null(adaptive_window_sizes)) {
#                         for (w in adaptive_window_sizes) {
#                             if (mid > w && mid <= n - w) {
#                                 left <- x_clean[(mid - w):(mid - 1)]
#                                 right <- x_clean[(mid + 1):(mid + w)]
#                                 center <- x_clean[mid]
#
#                                 if (all(center > left) && all(center > right)) {
#                                     pass_window <- TRUE
#                                     break
#                                 }
#                             }
#                         }
#                     }
#
#                     # Accept if passes either strategy
#                     if (pass_prominence || pass_window) {
#                         extrema_candidates <- c(extrema_candidates, index_clean[mid])
#                     }
#                 }
#             }
#         }
#         i <- i + 1
#     }
#
#     # Merge close extrema
#     if (min_gap > 0 && length(extrema_candidates) > 1) {
#         kept <- logical(length(extrema_candidates))
#         i <- 1
#         while (i <= length(extrema_candidates)) {
#             group <- extrema_candidates[i]
#             j <- i + 1
#             while (j <= length(extrema_candidates) &&
#                    (extrema_candidates[j] - extrema_candidates[j - 1]) < min_gap) {
#                 group <- c(group, extrema_candidates[j])
#                 j <- j + 1
#             }
#
#             values <- x[group]
#             best_val <- if (type == "maxima") max(values) else min(values)
#             best_indices <- which(values == best_val)
#
#             if (length(best_indices) == 1 || close_extremes == "random") {
#                 keep_one <- if (length(best_indices) == 1) best_indices else sample(best_indices, 1)
#                 kept[i + keep_one - 1] <- TRUE
#             } else if (close_extremes == "both") {
#                 kept[i - 1 + best_indices] <- TRUE
#             }
#
#             i <- j
#         }
#         extrema_candidates <- extrema_candidates[kept]
#     }
#
#     # Fallback: global max/min
#     if (length(extrema_candidates) == 0 && any(!is.na(x))) {
#         fallback_index <- if (type == "maxima") which.max(x) else which.min(x)
#         extrema_candidates <- fallback_index
#     }
#
#     return(extrema_candidates)
# }
