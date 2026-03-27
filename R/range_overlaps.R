#' Find overlapping ranges
#'
#' @param ranges (named) list of numeric vectors; can be start end only or complete
#' range. can even be unordered; only min and max are considered per entry
#' @param rm_self remove self overlap from each range
#' @param rm_mirrored_dup remove duplicates that only differ in order
#'
#' @returns data frame
#' @export
#'
#' @examples
#' ranges <- list(a = c(1:10),
#'                b = c(8:20),
#'                c = c(1:30),
#'                d = c(40:50))
#' range_overlaps(ranges)
#' range_overlaps(ranges, rm_mirrored_dup = F)
#' range_overlaps(ranges, rm_mirrored_dup = T, rm_self = F)
range_overlaps <- function(ranges,
                           rm_self = T,
                           rm_mirrored_dup = T) {

    # ranges can be start end only, sorted or unsorted vectors
    # ranges <- purrr::map(ranges, ~c(min(x), max(x)))

    # base R
    # n <- length(ranges)
    # overlaps <- list()
    # for (i in 1:(n-1)) {
    #   for (j in (i+1):n) {
    #     r1 <- ranges[[i]]
    #     r2 <- ranges[[j]]
    #
    #     if (r1[1] <= r2[2] && r2[1] <= r1[2]) {
    #       overlaps[[length(overlaps) + 1]] <- c(i, j)
    #     }
    #   }
    # }
    # overlaps

    if (is.null(names(ranges))) {
        names(ranges) <- seq_along(ranges)
    }
    ir <- IRanges::IRanges(start = purrr::map_int(ranges, min),
                           end = purrr::map_int(ranges, max))
    ovlp <- as.data.frame(IRanges::findOverlaps(ir))

    if (rm_self) {
        ovlp <- dplyr::filter(ovlp, queryHits != subjectHits)
    }

    if (nrow(ovlp) == 0) {
        return(NULL)
    }

    ovlp <- dplyr::rename(ovlp, "range1" = queryHits, "range2" = subjectHits)

    if (rm_mirrored_dup) {
        ovlp <- sort_cols_rowwise(ovlp) |>
            dplyr::distinct()
    }

    ovlp$range1 <- names(ranges)[ovlp$range1]
    ovlp$range2 <- names(ranges)[ovlp$range2]

    return(ovlp)

}
