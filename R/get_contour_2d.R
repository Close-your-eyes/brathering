#' Calculate contour lines from x-y coordinates
#'
#' @param x x coordinates or data frame
#' @param y y coordinates or empty when x is data frame
#' @param group group vector of column name in x
#' @param n passed to MASS::kde2d; large: rounder contours
#' @param scale scale levels to range 0-1 (for each group separately when
#' !is.null(group))
#' @param nlevels how many contour line levels
#' @param levels define contour levels explicitly; easy to use when scale = T
#' because then choose value between 0-1; if scale = F levels are not predictable
#' before
#'
#' @returns data frame to use with ggplot2::geom_path
#' @export
#'
#' @examples
#' df1 <- data.frame(x = rnorm(2000, mean = -10, sd = 3),
#'                   y = rnorm(2000, mean = -10, sd = 1),
#'                   group = "1")
#' df2 <- data.frame(x = rnorm(2000, mean = 10, sd = 1),
#'                   y = rnorm(2000, mean = 10, sd = 2),
#'                   group = "2")
#' df <- rbind(df1, df2)
#' cdf <- get_contour_2d(x = df, group = "group", levels = 0.2)
#' ggplot(df, aes(x, y)) +
#'     geom_point(alpha = 0.3, aes(color = group)) +
#'     geom_path(data = cdf, aes(x, y, group = combined_group))
get_contour_2d <- function(x,
                           y,
                           group = NULL,
                           n = NULL,
                           scale = T,
                           nlevels = 10,
                           levels = NULL) {

    xname <- "x"
    yname <- "y"
    groupname <- "group"
    if (missing(y) && (is.matrix(x) || is.data.frame(x))) {
        if (!is.null(group)) {
            if (length(group) == 1 && group %in% names(x)) {
                groupname <- group
                group <- x[[group]] #works with matrix?
            }
        }
        xname <- names(x)[1]
        yname <- names(x)[2]
        y <- x[,2]
        x <- x[,1]
    }
    if (is.null(group)) {
        group <- "1"
    }
    if (is.null(n)) {
        # adaptive grid: finer for larger samples
        n <- pmin(200, max(50, sqrt(length(x))))
    }

    if (is.factor(group)) {
        group <- as.character(group)
    }
    contour_df <- purrr::map2_dfr(.x = split(x, group), .y = split(y, group), function(x,y) {
        # decrease/increase limits by 10 % to not have contour lines cut prematurely
        dens <- MASS::kde2d(x, y, n = n, lims = c(adjust_range(range(x)), adjust_range(range(y))))
        if (scale) {
            dens$z <- dens$z/max(dens$z)
        }
        if (is.null(levels)) {
            clines <- grDevices::contourLines(dens$x, dens$y, dens$z, nlevels = nlevels)
        } else {
            clines <- grDevices::contourLines(dens$x, dens$y, dens$z, levels = levels)
        }

        names(clines) <- as.character(seq_along(clines))
        contour_df <- purrr::map_dfr(clines,
                                     ~data.frame(level = .x$level,
                                                 x = .x$x,
                                                 y = .x$y),
                                     .id = "line_group")
        return(contour_df)
    }, .id = groupname)

    names(contour_df)[c(4,5)] <- c(xname, yname)
    contour_df[["combined_group"]] <- paste0(contour_df[[groupname]], "_", contour_df[["line_group"]])
    return(contour_df)
}
adjust_range <- function(r, pct = 0.2) {
    lower <- if (r[1] < 0) r[1] * (1 + pct) else r[1] * (1 - pct)
    upper <- if (r[2] < 0) r[2] * (1 - pct) else r[2] * (1 + pct)
    return(c(lower, upper))
}
