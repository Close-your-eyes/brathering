#' Evenly arrange n points in 2D space in square shape
#'
#' May be used to add an ordered jitter to overlapping points.
#'
#' @param n number of points
#' @param height spread in y-direction
#' @param width spread in x-direction
#' @param scale do scale position of arranged points?
#' @param center center their position?
#' @param center_incomplete_rows center rows which are not filled completely
#'
#' @return matrix with 2 columns
#' @export
#'
#' @examples
#' plot(arrange_points_2d(100))
#' plot(arrange_points_2d(100, height = 10, width = 5, center = F))
#' plot(arrange_points_2d(195, height = 10, width = 5, center_incomplete_rows = T))
arrange_points_2d <- function(n,
                              height = 1,
                              width = 1,
                              scale = F,
                              center = T,
                              center_incomplete_rows = T) {

    if (sqrt(n) %% 1 == 0) {
        rows <- sqrt(n)
        cols <- rows

        spacing_x <- width / (cols + 1)
        spacing_y <- height / (rows + 1)
        points <- matrix(nrow = n, ncol = 2)
        counter <- 1
        for (i in 1:rows) {
            for (j in 1:cols) {
                points[counter, 1] <- j * spacing_x
                points[counter, 2] <- i * spacing_y
                counter <- counter + 1
            }
        }
    } else {
        sqrt_points <- sqrt(n)
        rows <- ceiling(sqrt_points)
        cols <- ceiling(sqrt_points)
        spacing_x <- width / (cols - 1)
        spacing_y <- height / (rows - 1)
        points <- matrix(nrow = n, ncol = 2)
        counter <- 1

        if (center_incomplete_rows) {
            for (i in 1:rows) {
                num_cols_current_row <- ifelse(i <= (n %% rows), cols, cols - 1)
                offset_x <- (cols - num_cols_current_row) * spacing_x / 2
                for (j in 1:num_cols_current_row) {
                    if (counter > n) {
                        break
                    }
                    points[counter, 1] <- offset_x + (j - 1) * spacing_x
                    points[counter, 2] <- (i - 1) * spacing_y
                    counter <- counter + 1
                }
            }
        } else {
            for (i in 1:rows) {
                for (j in 1:cols) {
                    if (counter > n) {
                        break
                    }
                    points[counter, 1] <- (j - 1) * spacing_x
                    points[counter, 2] <- (i - 1) * spacing_y
                    counter <- counter + 1
                }
            }
        }
    }

    if (anyNA(points)) {
        message("points contains NA. Some loop is flawed.")
    }
    return(scale(points, center = center, scale = scale))
}

