#' Evenly arrange n points in 2D space in circular shape
#'
#' @param n number of points
#' @param n_ring number of rings for arrangement
#' @param n_points_per_ring number of points per ring; sum must equal n
#' @param n_point_ring1 only define number of points on first ring; leave
#' remaining to internal heuristic
#' @param n_point_increment when setting n_point_ring1, increase of point on
#' outer rings
#' @param inner_radius radius of first ring
#' @param radius_increment radius increment per ring
#' @param center_x shift center in x-direction
#' @param center_y shift center in y-direction
#' @param return_polars also return radius and angle (polar coordinates),
#' next to cartesian coordinates
#'
#' @return matrix with 2 or 4 columns (cartesian without or with polar coordinates)
#' @export
#'
#' @examples
#' # number of rings and points per ring are set by own heuristic
#' plot(arrange_points_2d_circle(100))
#' # set number of rings
#' plot(arrange_points_2d_circle(100, n_ring = 1))
#' plot(arrange_points_2d_circle(100, n_ring = 10))
#' # set number of points per ring; sum must equal n
#' plot(arrange_points_2d_circle(100, n_points_per_ring = c(5,20,40,35)))
#' # only define points for first ring; n_ring is ignored then
#' plot(arrange_points_2d_circle(100, n_point_ring1 = 50, n_ring = 6))
#' # but incrememt per ring can be adjusted
#' plot(arrange_points_2d_circle(100, n_point_ring1 = 2, n_point_increment = 40))
#' plot(arrange_points_2d_circle(100, center_x = 5)) # shift center
#' # ... and decrease radius increment
#' points(arrange_points_2d_circle(50, center_x = 5, radius_increment = 0.15),
#'        col = "red")
#' # 3rd column: radius, 4th column: angle in radiants
#' arrange_points_2d_circle(100, return_polars = TRUE)
arrange_points_2d_circle <- function(n,
                                     n_ring = NULL,
                                     n_points_per_ring = NULL,
                                     n_point_ring1 = NULL,
                                     n_point_increment = 5,
                                     inner_radius = 0.2,
                                     radius_increment = 0.2,
                                     center_x = 0,
                                     center_y = 0,
                                     return_polars = FALSE) {

    if (is.null(n_ring)) {
        if (!is.null(n_points_per_ring)) {
            n_ring <- length(n_points_per_ring)
        } else {
            n_ring <- max(1, floor(sqrt(n)/2))
        }
    } else {
        if (n_ring > n) {
            stop("n_ring > n not possible.")
        }
    }

    if (is.null(n_points_per_ring)) {
        if (is.null(n_point_ring1)) {
            n_points_per_ring <- max(c(floor(n/n_ring), floor(n/7)))
        } else {
            n_points_per_ring <- n_point_ring1
        }
        while (sum(n_points_per_ring) < n) {
            n_points_per_ring <- c(n_points_per_ring, n_points_per_ring[length(n_points_per_ring)] + floor(n_point_increment))
        }
        if (sum(n_points_per_ring) > n) {
            n_points_per_ring[length(n_points_per_ring)] <- n_points_per_ring[length(n_points_per_ring)] -(sum(n_points_per_ring) - n)
        }
    } else {
        if (length(n_points_per_ring) != n_ring) {
            stop("length(n_points_per_ring) != n_ring")
        }
        if (sum(n_points_per_ring) != n) {
            stop("sum(n_points_per_ring) != n")
        }
    }

    points <- matrix(nrow = sum(n_points_per_ring), ncol = 4)

    for (ring in 1:length(n_points_per_ring)) {
        radius <- ring * radius_increment
        angle_increment <- 2 * pi / n_points_per_ring[ring]
        start_angle <- angle_increment / 2

        for (i in 1:n_points_per_ring[ring]) {
            angle <- start_angle + (i - 1) * angle_increment

            x <- center_x + radius * cos(angle)
            y <- center_y + radius * sin(angle)

            points[sum(n_points_per_ring[0:(ring-1)]) + i, 1] <- x
            points[sum(n_points_per_ring[0:(ring-1)]) + i, 2] <- y
            points[sum(n_points_per_ring[0:(ring-1)]) + i, 3] <- radius
            points[sum(n_points_per_ring[0:(ring-1)]) + i, 4] <- angle
        }
    }

    if (return_polars) {
        return(points)
    } else {
        return(points[,c(1,2)])
    }

}


