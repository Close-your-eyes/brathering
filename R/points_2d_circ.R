#' Evenly arrange n points in 2D space in circular shape
#'
#' May be used to add an ordered jitter to overlapping points.
#' n must be provided. Then, there are four ways to run this: (i) provide ring1
#' and n_incr or (ii) define per_ring explicitly as a vector or (iii) define
#' n_ring and have an algorithm (per_ring_algo) fill the rings or (iv) not even
#' define n_ring to have it chosen by a formula: max(1, floor(sqrt(n)/2)).
#'
#' @param n number of points
#' @param n_ring number of rings for arrangement
#' @param per_ring number of points per ring; sum must equal n
#' @param ring1 number of points on first ring
#' @param n_incr increase of point per ring
#' @param r_inner radius of first ring
#' @param r_incr radius increment per ring
#' @param center_x shift center in x-direction
#' @param center_y shift center in y-direction
#' @param return_polars also return radius and angle (polar coordinates),
#' next to cartesian coordinates
#' @param per_ring_algo how to fill rings? same_n: close equal number of points
#' per ring, same_dist: equally spaced points taking into account circumfences,
#' or one of two random procedures.
#'
#' @return matrix with 2 or 4 columns (cartesian without or with polar
#' coordinates)
#' @export
#'
#' @examples
#' # number of rings and points per ring are set by own heuristic
#' plot(points_2d_circ(100))
#' # set number of rings
#' plot(points_2d_circ(100, n_ring = 1))
#' plot(points_2d_circ(100, n_ring = 10))
#' # set number of points per ring; sum must equal n
#' plot(points_2d_circ(100, per_ring = c(5,20,40,35)))
#' # only define points for first ring; n_ring is ignored then
#' plot(points_2d_circ(100, ring1 = 50, n_ring = 6))
#' # but increment per ring can be adjusted
#' plot(points_2d_circ(100, ring1 = 2, n_incr = 40))
#' plot(points_2d_circ(100, center_x = 5)) # shift center
#' # ... and decrease radius increment
#' points(points_2d_circ(50, center_x = 5, r_incr = 0.15),
#'        col = "red")
#' # 3rd column: radius, 4th column: angle in radiants
#' points_2d_circ(100, return_polars = TRUE)
points_2d_circ <- function(n,
                           n_ring = NULL,
                           per_ring = NULL,
                           per_ring_algo = c("same_n", "same_dist", "random1", "random2"),
                           ring1 = NULL,
                           n_incr = NULL,
                           r_inner = 0.2,
                           r_incr = 0.2,
                           center_x = 0,
                           center_y = 0,
                           return_polars = FALSE) {

    per_ring_algo <- rlang::arg_match(per_ring_algo)

    if (!is.null(ring1) && !is.null(n_incr)) {
        if (ring1 > n) {
            stop("ring1 cannot be greater n.")
        }
        per_ring <- ring1
        while (sum(per_ring) < n) {
            per_ring <- c(per_ring, per_ring[length(per_ring)] + n_incr)
        }
        if (sum(per_ring) > n) {
            per_ring[length(per_ring)] <- per_ring[length(per_ring)] -(sum(per_ring) - n)
        }
        n_ring <- length(per_ring)
    }

    if (is.null(n_ring)) {
        if (!is.null(per_ring)) {
            n_ring <- length(per_ring)
        } else {
            n_ring <- max(1, floor(sqrt(n)/2))
        }
    } else {
        if (n_ring > n) {
            stop("n_ring > n not possible.")
        }
    }

    radii <- c(r_inner, r_inner + purrr::accumulate(.x = rep(r_incr, n_ring-1), .f = `+`, .init = 0))[1:n_ring]
    circ <- 2*pi*radii

    if (is.null(per_ring)) {
        if (per_ring_algo == "same_n") {
            per_ring <- n/n_ring
            per_ring <- brathering::round2(rep(per_ring, n_ring))
        }
        if (per_ring_algo == "same_dist") {
            per_ring <- brathering::round2(n*circ/sum(circ))
        }
        if (per_ring_algo == "random1") {
            per_ring <- floor(n/(n_ring + sample(1:n_ring, 1)))
            per_ring <- sort(c(per_ring, random_strictly_increasing_sum(n = n-per_ring, m = n_ring-1, z = per_ring+1)))
        }
        if (per_ring_algo == "random2") {
            per_ring <- sort(random_sum(n, n_ring))
        }
    } else {
        if (sum(per_ring) != n) {
            stop("sum of per_ring must equal n.")
        }
    }

    points <- matrix(nrow = sum(per_ring), ncol = 4)

    for (ring in 1:length(per_ring)) {
        radius <- ring * r_incr
        angle_increment <- 2 * pi / per_ring[ring]
        start_angle <- angle_increment / 2

        for (i in 1:per_ring[ring]) {
            angle <- start_angle + (i - 1) * angle_increment

            x <- center_x + radius * cos(angle)
            y <- center_y + radius * sin(angle)

            points[sum(per_ring[0:(ring-1)]) + i, 1] <- x
            points[sum(per_ring[0:(ring-1)]) + i, 2] <- y
            points[sum(per_ring[0:(ring-1)]) + i, 3] <- radius
            points[sum(per_ring[0:(ring-1)]) + i, 4] <- angle
        }
    }

    if (return_polars) {
        return(points)
    } else {
        return(points[,c(1,2)])
    }
}

random_strictly_increasing_sum <- function(n, m, z = 1) {
    min_required <- m * z + (m * (m - 1)) / 2  # since differences must be at least 1, 2, ..., m-1

    if (n < min_required) {
        stop("Impossible: n is too small for a strictly increasing sequence with the given z")
    }

    # Step 1: Base increasing sequence (0, 1, ..., m-1)
    base <- 0:(m - 1)

    # Step 2: Subtract base and z*m from n to get remaining to randomly distribute
    remaining <- n - sum(base) - m * z

    # Step 3: Random partition of 'remaining' into m parts (can be zero)
    parts <- if (remaining > 0) {
        diff(c(0, sort(sample(0:remaining, m - 1, replace = TRUE)), remaining))
    } else {
        rep(0, m)
    }

    # Step 4: Final result = z + base + random parts
    result <- z + base + parts

    return(result)
}

random_sum <- function(n, m) {
    # Generate (m - 1) unique random break points between 1 and (n - 1)
    breaks <- sort(sample(1:(n - 1), m - 1))

    # Add 0 and n as the boundaries, then take differences
    parts <- diff(c(0, breaks, n))

    return(parts)
}



