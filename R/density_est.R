#' 2D or 3D density estimation
#'
#' @param x matrix or x values, when matrix: col1 becomes x, col2 becomes y
#' @param y optional: y values when x is not a matrix
#' @param z optional: z values when x is not a matrix
#' @param n number of bins, passed to MASS::kde2d
#' @param ... arguments to MASS::kde2d
#' @param type 2D or 3D estimation
#'
#' @return list with raw and 0-to-1-scaled density values
#' @export
#'
#' @examples
#' density_est(data.frame(x = rnorm(1000), y = rnorm(1000), z = rnorm(1000)))
density_est <- function(x,
                        y,
                        z,
                        n = 100,
                        type = c("2D", "3D"),
                        ...) {

    type <- rlang::arg_match(type)

    if (type == "3D") {
        if (missing(y) && missing(z) && (is.matrix(x) || is.data.frame(x))) {
            y <- x[,2]
            z <- x[,3]
            x <- x[,1]
        }
        dens <- kde3d(x = x, y = y, z = z, n = n)
        ix <- findInterval(x, dens$x)
        iy <- findInterval(y, dens$y)
        iz <- findInterval(z, dens$z)
        ii <- cbind(cbind(ix, iy), iz)
        return(list(raw = dens$d[ii], scaled = dens$d[ii]/max(dens$d[ii])))
    }
    if (type == "2D") {
        if (missing(y) && (is.matrix(x) || is.data.frame(x))) {
            y <- x[,2]
            x <- x[,1]
        }
        dens <- MASS::kde2d(x = x, y = y, n = n, ...)
        ix <- findInterval(x, dens$x)
        iy <- findInterval(y, dens$y)
        ii <- cbind(ix, iy)
        return(list(raw = dens$z[ii], scaled = dens$z[ii]/max(dens$z[ii])))
    }

}


kde3d <- function (x, y, z, h, n = 100, lims = c(range(x), range(y), range(z))) {
    # from misc3d::kde3d
    nx <- length(x)
    if (length(y) != nx || length(z) != nx)
        stop("data vectors must be the same length")
    if (missing(h))
        h <- c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y),
               MASS::bandwidth.nrd(z))/6
    else if (length(h) != 3)
        h <- rep(h, length = 3)
    if (length(n) != 3)
        n <- rep(n, length = 3)
    if (length(lims) == 2)
        lims <- rep(lims, length = 6)
    gx <- seq(lims[1], lims[2], length = n[1])
    gy <- seq(lims[3], lims[4], length = n[2])
    gz <- seq(lims[5], lims[6], length = n[3])
    mx <- matrix(outer(gx, x, stats::dnorm, h[1]), n[1], nx)
    my <- matrix(outer(gy, y, stats::dnorm, h[2]), n[2], nx)
    mz <- matrix(outer(gz, z, stats::dnorm, h[3]), n[3], nx)
    v <- array(0, n)
    tmy.nx <- t(my)/nx
    for (k in 1:n[3]) {
        tmy.nz.zk <- tmy.nx * mz[k, ]
        v[, , k] <- mx %*% tmy.nz.zk
    }
    return(list(x = gx, y = gy, z = gz, d = v))
}
