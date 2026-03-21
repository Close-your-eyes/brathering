#' Sample points on ellipse with cov matrix
#'
#' @param mu center coordinates
#' @param covmat covariance matrix
#' @param n number of points
#' @param c radius or size factor
#'
#' @returns data frame
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mapping = aes(x,y)) +
#' geom_point(data = ellipse_points_2d()) +
#'     geom_point(data = ellipse_points_2d(c = 3), color = "firebrick") +
#'     geom_point(data = ellipse_points_2d(mu = c(1,1), covmat = c(4,0,0,1)), color = "cornflowerblue") +
#'     geom_point(data = ellipse_points_2d(mu = c(-1,-1), covmat = c(4,3,0,1)), color = "forestgreen") +
#'     geom_point(data = ellipse_points_2d(mu = c(-1,-1), covmat = c(4,0,3,1)), color = "hotpink") +
#'     geom_point(data = ellipse_points_2d(mu = c(1,-1), covmat = c(4,2,2,2)), color = "grey60") +
#'     coord_equal()
ellipse_points_2d <- function(mu = c(0,0),
                              covmat = matrix(c(1, 0, 0, 1), nrow = 2),
                              n = 100,
                              c = 1,
                              tol = 1e-10) {

    if (!is.matrix(covmat)) {
        covmat <- matrix(covmat, nrow = 2)
    }

    if (!all(dim(covmat) == c(2, 2))) {
        stop("covmat must be a 2x2 matrix.")
    }

    if (length(mu) != 2) {
        stop("mu must have length 2.")
    }

    if (c < -tol) {
        stop("c must be nonnegative.")
    }

    # if (max(abs(covmat - t(covmat))) > tol) {
    #     stop("covmat must be symmetric.")
    # }

    eig <- eigen(covmat, symmetric = TRUE)
    if (any(eig$values < -tol)) {
        stop("covmat must be positive semidefinite; negative eigenvalues found.")
    }

    # clamp tiny negative values caused by roundoff
    lam <- pmax(eig$values, 0)

    angles <- seq(0, 2 * pi, length.out = n)

    # Eigen decomposition
    eig <- eigen(covmat)
    Q <- eig$vectors
    L <- diag(sqrt(eig$values * c))

    # Unit circle
    circle <- rbind(cos(angles), sin(angles))

    # Transform
    ellipse <- t(Q %*% L %*% circle)
    colnames(ellipse) <- c("x", "y")
    # Shift by mean
    ellipse <- sweep(ellipse, 2, mu, "+")

    return(ellipse)
}



#' Title
#'
#' @param covmat
#' @param theta
#'
#' @returns
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mapping = aes(x,y)) +
#'     # 90°
#'     geom_point(data = ellipse_points_2d(mu = c(1,1), covmat = rotate_cov_2d(c(4,0,0,1), pi/2)), color = "firebrick") +
#'     # 45°
#'     geom_point(data = ellipse_points_2d(mu = c(1,1), covmat = rotate_cov_2d(c(4,0,0,1), pi/4)), color = "forestgreen") +
#'     geom_point(data = ellipse_points_2d(mu = c(1,1), covmat = c(4,0,0,1)), color = "cornflowerblue") +
#'     coord_equal()
rotate_cov_2d <- function(covmat = c(1,0, 0,1),
                       theta = pi*0) {
    if (!is.matrix(covmat)) {
        covmat <- matrix(covmat, nrow = 2)
    }
    R <- matrix(c(
        cos(theta), -sin(theta),
        sin(theta),  cos(theta)
    ), nrow = 2, byrow = TRUE)

    covmat2 <- R %*% covmat %*% t(R)
    return(covmat2)
}


#' Title
#'
#' @param mu
#' @param covmat
#' @param n_theta
#' @param n_phi
#' @param c
#'
#' @returns
#' @export
#'
#' @examples
#' brathering::plot3d(ellipsoid_points_3d())
#' brathering::plot3d(ellipsoid_points_3d(covmat = c(5,0,0, 0,1,0, 0,0,1)))
#' brathering::plot3d(ellipsoid_points_3d_ran())
#'
#'
#' brathering::plot3d(ellipsoid_points_3d(covmat = rotate_cov_3d(c(5,0,0, 0,1,0, 0,0,1), Rz = 0)))
#' # does it work?
#' brathering::plot3d(ellipsoid_points_3d(covmat = rotate_cov_3d(c(5,0,0, 0,1,0, 0,0,1), Rz = pi)))

ellipsoid_points_3d <- function(mu = c(0,0,0),
                                covmat = c(1,0,0, 0,1,0, 0,0,1),
                                n_theta = 50,
                                n_phi = 25,
                                c = 1) {
    if (!is.matrix(covmat)) {
        covmat <- matrix(covmat, nrow = 3)
    }
    eig <- eigen(covmat)
    Q <- eig$vectors
    A <- Q %*% diag(sqrt(eig$values * c))

    theta <- seq(0, 2*pi, length.out = n_theta)
    phi   <- seq(0, pi, length.out = n_phi)

    grid <- expand.grid(theta = theta, phi = phi)

    # unit sphere points
    u <- rbind(
        cos(grid$theta) * sin(grid$phi),
        sin(grid$theta) * sin(grid$phi),
        cos(grid$phi)
    )

    # transform sphere -> ellipsoid
    pts <- t(A %*% u)
    pts <- sweep(pts, 2, mu, "+")

    colnames(pts) <- c("x", "y", "z")
    pts
}

#' Title
#'
#' @param mu
#' @param covmat
#' @param n
#' @param c
#'
#' @returns
#' @export
#'
#' @examples
ellipsoid_points_3d_ran <- function(
        mu = c(0,0,0),
        covmat = c(1,0,0, 0,1,0, 0,0,1),
        n = 1000,
        c = 1
) {
    if (!is.matrix(covmat)) {
        covmat <- matrix(covmat, nrow = 3)
    }
    eig <- eigen(covmat)
    Q <- eig$vectors
    A <- Q %*% diag(sqrt(eig$values * c))

    U <- matrix(rnorm(3 * n), nrow = 3)
    U <- sweep(U, 2, sqrt(colSums(U^2)), "/")  # normalize columns

    pts <- t(A %*% U)
    pts <- sweep(pts, 2, mu, "+")

    colnames(pts) <- c("x", "y", "z")
    pts
}




#' Title
#'
#' @param covmat
#' @param Rx
#' @param Ry
#' @param Rz
#'
#' @returns
#' @export
#'
#' @examples
#' rotate_cov_3d()
rotate_cov_3d <- function(covmat = c(1,0,0, 0,1,0, 0,0,1),
                          Rx = 0*pi,
                          Ry = 0*pi,
                          Rz = 0*pi) {
    if (!is.matrix(covmat)) {
        covmat <- matrix(covmat, nrow = 3)
    }
    rot_x <- function(theta) {
        matrix(c(
            1, 0, 0,
            0, cos(theta), -sin(theta),
            0, sin(theta),  cos(theta)
        ), 3, 3, byrow = TRUE)
    }

    rot_y <- function(theta) {
        matrix(c(
            cos(theta), 0, sin(theta),
            0, 1, 0,
            -sin(theta), 0, cos(theta)
        ), 3, 3, byrow = TRUE)
    }

    rot_z <- function(theta) {
        matrix(c(
            cos(theta), -sin(theta), 0,
            sin(theta),  cos(theta), 0,
            0, 0, 1
        ), 3, 3, byrow = TRUE)
    }
    R <- rot_z(Rz) %*% rot_y(Ry) %*% rot_x(Rx)
    covmat_rot <- R %*% covmat %*% t(R)
    return(covmat_rot)
}



#' Get rotation matrix for nD
#'
#' Rotates the plane of 2 dimensions
#'
#' @param n total dimensions
#' @param i dim1 to rotate
#' @param j dim2 to rotate
#' @param theta angle in radians
#'
#' @returns
#' @export
#'
#' @examples
#' # Combine multiple rotations
#' R <- diag(4)
#' R <- givens_rotation(4, 1, 2, pi/6) %*% R
#' R <- givens_rotation(4, 2, 3, pi/4) %*% R
#' R <- givens_rotation(4, 3, 4, pi/3) %*% R
givens_rotation <- function(n, i, j, theta) {
    R <- diag(n)

    R[i, i] <- cos(theta)
    R[j, j] <- cos(theta)
    R[i, j] <- -sin(theta)
    R[j, i] <-  sin(theta)

    R
}


#' Random rotation matrix in nD
#'
#' @param n
#'
#' @returns
#' @export
#'
#' @examples
#' random_rotation(2)
#' hdos:::is_rotation_matrix(random_rotation(2))
#' random_rotation(4)
#' hdos:::is_rotation_matrix(random_rotation(4))
#' ggplot(ellipse_points_2d(covmat = c(5,0,0,1)), aes(x,y)) +
#'     geom_point() +
#'     coord_equal()
#'
#' ggplot(ellipse_points_2d(covmat = matrix(c(5,0,0,1), nrow = 2) %*% random_rotation(2)), aes(x,y)) +
#'     geom_point() +
#'     coord_equal()
random_rotation <- function(n) {
    A <- matrix(rnorm(n^2), n, n)
    Q <- qr.Q(qr(A))

    # ensure det = +1 (proper rotation)
    if (det(Q) < 0) Q[,1] <- -Q[,1]

    Q
}



