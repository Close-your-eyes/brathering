#' Generate random data with a fixed sample mean and standard deviation
#'
#' Draws values from a standard normal distribution, then rescales them to
#' have the requested sample mean and sample standard deviation.
#' `stats::rnorm()` specifies the mean and standard deviation of the population
#' you sample from. Your function forces the generated sample itself to have
#' those values.
#' Use rnorm() for ordinary statistical simulations where natural sampling
#' variation matters. Use rnorm_fixed() for demonstrations or controlled test
#' data requiring specific sample statistics.
#'
#' @param N Integer. Number of values to generate; must be at least 2.
#' @param mu Numeric scalar. Desired sample mean. Defaults to 0.
#' @param sd Non-negative numeric scalar. Desired sample standard deviation.
#'   Defaults to 1.
#'
#' @details
#' The sample mean and sample standard deviation match the requested values
#' up to floating-point rounding. Fixing these sample statistics makes the
#' generated values dependent; they are not independent normal draws.
#'
#' Input requirements are not explicitly validated.
#'
#' @returns A numeric vector.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm_fixed(100, mu = 10, sd = 2)
#' mean(x)
#' stats::sd(x)
rnorm_fixed <- function(N, mu = 0, sd = 1) {
    base::scale(stats::rnorm(N))[,1] * sd + mu
}
rnorm_fixed(10)
rnorm(10)
