#' Title
#'
#' @returns
#' @export
#'
#' @examples
correlation_demo1 <- function() {


    library(ggplot2)

    ## ratio of sd and diff
    combs <- expand.grid(n = c(10,50,100,500,1000,5000,1e4,5e4,1e5,5e5,1e6,1e7),
                         diff = c(1e-4,1e-3,1e-2,1e-1,1))

    ## ---- two populations - corr ----
    corrs <- parallel::mclapply(c(1e-4,1e-3,1e-2,1e-1,1), function(sd) {

        out <- purrr::map(asplit(combs, 1), function(z) {
            rbind(data.frame(x=rnorm(z[1], mean = 0, sd = 1), y=rnorm(z[1], mean = 0, sd = sd)),
                  data.frame(x=rnorm(z[1], mean = 10, sd = 1), y=rnorm(z[1], mean = z[2], sd = sd))) |>
                dplyr::mutate(n=z[1], diff = z[2])
        })
        corrs <- purrr::map_dfr(out, function(z) {

            test <- cor.test(z$x, z$y)
            z |>
                dplyr::distinct(n, diff) |>
                dplyr::mutate(p = test$p.value)
        }) |>
            dplyr::mutate(sd = sd)
        #(p = ifelse(p == 0, 1e-120, p),

        return(corrs)
    }, mc.cores = 6)
    corrs <- dplyr::bind_rows(corrs)

    p1 <- ggplot(corrs, aes(factor(n),factor(diff))) +
        geom_tile(aes(fill = -log10(p))) +
        colrr::scale_fill_spectral() +
        theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
        labs(title = "facet: sd in y-direction.") +
        facet_wrap(vars(sd))

    print(p1)

    # example
    sd <- 0.1
    n <- 100
    diff <- 0.1
    exmpl <- rbind(data.frame(x=rnorm(n = n, mean = 0, sd = 1), y=rnorm(n = n, mean = 0, sd = sd)),
                   data.frame(x=rnorm(n = n, mean = 10, sd = 1), y=rnorm(n = n, mean = diff, sd = sd)))
    p2 <- ggplot(exmpl, aes(x,y)) +
        geom_point() +
        labs(title = "example1") +
        geom_smooth(method = "lm")

    print(p2)

    message("When slope (strength of linear relationship) becomes low, one need more n to obtain a low p.")
    message("However, a large sd (e.g. 1) cannot be compensated by a massive n (e.g. 1e7). No signifcant p in many cases.")
    message("Grey tiles: NA. p-value was 0, so -log(p) caused NA.")
    message("Example 1: two separate populations with diff in mean. Example 2: continuous population with varying slope.")

    message("What is tested to derive p:")
    message("Null hypothesis (H0): H0:rho=0
    That is, the true (population) Pearson correlation coefficient ρ between x and y is zero.
    Alternative hypothesis (H1): H1:rho≠0
    Given a sample correlation coefficient r computed from n paired observations, R computes a t-statistic as:
    t = r*sqrt((n-2)/(1-r^2))
    This statistic follows a Student’s t-distribution with n−2 degrees of freedom under the null hypothesis H0:rho=0
    The correlation coefficient r measures effect size. The p-value measures how unlikely it is to observe such an r (or more extreme) under the null hypothesis that rho = 0.
    Thus, with a large sample size n, even a small r can yield a large t (and small p-value), leading to statistical significance — even though the relationship is weak.")

    ## ---- continuous population - corr -----
    corrs2 <- parallel::mclapply(c(1e-4,1e-3,1e-2,1e-1,1), function(sd) {

        out <- purrr::map(asplit(combs, 1), function(z) {
            x <- rnorm(z[1], sd = sd)
            y <- z[2] * x + rnorm(z[1], sd = sd)
            data.frame(x = x,y = y) |> dplyr::mutate(n=z[1], diff = z[2])
        })
        corrs <- purrr::map_dfr(out, function(z) {

            test <- cor.test(z$x, z$y)
            z |>
                dplyr::distinct(n, diff) |>
                dplyr::mutate(p = test$p.value)
        }) |>
            dplyr::mutate(sd = sd)
        #(p = ifelse(p == 0, 1e-120, p),

        return(corrs)
    }, mc.cores = 6)
    corrs2 <- dplyr::bind_rows(corrs2)

    p3 <- ggplot(corrs2, aes(factor(n),factor(diff))) +
        geom_tile(aes(fill = -log10(p))) +
        colrr::scale_fill_spectral() +
        theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
        labs(title = "facet: sd in y-direction.") +
        facet_wrap(vars(sd))

    print(p3)

    # example
    sd <- 0.1
    n <- 100
    diff <- 1
    x <- rnorm(n = n, sd = sd)
    y <- diff * x + rnorm(n = n, sd = sd)
    exmpl2 <- data.frame(x = x, y = y)
    p4 <- ggplot(exmpl2, aes(x,y)) +
        geom_point() +
        labs(title = "example2") +
        geom_smooth(method = "lm")

    print(p4)

    return(list(p1,p2,p3,p4))
}
