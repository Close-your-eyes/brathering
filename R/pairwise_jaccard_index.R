#' Pairwise jaccard indices
#'
#' @param x named list of vectors
#'
#' @return list of jaccard index formats and plot
#' @export
#'
#' @examples
#' a <- letters[1:5]
#' b <- letters[1:7]
#' c <- letters[5:10]
#' d <- letters[2:8]
#' x <- list(a=a,b=b,c=c,d=d)
#' jacdata <- pairwise_jaccard_index(x)
pairwise_jaccard_index <- function(x) {

    stopifnot("x must be a list" = is.list(x),
              "x needs names" = !is.null(names(x)))


    jacinds <- purrr::map(x, function(x1) {
        purrr::map(x, function(x2) {
            jaccard_index(x1, x2)
        })
    })

    jacinds2 <- purrr::map_dfr(stats::setNames(names(jacinds), names(jacinds)),
                               ~utils::stack(jacinds[[.x]]),
                               .id = "ind2")
    jacinds2 <- jacinds2[,c("ind", "ind2", "values")]
    names(jacinds2) <- c("xind", "yind", "jaccard_index")

    jacinds3 <-
        jacinds2 |>
        tidyr::pivot_wider(names_from = xind, values_from = jaccard_index) |>
        tibble::column_to_rownames("yind") |>
        as.matrix()

    jacinds4 <- jacinds3[nrow(jacinds3):1, ]

    jacplot <-
        ggplot2::ggplot(jacinds2, ggplot2::aes(x = xind, y = yind, fill = jaccard_index)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::theme(legend.title = ggplot2::element_text())


    jacplot2 <-
        ggplot2::ggplot(jacinds2, ggplot2::aes(x = xind, y = yind, fill = jaccard_index)) +
        ggplot2::geom_tile() +
        ggplot2::theme(legend.title = ggplot2::element_text()) +
        ggplot2::scale_fill_stepsn(colors = scales::viridis_pal()(10),
                                   breaks = seq(0,1,0.2),
                                   values = scales::rescale(seq(0, 1, length.out = 5)),
                                   show.limits = T) +
        ggplot2::guides(fill = ggplot2::guide_colorsteps())

    return(list(jacind_list = jacinds,
                jacind_df = jacinds2,
                jacind_mat = jacinds3,
                jacind_mat2 = jacinds4,
                plot = jacplot))
}

if(base::getRversion() >= "2.15.1")  utils::globalVariables(c("xind", "yind"))
