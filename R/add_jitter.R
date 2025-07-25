#' Add jitter to any points that are overlapping at same xy-coordinates
#'
#' @param df data frame with points to hard-add jitter to
#' @param col1 column name of x-coord
#' @param col2 column name of y-coord
#' @param jitter_fun points_2d_rect or points_2d_circ
#' or any function that returns a matrix or data frame with columns
#' 1 and 2 containing cartesian coordinates
#' @param ... arguments to jitter_fun
#'
#' @return df with jittered coordinates
#' @export
#' @importFrom rlang :=
#'
#' @examples
#' df <- data.frame(x = round(rnorm(10)), y = round(rnorm(10)))
#' n <- 30  # Number of times to repeat
#' df2 <- do.call(rbind, replicate(n, df, simplify = FALSE))
#' ggplot2::ggplot(df2, ggplot2::aes(x,y)) + ggplot2::geom_point()
#' df3 <- add_jitter(df2)
#' ggplot2::ggplot(df3, ggplot2::aes(x,y)) + ggplot2::geom_point()
#' df4 <- add_jitter(df2, jitter_fun = points_2d_circ)
#' ggplot2::ggplot(df4, ggplot2::aes(x,y)) + ggplot2::geom_point()
add_jitter <- function(df,
                       col1 = "x",
                       col2 = "y",
                       jitter_fun = points_2d_rect,
                       ...) {

    jitter_fun <- match.fun(jitter_fun)
    gr <- find_sep(names(df), sep = "group")

    df <-
        df |>
        dplyr::group_by(!!rlang::sym(col1), !!rlang::sym(col2)) |>
        dplyr::mutate(!!gr := dplyr::cur_group_id()) |>
        dplyr::ungroup()
    df <- as.data.frame(df)

    for (i in unique(df[,gr])) {
        jitter <- jitter_fun(n = nrow(df[which(df[,gr] == i),]), ...)
        df[which(df[,gr] == i),col1] <- df[which(df[,gr] == i),col1] + jitter[,1]
        df[which(df[,gr] == i),col2] <- df[which(df[,gr] == i),col2] + jitter[,2]
    }
    df <- dplyr::select(df, -!!rlang::sym(gr))
    return(df)
}

if(base::getRversion() >= "2.15.1")  utils::globalVariables(c(":="))

