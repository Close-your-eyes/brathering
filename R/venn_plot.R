#' Create a Venn or UpSet plot from long-format data
#'
#' Converts observation-category pairs into a binary membership matrix and
#' creates a ggplot-based Venn diagram, a \pkg{limma} Venn diagram, or an
#' UpSet plot.
#'
#' @param data A data frame or tibble in long format.
#' @param obs_col Character string naming the column containing observation
#'   identifiers.
#' @param cat_col Character string naming the column containing category
#'   identifiers.
#' @param limma_plot Logical. If \code{TRUE}, draw the diagram using
#'   \code{\link[limma]{vennDiagram}}. This is automatically enabled when
#'   exactly five categories are present, unless \code{upset_plot = TRUE}.
#' @param upset_plot Logical. If \code{TRUE}, create an UpSet plot using
#'   \code{\link[ComplexUpset]{upset}}. This is automatically enabled when
#'   six or more categories are present.
#' @param upset_order_intersects Character string or character vector specifying
#'   how intersections are ordered in an UpSet plot. Supported values include
#'   \code{"cardinality"}, \code{"degree"}, and \code{"ratio"}. Multiple
#'   criteria may be supplied, for example
#'   \code{c("degree", "cardinality")}.
#' @param upset_order_sets Either \code{FALSE}, \code{"ascending"}, or
#'   \code{"descending"}. Controls whether and how sets are ordered in an
#'   UpSet plot. If \code{FALSE}, the original set order is retained.
#' @param flip_axes Logical. If \code{TRUE}, exchange the x- and y-axes of the
#'   ggplot-based Venn diagram. Ignored when \code{limma_plot = TRUE} or
#'   \code{upset_plot = TRUE}.
#' @param ... Additional arguments passed to
#'   \code{\link[ComplexUpset]{upset}} when \code{upset_plot = TRUE}.
#'
#' @details
#' Each distinct observation-category pair represents membership of an
#' observation in a category. Duplicate pairs and rows containing missing
#' observation or category values are removed before membership counts are
#' calculated.
#'
#' The ggplot-based Venn diagram supports two to four categories. With five
#' categories, \code{limma_plot} is automatically enabled if it is not already
#' requested. With six or more categories, \code{upset_plot} is automatically
#' enabled and \code{limma_plot} is disabled.
#'
#' If \code{limma_plot = TRUE} and \code{upset_plot = TRUE} are both supplied
#' for five or fewer categories, the limma plot takes precedence.
#'
#' For UpSet plots, intersections are ordered according to
#' \code{upset_order_intersects}. The default, \code{"degree"}, orders them by
#' the number of sets participating in each intersection. To order by the
#' number of observations instead, use \code{"cardinality"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{data}}{
#'     For Venn diagrams, a data frame containing membership patterns, counts,
#'     relative counts, and formatted count labels. For UpSet plots, a data
#'     frame containing the binary observation-by-category membership matrix.
#'   }
#'   \item{\code{data_circ}}{
#'     A data frame describing the circles or ellipses used in the ggplot-based
#'     Venn diagram. It is \code{NULL} for limma and UpSet plots.
#'   }
#'   \item{\code{plot}}{
#'     For ggplot-based Venn and UpSet diagrams, the resulting plot object.
#'     For a limma diagram, the plot is drawn as a side effect and this element
#'     contains the value returned by \code{\link[limma]{vennDiagram}}.
#'   }
#' }
#'
#' @examples
#' # Two-category ggplot-based Venn diagram
#' dat2 <- data.frame(
#'     observation = c("a", "b", "b", "c"),
#'     category = c("A", "A", "B", "B")
#' )
#'
#' result2 <- venn_plot(
#'     dat2,
#'     obs_col = "observation",
#'     cat_col = "category"
#' )
#' result2$plot
#'
#' # Three-category Venn diagram with flipped axes
#' dat3 <- data.frame(
#'     observation = c("a", "b", "b", "c", "c", "d"),
#'     category = c("A", "A", "B", "B", "C", "C")
#' )
#'
#' result3 <- venn_plot(
#'     dat3,
#'     obs_col = "observation",
#'     cat_col = "category",
#'     flip_axes = TRUE
#' )
#' result3$plot
#'
#' # UpSet plot ordered by intersection size
#' dat_upset <- data.frame(
#'     observation = c(
#'         "a", "a", "b", "b", "c", "c",
#'         "d", "d", "e", "e", "f", "f"
#'     ),
#'     category = c(
#'         "A", "B", "A", "C", "B", "D",
#'         "C", "E", "D", "F", "A", "F"
#'     )
#' )
#'
#' result_upset <- venn_plot(
#'     dat_upset,
#'     obs_col = "observation",
#'     cat_col = "category",
#'     upset_plot = TRUE,
#'     upset_order_intersects = "cardinality",
#'     upset_order_sets = "descending"
#' )
#' result_upset$plot
#'
#' @export
#' @importFrom zeallot %<-%
venn_plot <- function(data,
                      obs_col = "obs",
                      cat_col = "cat",
                      limma_plot = FALSE,
                      upset_plot = FALSE,
                      upset_order_intersects = "degree",
                      upset_order_sets = F,
                      flip_axes = FALSE,
                      ...) {

    # if (!requireNamespace("BiocManager", quietly = T)) {
    #     utils::install.packages("BiocManager")
    # }
    # if (!requireNamespace("limma", quietly = T)) {
    #     BiocManager::install("limma")
    # }

    if (missing(data)) {
        stop("Please provide a data frame or tibble as data.")
    }
    if (is.null(obs_col) || !obs_col %in% names(data)) {
        stop("obs_col not found.")
    }
    if (is.null(cat_col) || !cat_col %in% names(data)) {
        stop("cat_col not found.")
    }

    data <- data |>
        dplyr::select(
            !!rlang::sym(cat_col),
            !!rlang::sym(obs_col)
        ) |>
        dplyr::distinct() |>
        tidyr::drop_na() |>
        droplevels() |>
        as.data.frame()

    ncat <- length(unique(data[[cat_col]]))
    if (ncat < 2) {
        stop("At least two categories are required.")
    }
    if (ncat > 4 && ncat < 6 && !limma_plot) {
        message("More than 4 categories can only be plotted with limma. Setting limma_plot to TRUE.")
        limma_plot <- TRUE
    }

    if (ncat > 5 && !upset_plot) {
        message("More than 5 categories can only be plotted with upset of tile plot. Setting upset_plot to TRUE.")
        limma_plot <- FALSE
        upset_plot <- TRUE
    }

    # make factor cols
    for (i in 1:ncol(data)) {
        data[,i] <- as.factor(data[,i])
    }
    # make matrix for limma
    data_vc <- table(data[[obs_col]], data[[cat_col]]) > 0
    vc <- limma::vennCounts(data_vc)

    vc_df <- as.data.frame(matrix(vc, ncol = ncol(vc)))
    names(vc_df) <- colnames(vc)
    vc_df[["Counts_rel_total"]] <- vc_df[["Counts"]]/nlevels(data[[obs_col]])
    vc_df[["label_total"]] <- paste0(vc_df[["Counts"]], " (", round(vc_df[["Counts_rel_total"]]*100, 0), " %)")
    vc_df[["label_total"]] <- ifelse(vc_df[["Counts"]] == 0, 0, vc_df[["label_total"]])

    for (i in levels(data[[cat_col]])) {
        colname <- paste0("Counts_rel_", i)
        vc_df[[colname]] <- vc_df[["Counts"]]/sum(vc_df[which(vc_df[[which(names(vc_df) == i)]] == 1),"Counts"])
        vc_df[which(vc_df[,which(names(vc_df) == i)] != 1),colname] <- NA
        vc_df[[paste0("label_", i)]] <- ifelse(is.na(vc_df[[colname]]), vc_df[["Counts"]], paste0(vc_df[["Counts"]], " (", round(vc_df[[colname]]*100, 0), " %)"))
    }

    if (limma_plot) {
        return(list(data = vc_df,
                    data_circ = NULL,
                    plot = limma::vennDiagram(vc,
                                              circle.col = scales::hue_pal()(ncat))))
    }

    if (upset_plot) {
        # simpler version: tile plot
        return(list(data = as.data.frame(data_vc),
                    data_circ = NULL,
                    plot = ComplexUpset::upset(
                        as.data.frame(data_vc),
                        intersect = colnames(data_vc),
                        sort_intersections_by = upset_order_intersects,
                        sort_sets = upset_order_sets,
                        ...)
        ))
    }

    c(data_circ, plot) %<-% make_venn_gg(
        data = data,
        vc_df = vc_df,
        cat_col = cat_col,
        flip_axes = flip_axes
    )

    return(list(data = vc_df, data_circ = data_circ, plot = plot))
}

make_venn_gg <- function(data,
                         vc_df,
                         cat_col,
                         flip_axes) {

    rm_outside <- F

    xx <- "x"
    yy <- "y"
    if (flip_axes) {
        xx <- "y"
        yy <- "x"
    }

    levs <- levels(data[[cat_col]])
    if ("0" %in% names(vc_df)) {
        stop("'0' cannot be a gene set name.")
    }
    sep <- brathering::find_sep(names(vc_df))

    ## plot circles
    if (length(levs) == 4) {
        ## 4 groups: ellipses required
        venn.circ <- data.frame(x = c(0, 2, 0, -2),
                                y = c(1, 0, 1, 0),
                                angle = c(pi / 4, pi / 4, -pi / 4, -pi / 4),
                                labels = levs)

        vc_df <- make_unique_labels(vc_df, length(levs), sep)
        vc_df <- dplyr::left_join(vc_df, venn.circ, by = "labels") # angle not needed; x and y column could be generate by other method as well


        # outside label
        vc_df[which(vc_df$labels == ""), c("x","y")] <- c(+5,-2.5)

        # center label
        vc_df[Reduce(intersect, list(which(vc_df[,1] == "1"),
                                     which(vc_df[,2] == "1"),
                                     which(vc_df[,3] == "1"),
                                     which(vc_df[,4] == "1"))), c("x","y")] <- c(0,-0.75)

        # unique labels
        vc_df[Reduce(intersect, list(which(vc_df$x == -2),which(vc_df$y == 0), which(vc_df$angle == -pi / 4))),c("x","y")] <- c(-4.5,2)
        vc_df[Reduce(intersect, list(which(vc_df$x == 0),which(vc_df$y == 1), which(vc_df$angle == -pi / 4))),c("x","y")] <- c(-2,4)
        vc_df[Reduce(intersect, list(which(vc_df$x == 0),which(vc_df$y == 1), which(vc_df$angle == pi / 4))),c("x","y")] <- c(2,4)
        vc_df[Reduce(intersect, list(which(vc_df$x == 2),which(vc_df$y == 0), which(vc_df$angle == pi / 4))),c("x","y")] <- c(4.5,2)

        # intersecting labels - two way overlap (1)
        row_inds <- Reduce(intersect, list(which(abs(vc_df$x) == 2),which(vc_df$y == 4)))
        rows <- vc_df[row_inds,c(1:4)]
        # find columns with ones
        cols1 <- which(sapply(rows, function(x) any(x == 1)))
        cols0 <- which(sapply(rows, function(x) !any(x == 1)))
        row2 <- Reduce(intersect, list(which(vc_df[[cols1[1]]] == 1),
                                       which(vc_df[[cols1[2]]] == 1),
                                       which(vc_df[[cols0[1]]] == 0),
                                       which(vc_df[[cols0[2]]] == 0)))
        vc_df[row2,"x"] <- 0
        vc_df[row2,"y"] <- 2.5

        # intersecting labels - two way overlap (2)
        row_inds <- Reduce(intersect, list(which(abs(vc_df$x) == 4.5),which(vc_df$y == 2)))
        rows <- vc_df[row_inds,c(1:4)]
        # find columns with ones
        cols1 <- which(sapply(rows, function(x) any(x == 1)))
        cols0 <- which(sapply(rows, function(x) !any(x == 1)))
        row2 <- Reduce(intersect, list(which(vc_df[[cols1[1]]] == 1),
                                       which(vc_df[[cols1[2]]] == 1),
                                       which(vc_df[[cols0[1]]] == 0),
                                       which(vc_df[[cols0[2]]] == 0)))
        vc_df[row2,"x"] <- 0
        vc_df[row2,"y"] <- -3

        # intersecting labels - two way overlap (3)
        row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(2, 4.5)),which(vc_df$y %in% c(2,4))))
        rows <- vc_df[row_inds,c(1:4)]
        # find columns with ones
        cols1 <- which(sapply(rows, function(x) any(x == 1)))
        cols0 <- which(sapply(rows, function(x) !any(x == 1)))
        row2 <- Reduce(intersect, list(which(vc_df[[cols1[1]]] == 1),
                                       which(vc_df[[cols1[2]]] == 1),
                                       which(vc_df[[cols0[1]]] == 0),
                                       which(vc_df[[cols0[2]]] == 0)))
        vc_df[row2,"x"] <- 3
        vc_df[row2,"y"] <- 2.5

        # intersecting labels - two way overlap (4)
        row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(-2, -4.5)),which(vc_df$y %in% c(2,4))))
        rows <- vc_df[row_inds,c(1:4)]
        # find columns with ones
        cols1 <- which(sapply(rows, function(x) any(x == 1)))
        cols0 <- which(sapply(rows, function(x) !any(x == 1)))
        row2 <- Reduce(intersect, list(which(vc_df[[cols1[1]]] == 1),
                                       which(vc_df[[cols1[2]]] == 1),
                                       which(vc_df[[cols0[1]]] == 0),
                                       which(vc_df[[cols0[2]]] == 0)))
        vc_df[row2,"x"] <- -3
        vc_df[row2,"y"] <- 2.5

        # intersecting labels - two way overlap (5)
        row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(2, -4.5)),which(vc_df$y %in% c(2,4))))
        rows <- vc_df[row_inds,c(1:4)]
        # find columns with ones
        cols1 <- which(sapply(rows, function(x) any(x == 1)))
        cols0 <- which(sapply(rows, function(x) !any(x == 1)))
        row2 <- Reduce(intersect, list(which(vc_df[[cols1[1]]] == 1),
                                       which(vc_df[[cols1[2]]] == 1),
                                       which(vc_df[[cols0[1]]] == 0),
                                       which(vc_df[[cols0[2]]] == 0)))
        vc_df[row2,"x"] <- -2.5
        vc_df[row2,"y"] <- -1.25

        # intersecting labels - two way overlap (6)
        row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(-2, 4.5)),which(vc_df$y %in% c(2,4))))
        rows <- vc_df[row_inds,c(1:4)]
        # find columns with ones
        cols1 <- which(sapply(rows, function(x) any(x == 1)))
        cols0 <- which(sapply(rows, function(x) !any(x == 1)))
        row2 <- Reduce(intersect, list(which(vc_df[[cols1[1]]] == 1),
                                       which(vc_df[[cols1[2]]] == 1),
                                       which(vc_df[[cols0[1]]] == 0),
                                       which(vc_df[[cols0[2]]] == 0)))
        vc_df[row2,"x"] <- 2.5
        vc_df[row2,"y"] <- -1.25

        # intersecting labels - three way overlap (1)
        row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(-2, 2, 4.5)),which(vc_df$y %in% c(2,4))))
        rows <- vc_df[row_inds,c(1:4)]
        # find columns with ones
        cols1 <- which(sapply(rows, function(x) any(x == 1)))
        cols0 <- which(sapply(rows, function(x) !any(x == 1)))
        row2 <- Reduce(intersect, list(which(vc_df[[cols1[1]]] == 1),
                                       which(vc_df[[cols1[2]]] == 1),
                                       which(vc_df[,cols1[3]] == 1),
                                       which(vc_df[[cols0[1]]] == 0)))
        vc_df[row2,"x"] <- 1.5
        vc_df[row2,"y"] <- 1.25

        # intersecting labels - three way overlap (2)
        row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(-2, 2, -4.5)),which(vc_df$y %in% c(2,4))))
        rows <- vc_df[row_inds,c(1:4)]
        # find columns with ones
        cols1 <- which(sapply(rows, function(x) any(x == 1)))
        cols0 <- which(sapply(rows, function(x) !any(x == 1)))
        row2 <- Reduce(intersect, list(which(vc_df[[cols1[1]]] == 1),
                                       which(vc_df[[cols1[2]]] == 1),
                                       which(vc_df[,cols1[3]] == 1),
                                       which(vc_df[[cols0[1]]] == 0)))
        vc_df[row2,"x"] <- -1.5
        vc_df[row2,"y"] <- 1.25

        # intersecting labels - three way overlap (3)
        row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(2, 4.5, -4.5)),which(vc_df$y %in% c(2,4))))
        rows <- vc_df[row_inds,c(1:4)]
        # find columns with ones
        cols1 <- which(sapply(rows, function(x) any(x == 1)))
        cols0 <- which(sapply(rows, function(x) !any(x == 1)))
        row2 <- Reduce(intersect, list(which(vc_df[[cols1[1]]] == 1),
                                       which(vc_df[[cols1[2]]] == 1),
                                       which(vc_df[,cols1[3]] == 1),
                                       which(vc_df[[cols0[1]]] == 0)))
        vc_df[row2,"x"] <- -1
        vc_df[row2,"y"] <- -2

        # intersecting labels - three way overlap (4)
        row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(-2, 4.5, -4.5)),which(vc_df$y %in% c(2,4))))
        rows <- vc_df[row_inds,c(1:4)]
        # find columns with ones
        cols1 <- which(sapply(rows, function(x) any(x == 1)))
        cols0 <- which(sapply(rows, function(x) !any(x == 1)))
        row2 <- Reduce(intersect, list(which(vc_df[[cols1[1]]] == 1),
                                       which(vc_df[[cols1[2]]] == 1),
                                       which(vc_df[,cols1[3]] == 1),
                                       which(vc_df[[cols0[1]]] == 0)))
        vc_df[row2,"x"] <- 1
        vc_df[row2,"y"] <- -2

        if (rm_outside) {
            vc_df <- vc_df[-Reduce(intersect, list(which(vc_df[,1] == 0),which(vc_df[,2] == 0),which(vc_df[,3] == 0),which(vc_df[,4] == 0))),]
        }

        g <- ggplot2::ggplot(venn.circ) + ggforce::geom_ellipse(ggplot2::aes(x0 = x, y0 = y, a = 4.5, b = 2.5, angle = angle, fill = labels), alpha = 0.3)
        #g + geom_text(aes(label = Counts, x = !!rlang::sym(xx), y = !!rlang::sym(yy)), data = vc_df, inherit.aes = F, size = 5)
    }

    if (length(levs) == 3) {
        venn.circ <- data.frame(x = c(0, 0.866, -0.866),
                                y = c(1, -0.5, -0.5),
                                labels = levs)
        vc_df <- make_unique_labels(vc_df, length(levs), sep)
        vc_df <- dplyr::left_join(vc_df, venn.circ, by = "labels")

        # outside label
        vc_df[which(vc_df$labels == ""), c("x","y")] <- c(2.1,-1.9)

        # center label
        vc_df[Reduce(intersect, list(which(vc_df[,1] == "1"),
                                     which(vc_df[,2] == "1"),
                                     which(vc_df[,3] == "1"))), c("x","y")] <- c(0,0)

        # unique labels
        for (i in 1:3) {
            vc_df[which(vc_df$labels == levs[i]), "x"] <- vc_df[which(vc_df$labels == levs[i]), "x"]*1.8
            vc_df[which(vc_df$labels == levs[i]), "y"] <- vc_df[which(vc_df$labels == levs[i]), "y"]*1.8
        }

        # 2-way overlapping; place in the middle between unqiue labels
        for (i in which(is.na(vc_df$x) & is.na(vc_df$y))) {
            c_cols <- which(vc_df[i,c(1,2,3)] == 1)
            ref_rows <- sapply(c_cols, function(j) {
                ot_cols <- c(1,2,3)[which(c(1,2,3) != j)]
                Reduce(intersect, list(which(vc_df[,j] == 1),
                                       which(vc_df[,ot_cols[1]] == 0),
                                       which(vc_df[,ot_cols[2]] == 0)))
            })

            vc_df[i,"x"] <- mean(vc_df[ref_rows, "x"])
            vc_df[i,"y"] <- mean(vc_df[ref_rows, "y"])
        }

        if (rm_outside) {
            vc_df <- vc_df[-Reduce(intersect, list(which(vc_df[,1] == 0),which(vc_df[,2] == 0),which(vc_df[,3] == 0))),]
        }

        g <- ggplot2::ggplot(venn.circ) + ggforce::geom_circle(ggplot2::aes(x0 = !!rlang::sym(xx), y0 = !!rlang::sym(yy), r = 1.4, fill = labels), alpha = 0.3, size = 1)
    }

    if (length(levs) == 2) {

        venn.circ <- data.frame(x = c(-0.5, +0.5),
                                y = c(+0.00, +0.00),
                                labels = levs)
        vc_df <- make_unique_labels(vc_df, length(levs), sep)
        vc_df <- dplyr::left_join(vc_df, venn.circ, by = "labels")

        ## set label positions
        vc_df[which(vc_df$labels == levs[1]), "x"] <- vc_df[which(vc_df$labels == levs[1]), "x"]*2
        vc_df[which(vc_df$labels == levs[2]), "x"] <- vc_df[which(vc_df$labels == levs[2]), "x"]*2
        vc_df[which(vc_df$labels == ""), c("x","y")] <- c(1.4,-0.9)
        vc_df[which(vc_df$labels == paste(levs, collapse = sep)), c("x","y")] <- c(0,0)

        g <- ggplot2::ggplot(venn.circ) +
            ggforce::geom_circle(ggplot2::aes(x0 = !!rlang::sym(xx),
                                              y0 = !!rlang::sym(yy),
                                              r = 1,
                                              fill = labels), alpha = 0.3, linewidth = 1)

        if (rm_outside) {
            vc_df <- vc_df[-Reduce(intersect, list(which(vc_df[,1] == 0),which(vc_df[,2] == 0))),]
        }
    }

    g <- g +
        ggplot2::coord_equal() +
        #theme_void() +
        ggplot2::theme(legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 14), legend.position = "bottom") +
        ggplot2::geom_text(ggplot2::aes(label = Counts, x = !!rlang::sym(xx), y = !!rlang::sym(yy)), data = vc_df, inherit.aes = F, size = 5)

    return(list(data_circ = venn.circ, plot = g))
}

make_unique_labels <- function(vc_df, nlevel, sep) {
    ## use ^0$ to avoid that 0 in names(vc_df) are replaced
    ## for 1 it is not necessary to add ^ and $, even though it would not hurt
    ## 0 and 1 are the factor levels from limma which are replaced and which are used to concat names for venn regions
    vc_df$labels <- do.call(paste, c(purrr::map(1:nlevel, ~gsub("^0$", "", gsub("1", names(vc_df)[.x], vc_df[,.x]))), sep = sep))
    vc_df$labels <- gsub(paste0("^",sep,"{1,}"), "", vc_df$labels)
    vc_df$labels <- gsub(paste0(sep,"{1,}$"), "", vc_df$labels)
    return(vc_df)
}
if(base::getRversion() >= "2.15.1")  utils::globalVariables(c("x", "y", "angle", "Counts", "n"))
