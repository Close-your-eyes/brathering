#' Create venn diagramm from long data frame
#'
#' @param data data frame or tibble
#' @param obs_col name of column with observations
#' @param cat_col name of column with categories
#' @param limma_plot return a plot from limma library instead of ggplot
#' (required of number of categories > 4)
#' @param flip flip x and y axes of ggplot
#' @param rm_non_intersect remove elements that do not intersect with any
#' category
#' @param make_obs_by_cat_unique make same observations per group unique
#' so that they are not collapsed into a single observation by limma
#'
#' @return list of processed data frames and plot (limma or ggplot)
#' @export
#'
#' @examples
#' # 2-way venn
#' cat <- sample(c("A","B"), 100, replace = TRUE)
#' obs <- sample(letters[5:15], 100, replace = TRUE)
#' df <- data.frame(cat = cat, obs = obs)
#' out <- venn_plot(df, obs_col = "obs", cat_col = "cat",
#'                  rm_non_intersect = FALSE, limma_plot = FALSE)
#' out[["plot"]]
#' # 3-way venn
#' cat <- sample(c("A","B","C"), 100, replace = TRUE)
#' obs <- sample(letters[5:15], 100, replace = TRUE)
#' df <- data.frame(cat = cat, obs = obs)
#' out <- venn_plot(df, obs_col = "obs", cat_col = "cat",
#'                  rm_non_intersect = FALSE, limma_plot = FALSE)
#' out[["plot"]]
#' # 4-way venn
#' cat <- sample(c("A","B","C","D"), 100, replace = TRUE)
#' obs <- sample(letters[5:15], 100, replace = TRUE)
#' df <- data.frame(cat = cat, obs = obs)
#' out <- venn_plot(df, obs_col = "obs", cat_col = "cat",
#'                  rm_non_intersect = FALSE, limma_plot = FALSE)
#' out[["plot"]]
#' # 5-way venn only with limma
#' cat <- sample(c("A","B","C","D","E"), 100, replace = TRUE)
#' obs <- sample(letters[5:15], 100, replace = TRUE)
#' df <- data.frame(cat = cat, obs = obs)
#' out <- venn_plot(df, obs_col = "obs", cat_col = "cat",
#'                  rm_non_intersect = F, limma_plot = TRUE)
#' out[["plot"]]
venn_plot <- function(data,
                      obs_col = "obs",
                      cat_col = "cat",
                      make_obs_by_cat_unique = TRUE,
                      limma_plot = FALSE,
                      flip = FALSE,
                      rm_non_intersect = TRUE) {
    if (!requireNamespace("BiocManager", quietly = T)) {
        utils::install.packages("BiocManager")
    }
    if (!requireNamespace("limma", quietly = T)) {
        BiocManager::install("limma")
    }
    if (missing(data)) {
        stop("Please provide a data frame or tibble as data.")
    }
    if (is.null(obs_col) || !obs_col %in% names(data)) {
        stop("obs_col not found.")
    }
    if (is.null(cat_col) || !cat_col %in% names(data)) {
        stop("cat_col not found.")
    }
    data <- data[,c(cat_col, obs_col)]
    data <- as.data.frame(data)

    if (length(unique(data[,cat_col])) == 1) {
        stop("only one category found.")
    }
    if (length(unique(data[,cat_col])) > 4 && !limma_plot) {
        message("More than 4 categories can only be plotted with limma. Setting limma_plot to TRUE.")
        limma_plot <- T
    }

    # make_obs_by_cat_unique
    ## data as list of data frame - list TODO
    long_summ <-
        data |>
        dplyr::group_by(!!rlang::sym(obs_col), !!rlang::sym(cat_col)) |>
        dplyr::tally() |>
        dplyr::ungroup()

    wide_summ <-
        long_summ |>
        tidyr::pivot_wider(names_from = !!rlang::sym(cat_col), values_from = n)

    if (any(long_summ$n > 1)) {
        message("There are duplicate observations per category.
        These are made unique (and hence kept) when make_obs_by_cat_unique = T
                or are collapsed to a single observation when make_obs_by_cat_unique = F.")
        print(wide_summ)
    }

    if (make_obs_by_cat_unique) {
        sep <- find_sep(unique(c(data[[cat_col]], data[[obs_col]])))
        data[,obs_col] <- sapply(strsplit(make.unique(paste(data[[cat_col]], data[[obs_col]], sep = sep)), sep), "[", 2)
    } else {
        rows1 <- nrow(data)
        # factors are not lost by this line
        data <- unique(data)
        rows2 <- nrow(data)
        if (rows2 < rows1) {
            message("making data unique reduced the number of observations from ", rows1, " to ", rows2, ".")
        }
    }

    # make factor cols
    data[,which(!sapply(data, is.factor))] <- lapply(data[,which(!sapply(data, is.factor)),drop=F], as.factor)

    # make matrix for limma
    data_vc <- as.matrix(do.call(rbind, lapply(levels(data[,obs_col]), function(x) levels(data[,cat_col]) %in% unique(data[which(data[,obs_col] == x), cat_col]))))
    rownames(data_vc) <- levels(data[,obs_col])
    colnames(data_vc) <- levels(data[,cat_col])
    vc <- limma::vennCounts(data_vc)

    vc_df <- as.data.frame(matrix(vc, ncol = ncol(vc)))
    names(vc_df) <- colnames(vc)
    vc_df$Counts_rel_total <- vc_df$Counts/nlevels(data[,obs_col])
    vc_df$label_total <- paste0(vc_df$Counts, " (", round(vc_df$Counts_rel_total*100, 0), " %)")
    vc_df$label_total <- ifelse(vc_df$Counts == 0, 0, vc_df$label_total)

    for (i in levels(data[,cat_col])) {
        vc_df[,paste0("Counts_rel_", i)] <- vc_df$Counts/sum(vc_df[which(vc_df[,which(names(vc_df) == i)] == 1),"Counts"])
        vc_df[which(vc_df[,which(names(vc_df) == i)] != 1),paste0("Counts_rel_", i)] <- NA
        vc_df[,paste0("label_", i)] <- ifelse(is.na(vc_df[,paste0("Counts_rel_", i)]), vc_df$Counts, paste0(vc_df$Counts, " (", round(vc_df[,paste0("Counts_rel_", i)]*100, 0), " %)"))
    }

    if (limma_plot) {
        return(list(data = vc_df,
                    plot = limma::vennDiagram(vc, circle.col = scales::hue_pal()(ncol(wide_summ))))) # c("#ff0000", "#00ff00",  "#0000ff", "#ffff00")
    } else {

        if (flip) {
            xx <- "y"
            yy <- "x"
        } else {
            xx <- "x"
            yy <- "y"
        }


        ## plot circles
        if (nlevels(data[,cat_col]) == 4) {
            ## 4 groups: ellipses required
            venn.circ <- data.frame(x = c(0, 2, 0, -2),
                                    y = c(1, 0, 1, 0),
                                    angle = c(pi / 4, pi / 4, -pi / 4, -pi / 4),
                                    labels = levels(data[,cat_col]))
            #g <- ggplot(venn.circ) + ggforce::geom_ellipse(aes(x0 = x, y0 = y, a = 4.5, b = 2.5, angle = angle, fill = labels), alpha = 0.3)

            if ("0" %in% names(vc_df)) {
                stop("'0' cannot be a gene set name.")
            }
            vc_df$labels <- paste0(gsub("^0$", "", gsub("1", names(vc_df)[1], vc_df[,1])),
                                   gsub("^0$", "", gsub("1", names(vc_df)[2], vc_df[,2])),
                                   gsub("^0$", "", gsub("1", names(vc_df)[3], vc_df[,3])),
                                   gsub("^0$", "", gsub("1", names(vc_df)[4], vc_df[,4])))
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
            row2 <- Reduce(intersect, list(which(vc_df[,cols1[1]] == 1),
                                           which(vc_df[,cols1[2]] == 1),
                                           which(vc_df[,cols0[1]] == 0),
                                           which(vc_df[,cols0[2]] == 0)))
            vc_df[row2,"x"] <- 0
            vc_df[row2,"y"] <- 2.5

            # intersecting labels - two way overlap (2)
            row_inds <- Reduce(intersect, list(which(abs(vc_df$x) == 4.5),which(vc_df$y == 2)))
            rows <- vc_df[row_inds,c(1:4)]
            # find columns with ones
            cols1 <- which(sapply(rows, function(x) any(x == 1)))
            cols0 <- which(sapply(rows, function(x) !any(x == 1)))
            row2 <- Reduce(intersect, list(which(vc_df[,cols1[1]] == 1),
                                           which(vc_df[,cols1[2]] == 1),
                                           which(vc_df[,cols0[1]] == 0),
                                           which(vc_df[,cols0[2]] == 0)))
            vc_df[row2,"x"] <- 0
            vc_df[row2,"y"] <- -3

            # intersecting labels - two way overlap (3)
            row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(2, 4.5)),which(vc_df$y %in% c(2,4))))
            rows <- vc_df[row_inds,c(1:4)]
            # find columns with ones
            cols1 <- which(sapply(rows, function(x) any(x == 1)))
            cols0 <- which(sapply(rows, function(x) !any(x == 1)))
            row2 <- Reduce(intersect, list(which(vc_df[,cols1[1]] == 1),
                                           which(vc_df[,cols1[2]] == 1),
                                           which(vc_df[,cols0[1]] == 0),
                                           which(vc_df[,cols0[2]] == 0)))
            vc_df[row2,"x"] <- 3
            vc_df[row2,"y"] <- 2.5

            # intersecting labels - two way overlap (4)
            row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(-2, -4.5)),which(vc_df$y %in% c(2,4))))
            rows <- vc_df[row_inds,c(1:4)]
            # find columns with ones
            cols1 <- which(sapply(rows, function(x) any(x == 1)))
            cols0 <- which(sapply(rows, function(x) !any(x == 1)))
            row2 <- Reduce(intersect, list(which(vc_df[,cols1[1]] == 1),
                                           which(vc_df[,cols1[2]] == 1),
                                           which(vc_df[,cols0[1]] == 0),
                                           which(vc_df[,cols0[2]] == 0)))
            vc_df[row2,"x"] <- -3
            vc_df[row2,"y"] <- 2.5

            # intersecting labels - two way overlap (5)
            row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(2, -4.5)),which(vc_df$y %in% c(2,4))))
            rows <- vc_df[row_inds,c(1:4)]
            # find columns with ones
            cols1 <- which(sapply(rows, function(x) any(x == 1)))
            cols0 <- which(sapply(rows, function(x) !any(x == 1)))
            row2 <- Reduce(intersect, list(which(vc_df[,cols1[1]] == 1),
                                           which(vc_df[,cols1[2]] == 1),
                                           which(vc_df[,cols0[1]] == 0),
                                           which(vc_df[,cols0[2]] == 0)))
            vc_df[row2,"x"] <- -2.5
            vc_df[row2,"y"] <- -1.25

            # intersecting labels - two way overlap (6)
            row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(-2, 4.5)),which(vc_df$y %in% c(2,4))))
            rows <- vc_df[row_inds,c(1:4)]
            # find columns with ones
            cols1 <- which(sapply(rows, function(x) any(x == 1)))
            cols0 <- which(sapply(rows, function(x) !any(x == 1)))
            row2 <- Reduce(intersect, list(which(vc_df[,cols1[1]] == 1),
                                           which(vc_df[,cols1[2]] == 1),
                                           which(vc_df[,cols0[1]] == 0),
                                           which(vc_df[,cols0[2]] == 0)))
            vc_df[row2,"x"] <- 2.5
            vc_df[row2,"y"] <- -1.25

            # intersecting labels - three way overlap (1)
            row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(-2, 2, 4.5)),which(vc_df$y %in% c(2,4))))
            rows <- vc_df[row_inds,c(1:4)]
            # find columns with ones
            cols1 <- which(sapply(rows, function(x) any(x == 1)))
            cols0 <- which(sapply(rows, function(x) !any(x == 1)))
            row2 <- Reduce(intersect, list(which(vc_df[,cols1[1]] == 1),
                                           which(vc_df[,cols1[2]] == 1),
                                           which(vc_df[,cols1[3]] == 1),
                                           which(vc_df[,cols0[1]] == 0)))
            vc_df[row2,"x"] <- 1.5
            vc_df[row2,"y"] <- 1.25

            # intersecting labels - three way overlap (2)
            row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(-2, 2, -4.5)),which(vc_df$y %in% c(2,4))))
            rows <- vc_df[row_inds,c(1:4)]
            # find columns with ones
            cols1 <- which(sapply(rows, function(x) any(x == 1)))
            cols0 <- which(sapply(rows, function(x) !any(x == 1)))
            row2 <- Reduce(intersect, list(which(vc_df[,cols1[1]] == 1),
                                           which(vc_df[,cols1[2]] == 1),
                                           which(vc_df[,cols1[3]] == 1),
                                           which(vc_df[,cols0[1]] == 0)))
            vc_df[row2,"x"] <- -1.5
            vc_df[row2,"y"] <- 1.25

            # intersecting labels - three way overlap (3)
            row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(2, 4.5, -4.5)),which(vc_df$y %in% c(2,4))))
            rows <- vc_df[row_inds,c(1:4)]
            # find columns with ones
            cols1 <- which(sapply(rows, function(x) any(x == 1)))
            cols0 <- which(sapply(rows, function(x) !any(x == 1)))
            row2 <- Reduce(intersect, list(which(vc_df[,cols1[1]] == 1),
                                           which(vc_df[,cols1[2]] == 1),
                                           which(vc_df[,cols1[3]] == 1),
                                           which(vc_df[,cols0[1]] == 0)))
            vc_df[row2,"x"] <- -1
            vc_df[row2,"y"] <- -2

            # intersecting labels - three way overlap (4)
            row_inds <- Reduce(intersect, list(which(vc_df$x %in% c(-2, 4.5, -4.5)),which(vc_df$y %in% c(2,4))))
            rows <- vc_df[row_inds,c(1:4)]
            # find columns with ones
            cols1 <- which(sapply(rows, function(x) any(x == 1)))
            cols0 <- which(sapply(rows, function(x) !any(x == 1)))
            row2 <- Reduce(intersect, list(which(vc_df[,cols1[1]] == 1),
                                           which(vc_df[,cols1[2]] == 1),
                                           which(vc_df[,cols1[3]] == 1),
                                           which(vc_df[,cols0[1]] == 0)))
            vc_df[row2,"x"] <- 1
            vc_df[row2,"y"] <- -2

            if (rm_non_intersect) {
                vc_df <- vc_df[-Reduce(intersect, list(which(vc_df[,1] == 0),which(vc_df[,2] == 0),which(vc_df[,3] == 0),which(vc_df[,4] == 0))),]
            }

            g <- ggplot2::ggplot(venn.circ) + ggforce::geom_ellipse(ggplot2::aes(x0 = x, y0 = y, a = 4.5, b = 2.5, angle = angle, fill = labels), alpha = 0.3)
            #g + geom_text(aes(label = Counts, x = !!rlang::sym(xx), y = !!rlang::sym(yy)), data = vc_df, inherit.aes = F, size = 5)
        }

        if (nlevels(data[,cat_col]) == 3) {
            venn.circ <- data.frame(x = c(0, 0.866, -0.866),
                                    y = c(1, -0.5, -0.5),
                                    labels = levels(data[,cat_col]))

            ## use ^0$ to avoid that 0 in names(vc_df) are replaced
            ## for 1 it is not necessary to add ^ and $, even though it would not hurt
            ## 0 and 1 are the factor levels from limma which are replaced and which are used to concat names for venn regions
            if ("0" %in% names(vc_df)) {
                stop("'0' cannot be a gene set name.")
            }
            vc_df$labels <- paste0(gsub("^0$", "", gsub("1", names(vc_df)[1], vc_df[,1])),
                                   gsub("^0$", "", gsub("1", names(vc_df)[2], vc_df[,2])),
                                   gsub("^0$", "", gsub("1", names(vc_df)[3], vc_df[,3])))
            vc_df <- dplyr::left_join(vc_df, venn.circ, by = "labels")

            # outside label
            vc_df[which(vc_df$labels == ""), c("x","y")] <- c(2.1,-1.9)

            # center label
            vc_df[Reduce(intersect, list(which(vc_df[,1] == "1"),
                                         which(vc_df[,2] == "1"),
                                         which(vc_df[,3] == "1"))), c("x","y")] <- c(0,0)

            # unique labels
            for (i in 1:3) {
                vc_df[which(vc_df$labels == levels(data[,cat_col])[i]), "x"] <- vc_df[which(vc_df$labels == levels(data[,cat_col])[i]), "x"]*1.8
                vc_df[which(vc_df$labels == levels(data[,cat_col])[i]), "y"] <- vc_df[which(vc_df$labels == levels(data[,cat_col])[i]), "y"]*1.8
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

            if (rm_non_intersect) {
                vc_df <- vc_df[-Reduce(intersect, list(which(vc_df[,1] == 0),which(vc_df[,2] == 0),which(vc_df[,3] == 0))),]
            }

            g <- ggplot2::ggplot(venn.circ) + ggforce::geom_circle(ggplot2::aes(x0 = !!rlang::sym(xx), y0 = !!rlang::sym(yy), r = 1.4, fill = labels), alpha = 0.3, size = 1)
        }

        if (nlevels(data[,cat_col]) == 2) {

            venn.circ <- data.frame(x = c(-0.5, +0.5),
                                    y = c(+0.00, +0.00),
                                    labels = levels(data[,cat_col]))
            vc_df$labels <- paste0(gsub("0", "", gsub("1", names(vc_df)[1], vc_df[,1])),
                                   gsub("0", "", gsub("1", names(vc_df)[2], vc_df[,2])))
            vc_df <- dplyr::left_join(vc_df, venn.circ, by = "labels")

            ## set label positions
            vc_df[which(vc_df$labels == levels(data[,cat_col])[1]), "x"] <- vc_df[which(vc_df$labels == levels(data[,cat_col])[1]), "x"]*2
            vc_df[which(vc_df$labels == levels(data[,cat_col])[2]), "x"] <- vc_df[which(vc_df$labels == levels(data[,cat_col])[2]), "x"]*2
            vc_df[which(vc_df$labels == ""), c("x","y")] <- c(1.4,-0.9)
            vc_df[which(vc_df$labels == paste(levels(data[,cat_col]), collapse = "")), c("x","y")] <- c(0,0)

            g <-
                ggplot2::ggplot(venn.circ) +
                ggforce::geom_circle(ggplot2::aes(x0 = !!rlang::sym(xx),
                                                  y0 = !!rlang::sym(yy),
                                                  r = 1,
                                                  fill = labels), alpha = 0.3, linewidth = 1)

            if (rm_non_intersect) {
                vc_df <- vc_df[-Reduce(intersect, list(which(vc_df[,1] == 0),which(vc_df[,2] == 0))),]
            }
        }

        g <-
            g +
            ggplot2::coord_equal() +
            #theme_void() +
            ggplot2::theme(legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = 14), legend.position = "bottom") +
            ggplot2::geom_text(ggplot2::aes(label = Counts, x = !!rlang::sym(xx), y = !!rlang::sym(yy)), data = vc_df, inherit.aes = F, size = 5)

        return(list(data = vc_df, data_circ = venn.circ, plot = g))
    }
}

if(base::getRversion() >= "2.15.1")  utils::globalVariables(c("x", "y", "angle", "Counts", "n"))
