#' Make pie or donut chart
#'
#' @param x unnamed vector or named numeric vector (= already summarized)
#' @param order sort summarized/tabulated groups of x
#' @param fill color palette to fill pie pieces, can be named by groups in x
#' @param color border color of pie pieces
#' @param radius_inside inner radius where pieces are cut. outer radius is fixed
#' to 1. set to 0 to get pie chart
#' @param label_outside which labels outside of pie pieces (radius > 1): none,
#' absolute, relative
#' @param label_inside see label_outside, radius < 1
#' @param label_rel_cutoff min. fraction of pie piece to plot label
#' @param label_size size of label, passed to geom_text
#' @param label_radius_inside vector of radii where to place labels,
#' gets recycled
#' @param label_radius_outside see label_radius_inside
#' @param label_angle_inside vector of label angles, gets recycled
#' @param label_angle_outside see label_angle_inside
#' @param label_overlap method of trying to fix overlapping labels
#' @param overlap_outside_radius radius for label_overlap = "outside"
#' @param label_rel_pct relative labels in percent?
#' @param label_rel_dec decimal places of relative labels
#' @param legend_title legend title
#' @param theme_args arguments to ggplot2::theme()
#' @param fill_na
#' @param col_pal_args
#'
#' @returns list of plot and data frame
#' @export
#'
#' @importFrom rlang %||%
#'
#' @examples
#' piechart(x = c(rep("a",10), rep("b",6), rep("c", 3), rep("d",2)),
#'          label_rel_pct = T,
#'          label_rel_dec = 1)
#' piechart(x = setNames(1:10, letters[1:10]))
piechart <- function(x,
                     order = T,
                     fill = colrr::col_pal("custom"),
                     fill_na = "grey50",
                     color = "white",
                     radius_inside = 0.3,
                     label_outside = c("none", "abs", "rel"),
                     label_inside = c("rel", "abs", "none"),
                     label_rel_cutoff = 0,
                     label_size = 5,
                     label_radius_inside = 0.75,
                     label_radius_outside = 1.1,
                     label_angle_inside = NULL, # circle or numeric
                     label_angle_outside = NULL, # circle or numeric
                     label_overlap = c("ignore", "alternate", "outside"),
                     overlap_outside_radius = 1.1,
                     label_rel_pct = F,
                     label_rel_dec = 2,
                     legend_title = NULL,
                     theme_args = list(panel.grid = ggplot2::element_blank(),
                                       axis.title = ggplot2::element_blank(),
                                       axis.text = ggplot2::element_blank(),
                                       axis.ticks = ggplot2::element_blank()),
                     col_pal_args = list(missing_fct_to_na = T)) {

    ## geom_textpath for labels?

    if (!requireNamespace("ggforce", quietly = T)) {
        utils::install.packages("ggforce")
    }
    if (!requireNamespace("farver", quietly = T)) {
        utils::install.packages("farver")
    }

    if (!is.null(label_angle_inside)  && !is.numeric(label_angle_inside)) {
        stop("label_angle_inside has to be 'circle' or numeric.")
    }
    if (!is.null(label_angle_outside) && !is.numeric(label_angle_outside)) {
        stop("label_angle_outside has to be 'circle' or numeric.")
    }

    ## ... make that a list and check for default elements (see below)
    ## them call theme with Gmisc::fastDoCall

    label_overlap <- rlang::arg_match(label_overlap)
    label_outside <- rlang::arg_match(label_outside)
    label_inside <- rlang::arg_match(label_inside)


    tab <- make_pie_basis(x = x,
                          order = order)

    tab <- make_label_angles_and_radii(tab = tab,
                                       label_angle_inside = label_angle_inside,
                                       label_angle_outside = label_angle_inside,
                                       label_radius_outside = label_radius_outside,
                                       label_radius_inside = label_radius_inside,
                                       label_overlap = label_overlap)
    # adds group_cols
    # tab <- check_and_add_col_pal(tab = tab, col_pal = fill)
    col_pal <- colrr::make_col_pal(col_vec = fill,
                                   fct_lvls = tab$group,
                                   missing_fct_to_na = ifelse("missing_fct_to_na" %in% names(col_pal_args), col_pal_args[["missing_fct_to_na"]], T),
                                   col_pal_args = col_pal_args[-which(names(col_pal_args) %in% c("name", "missing_fct_to_na"))])
    tab$group_cols <- col_pal[as.character(tab$group)]

    # add text colors
    for (i in c("text_color_inside", "text_color_outside")) {
        tab[[i]] <- bw_txt(tab$group_cols)
    }
    # for (i in c("label_radius_inside", "label_radius_outside")) {
    #   tab[which(tab[[i]] >= 1), gsub("label_radius", "text_color", i)] <- bw_txt(plot[["theme"]][["panel.background"]][["fill"]])
    # }

    if (label_inside == "rel") {
        tab <- make_rel_labels(which = "label_text_inside",
                               label_rel_pct = label_rel_pct,
                               label_rel_cutoff = label_rel_cutoff,
                               #print_pct_sign = print_pct_sign,
                               label_rel_dec = label_rel_dec,
                               tab = tab)
    } else if (label_inside == "abs") {
        tab$label_text_inside <- tab[,"abs"]
    }

    if (label_outside == "rel") {
        tab <- make_rel_labels(which = "label_text_outside",
                               label_rel_pct = label_rel_pct,
                               label_rel_cutoff = label_rel_cutoff,
                               #print_pct_sign = print_pct_sign,
                               label_rel_dec = label_rel_dec,
                               tab = tab)
    } else if (label_outside == "abs") {
        tab$label_text_outside <- tab[,"abs"]
    }

    plot <-
        ggplot2::ggplot(tab, ggplot2::aes(
            x0 = 0,
            y0 = 0,
            r0 = radius_inside,
            r = 1,
            start = start_angle,
            end = end_angle,
            fill = group
        )) +
        ggforce::geom_arc_bar(colour = color) +
        ggplot2::labs(fill = legend_title) +
        ggplot2::scale_fill_manual(values = stats::setNames(tab$group_cols, tab$group),
                                   na.value = fill_na) +
        ggplot2::coord_fixed(ratio = 1) +
        Gmisc::fastDoCall(ggplot2::theme, args = theme_args)

    if (label_inside != "none") {
        plot <-
            plot +
            ggplot2::geom_text(data = tab, ggplot2::aes(color = I(text_color_inside),
                                                        x = label_radius_inside*sin(mid_angle),
                                                        y = label_radius_inside*cos(mid_angle),
                                                        angle = label_angle_inside,
                                                        label = label_text_inside),
                               size = label_size)
    }

    if (label_outside != "none") {
        plot <-
            plot +
            ggplot2::geom_text(data = tab, ggplot2::aes(color = I(text_color_outside),
                                                        x = label_radius_outside*sin(mid_angle),
                                                        y = label_radius_outside*cos(mid_angle),
                                                        angle = label_angle_outside,
                                                        label = label_text_outside),
                               size = label_size,
                               hjust = 1)
    }

    return(list(plot = plot, data = tab))
}

check_and_add_col_pal <- function(tab, col_pal) {
    if (length(col_pal) != nlevels(tab$group)) {
        if (is.null(names(col_pal))) {
            if (length(col_pal) < length(unique(tab$group))) {
                col_pal <- scales::hue_pal()(length(unique(tab$group)))
                message("Number of colors provided not sufficient for number of factor levels. Falling back to scales::hue_pal().")
            } else {
                col_pal <- col_pal[1:length(unique(tab$group))]
            }
            names(col_pal) <- tab$group
        } else {
            if (length(col_pal) > length(unique(tab$group)) && all(names(col_pal) %in% unique(tab$group))) {
                col_pal <- col_pal[unique(as.character(tab$group))]
                tab$group_cols <- col_pal[tab$group]
            } else {
                message("Number of colors provided not matching the number of factor levels in meta.col. Falling back to scales::hue_pal().")
                col_pal <- scales::hue_pal()(length(unique(tab$group)))
                names(col_pal) <- tab$group
            }
        }

    } else if (!is.null(names(col_pal)) && !all(names(col_pal) %in% unique(tab$group))) {
        message("Not all names of col_pal found in factor levels of meta.col. Falling back to scales::hue_pal().")
        col_pal <- scales::hue_pal()(length(unique(tab$group)))
        names(col_pal) <- tab$group
    }
    tab$group_cols <- col_pal[as.character(tab$group)]
    return(tab)
}

bw_txt <- function(bg_col) {
    ifelse(farver::decode_colour(bg_col, to = "hcl")[, "l"] > 50, "black", "white")
}

make_rel_labels <- function(which = c("label_text_inside", "label_text_outside"),
                            label_rel_pct,
                            label_rel_cutoff,
                            print_pct_sign = T,
                            label_rel_dec,
                            tab) {
    which <- rlang::arg_match(which)
    if (label_rel_pct) {
        ## problem with decimals and > 1 % may arise
        temp_labels <- round2(tab$rel*100, label_rel_dec)
        temp_labels[which(temp_labels == 0 & tab$rel > 0)] <- "< 1"
        tab[[which]] <- temp_labels
        if (any(tab$rel < label_rel_cutoff)) {
            tab[[which]][which(tab$rel < label_rel_cutoff)] <- ""
        }
        if (print_pct_sign) {
            for (i in 1:length(tab[[which]])) {
                if (tab[[which]][i] != "") {
                    if (tab$rel[i] < 0.01 & tab$rel[i] > 0) {
                        tab[[which]][i] <- "< 1 %"
                    } else if (tab$rel[i] > 0.01 & tab$rel[i] < 0.99) {
                        tab[[which]][i] <- paste0(tab[[which]][i] , " %")
                    } else if (tab$rel[i] > 0.99 & tab$rel[i] < 1) {
                        tab[[which]][i] <- "> 99 %"
                    } else {
                        tab[[which]][i] <- paste0(tab[[which]][i], " %")
                    }
                }
            }
        } else {
            # or use sprinf fun
            tab[[which]] <- format(tab[[which]], nsmall = label_rel_dec)
        }
    } else {
        tab[[which]] <- format(round2(tab$rel, label_rel_dec), nsmall = label_rel_dec)
    }
    return(tab)
}

make_pie_basis <- function(x, order) {
    # https://stackoverflow.com/questions/16184188/ggplot-facet-piechart-placing-text-in-the-middle-of-pie-chart-slices (ggforce)
    if (is.null(names(x))) {
        tab <- table(as.character(x), exclude = c())
        tab <- stats::setNames(as.numeric(tab), names(tab)) # to ordinary named numeric vector
    } else if (is.numeric(x) && !is.null(names(x))) {
        tab <- x
    } else {
        stop("x must be numeric with names (=summarized data) or unnamed.")
    }
    tab <- data.frame(abs = unname(tab),
                      rel = as.numeric(tab/sum(tab)),
                      group = factor(names(tab), levels = names(tab)))
    if (!is.null(order)) {
        tab <- tab[order(tab$rel, decreasing = order), ]
    }
    tab$start_angle <- c(0,cumsum(tab$rel))[-(length(tab$rel) + 1)]*pi*2
    tab$end_angle <- c(cumsum(tab$rel))*pi*2
    tab$mid_angle <-  0.5*(tab$start_angle + tab$end_angle)
    tab$label_angle <- ifelse(tab$mid_angle > pi, 270 - tab$mid_angle*180/pi, 90 - tab$mid_angle*180/pi)

    tab$rel_lag <- as.numeric(lag(tab$rel, default = 0)) # drop attributes; for rle below
    tab$rel_lag_diff <- tab$rel - tab$rel_lag
    tab$rel_lag_diff_series <- cumsum(abs(tab$rel_lag_diff) <= 0.05) ## not used yet

    return(tab)
}


make_label_angles_and_radii <- function(tab,
                                        label_angle_inside,
                                        label_angle_outside,
                                        label_radius_outside,
                                        label_radius_inside,
                                        label_overlap) {
    # text angle equal to angle of circle but readable
    # relevant if order = T or order = F ??
    tab$label_angle_inside  <- label_angle_inside %||% tab$label_angle
    tab$label_angle_outside <- label_angle_outside %||% tab$label_angle

    tab$label_radius_outside <- recycle(long = tab$abs, short = label_radius_outside)
    tab$label_radius_inside <- recycle(long = tab$abs, short = label_radius_inside)


    rel_series <- rle(abs(tab$rel_lag_diff) <= 0.05)
    relcs <- cumsum(rel_series$lengths)
    lagrelcs <- lag(relcs+1)
    if (label_overlap == "alternate") {
        tab$label_radius_inside[unlist(seq2(lagrelcs[-1], relcs[-1]))] <-
            ifelse(unlist(seq2(lagrelcs[-1], relcs[-1])) %% 2 == 0,
                   tab$label_radius_inside[unlist(seq2(lagrelcs[-1], relcs[-1]))] - 0.1,
                   tab$label_radius_inside[unlist(seq2(lagrelcs[-1], relcs[-1]))] + 0.1)
    } else if (label_overlap == "outside") {
        tab$label_radius_inside[unlist(seq2(lagrelcs[-1], relcs[-1]))] <- overlap_outside_radius
    }
    return(tab)
}


