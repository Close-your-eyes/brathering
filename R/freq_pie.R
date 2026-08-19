#' Make a Pie or Donut Chart
#'
#' Draw a pie or donut chart in Cartesian coordinates. Input may be raw
#' observations or an already summarized named numeric vector. Absolute or
#' relative labels can be placed and rotated independently inside and outside
#' the slices.
#'
#' @param x An unnamed vector of observations to tabulate, or a named numeric
#'   vector of already summarized non-negative values.
#' @param order Controls sorting after tabulation. Use `NULL` to retain the
#'   existing order, `TRUE` for decreasing values, or `FALSE` for increasing
#'   values.
#' @param fill A vector of valid slice colours. It may be named with groups in
#'   `x`; otherwise colours are matched by position.
#' @param fill_na Colour used for a missing group.
#' @param color Border colour of the slices. Use `NA` for no border.
#' @param radius_inside Inner radius of the slices. The outer radius is fixed at
#'   `1`; use `0` for a pie chart or a value between `0` and `1` for a donut.
#' @param label_outside Outside-label content: `"none"`, `"abs"` for absolute
#'   values, or `"rel"` for relative values.
#' @param label_inside Inside-label content. Accepts the same values as
#'   `label_outside`.
#' @param label_rel_cutoff Minimum slice proportion required for a label to be
#'   displayed. For example, `0.05` suppresses labels below five percent.
#' @param label_size Text size passed to [ggplot2::geom_text()].
#' @param label_radius_inside Numeric vector of radii for inside labels. Values
#'   are recycled across slices.
#' @param label_radius_outside Numeric vector of radii for outside labels.
#'   Values are recycled across slices; values greater than `1` lie beyond the
#'   outer edge.
#' @param label_angle_inside Inside-label orientation. Supply
#'   `"radial_readable"`, `"radial"`, `"tangent_readable"`, or `"tangent"`;
#'   alternatively, supply one or more numeric angles in degrees as interpreted
#'   by [ggplot2::geom_text()]. See **Label angles**.
#' @param label_angle_outside Outside-label orientation. Accepts the same values
#'   as `label_angle_inside`.
#' @param label_overlap Method used to reduce overlap among nearby labels:
#'   `"ignore"`, `"alternate"`, or `"outside"`. `"alternate"` offsets inside
#'   label radii; `"outside"` moves affected inside labels to
#'   `overlap_outside_radius`.
#' @param label_color_inside Colour for inside labels. The special value
#'   `"..auto.."` chooses black or white separately for each label from its
#'   slice fill. For an explicit vector, only the first value is used.
#' @param label_color_outside Colour for outside labels. The special value
#'   `"..auto.."` chooses black or white from the plot-background fill. For an
#'   explicit vector, only the first value is used.
#' @param overlap_outside_radius Radius used when
#'   `label_overlap = "outside"`.
#' @param label_rel_pct If `TRUE`, relative labels are multiplied by 100 and
#'   printed with a percent sign. If `FALSE`, proportions are printed.
#' @param label_rel_dec Non-negative number of decimal places used for relative
#'   labels.
#' @param legend_title Optional title for the fill legend.
#' @param theme A ggplot2 theme added to the plot before `theme_args` are
#'   applied.
#' @param theme_args Named list of arguments passed to [ggplot2::theme()]. The
#'   defaults remove axes, ticks, and axis lines.
#' @param theme_args_add Additional named theme arguments appended to
#'   `theme_args`. This is useful for wrappers such as [donutchart()].
#' @param col_pal_args Additional named arguments passed to
#'   `colrr::make_col_pal()`.
#' @param axes_expand Non-negative multiplicative expansion applied to both
#'   Cartesian axes. Increase it to leave more room for outside labels.
#'
#' @section Label angles:
#' Slice angles used by [ggforce::geom_arc_bar()] are measured in radians,
#' beginning at 12 o'clock and increasing clockwise. Text angles passed to
#' [ggplot2::geom_text()] are measured in degrees, with positive values rotating
#' counterclockwise.
#'
#' The named label-angle modes are:
#'
#' * `"radial"`: align the text baseline with the slice's radial direction.
#' * `"radial_readable"`: use radial alignment and flip labels on the opposite
#'   half of the circle to keep them upright.
#' * `"tangent"`: align the text baseline with the circle's tangent, making it
#'   orthogonal to the slice's radial direction.
#' * `"tangent_readable"`: use tangential alignment and flip equivalent
#'   orientations to keep labels upright.
#'
#' Modes without the `_readable` suffix preserve the uncorrected geometric
#' orientation, so some labels may appear upside down. Numeric angles bypass
#' automatic calculation; for example, `0` makes labels horizontal.
#'
#' @return A named list with two elements: `plot`, the ggplot object, and `data`,
#'   the per-slice data frame used to construct it.
#' @export
#'
#' @examples
#' piechart(
#'     x = c(rep("a", 10), rep("b", 6), rep("c", 3), rep("d", 2)),
#'     label_rel_pct = TRUE,
#'     label_rel_dec = 1
#' )
#'
#' piechart(x = setNames(1:10, letters[1:10]))
#'
#' # Place percentage labels outside and align them with the circle tangent.
#' piechart(
#'     x = setNames(c(10, 6, 3, 2), letters[1:4]),
#'     label_inside = "none",
#'     label_outside = "rel",
#'     label_rel_pct = TRUE,
#'     label_angle_outside = "tangent_readable",
#'     axes_expand = 0.15
#'
#' piechart(
#'     x = setNames(c(10, 6, 3, 2), letters[1:4]),
#'     radius_inside = 0.7,
#'     label_inside = "none",
#'     label_outside = "rel",
#'     label_rel_pct = TRUE,
#'     label_angle_outside = "tangent_readable",
#'     axes_expand = 0.15,
#'     theme = colrr::theme_material())$plot +
#'     theme(legend.position = "inside")
#' )
piechart <- function(x,
                     order = NULL,
                     fill = colrr::col_pal("custom"),
                     fill_na = "grey50",
                     color = "white",
                     radius_inside = 0.3,
                     label_outside = c("none", "abs", "rel"),
                     label_inside = c("rel", "abs", "none"),
                     label_rel_cutoff = 0,
                     label_size = 5,
                     label_radius_inside = 0.65,
                     label_radius_outside = 1.1,
                     label_angle_inside = "radial_readable",
                     label_angle_outside = "radial_readable",
                     label_overlap = c("ignore", "alternate", "outside"),
                     label_color_inside = "..auto..",
                     label_color_outside = "..auto..",
                     overlap_outside_radius = 1.1,
                     label_rel_pct = F,
                     label_rel_dec = 2,
                     legend_title = NULL,
                     theme = ggplot2::theme_classic(),
                     theme_args = list(panel.grid = ggplot2::element_blank(),
                                       axis.title.x = ggplot2::element_blank(),
                                       axis.title.y = ggplot2::element_blank(),
                                       axis.text.x = ggplot2::element_blank(),
                                       axis.text.y = ggplot2::element_blank(),
                                       axis.ticks.x = ggplot2::element_blank(),
                                       axis.ticks.y = ggplot2::element_blank(),
                                       axis.line.x = ggplot2::element_blank(),
                                       axis.line.y = ggplot2::element_blank()),
                     theme_args_add = list(),
                     col_pal_args = list(missing_fct_to_na = T),
                     axes_expand = 0.05) {

    label_overlap <- rlang::arg_match(label_overlap)
    label_outside <- rlang::arg_match(label_outside)
    label_inside <- rlang::arg_match(label_inside)

    theme_args <- c(theme_args, theme_args_add)

    tab <- make_pie_basis(x = x,
                          order = order)

    tab <- make_label_angles_and_radii(tab = tab,
                                       label_angle_inside = label_angle_inside,
                                       label_angle_outside = label_angle_outside,
                                       label_radius_outside = label_radius_outside,
                                       label_radius_inside = label_radius_inside,
                                       label_overlap = label_overlap,
                                       overlap_outside_radius = overlap_outside_radius)
    # adds group_cols
    # tab <- check_and_add_col_pal(tab = tab, col_pal = fill)

    col_pal <- colrr::make_col_pal(col_vec = fill,
                                   fct_lvls = levels(tab$group), # levels or as.character?
                                   missing_fct_to_na = ifelse("missing_fct_to_na" %in% names(col_pal_args), col_pal_args[["missing_fct_to_na"]], T),
                                   col_pal_args = col_pal_args[-which(names(col_pal_args) %in% c("name", "missing_fct_to_na"))])
    tab$group_cols <- col_pal[as.character(tab$group)]


    if (label_inside == "rel") {
        tab <- make_rel_labels(which = "label_text_inside",
                               label_rel_pct = label_rel_pct,
                               label_rel_cutoff = label_rel_cutoff,
                               #print_pct_sign = print_pct_sign,
                               label_rel_dec = label_rel_dec,
                               tab = tab)
    } else if (label_inside == "abs") {
        tab$label_text_inside <- ifelse(tab[["rel"]] > label_rel_cutoff, tab[["abs"]], "")
    }

    if (label_outside == "rel") {
        tab <- make_rel_labels(which = "label_text_outside",
                               label_rel_pct = label_rel_pct,
                               label_rel_cutoff = label_rel_cutoff,
                               #print_pct_sign = print_pct_sign,
                               label_rel_dec = label_rel_dec,
                               tab = tab)
    } else if (label_outside == "abs") {
        tab$label_text_outside <- ifelse(tab[["rel"]] > label_rel_cutoff, tab[["abs"]], "")
    }



    plot <- ggplot2::ggplot(tab, ggplot2::aes(
        x0 = 0,
        y0 = 0,
        r0 = radius_inside,
        r = 1,
        start = start_angle_rad,
        end = end_angle_rad,
        fill = group)) +
        ggforce::geom_arc_bar(colour = color) +
        ggplot2::labs(fill = legend_title) +
        ggplot2::scale_fill_manual(values = stats::setNames(tab$group_cols, tab$group),
                                   na.value = fill_na) +
        ggplot2::coord_fixed(ratio = 1) +
        theme +
        Gmisc::fastDoCall(ggplot2::theme, args = theme_args) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = axes_expand)) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = axes_expand))

    # add text colors

    if (label_color_inside[1] == "..auto..") {
        tab[["text_color_inside"]] <- bw_txt(tab$group_cols)
    } else {
        tab[["text_color_inside"]] <- label_color_inside[1]
    }
    if (label_color_outside[1] == "..auto..") {
        tab[["text_color_outside"]] <- bw_txt(gg_get_theme_element(ggobj = plot, element = "plot.background")@fill)
    } else {
        tab[["text_color_outside"]] <- label_color_outside[1]
    }

    if (label_inside != "none") {
        plot <- plot +
            ggplot2::geom_text(data = tab, ggplot2::aes(color = I(text_color_inside),
                                                        x = label_radius_inside*sin(mid_angle_rad),
                                                        y = label_radius_inside*cos(mid_angle_rad),
                                                        angle = label_angle_inside,
                                                        label = label_text_inside),
                               size = label_size)
    }

    if (label_outside != "none") {
        plot <- plot +
            ggplot2::geom_text(data = tab, ggplot2::aes(color = I(text_color_outside),
                                                        x = label_radius_outside*sin(mid_angle_rad),
                                                        y = label_radius_outside*cos(mid_angle_rad),
                                                        angle = label_angle_outside,
                                                        label = label_text_outside),
                               size = label_size)
        # hjust = 1)
    }

    return(list(plot = plot, data = tab))
}


#' Make a Donut Chart
#'
#' A convenience wrapper around [piechart()] with a larger inner radius,
#' tangential readable labels, and optional placement of the legend inside the
#' plotting area.
#'
#' @param ... Additional arguments passed to [piechart()]. Arguments supplied
#'   here must not duplicate the explicitly forwarded wrapper arguments.
#' @param radius_inside Inner radius of the donut. The outer radius is fixed at
#'   `1`; larger values create a thinner ring.
#' @param label_radius_inside Radius used for labels drawn inside the donut
#'   slices. The default, `0.85`, places them near the middle of the default
#'   ring.
#' @param label_angle_outside Orientation of outside labels. Accepts the same
#'   named modes or numeric degree values as
#'   `piechart(label_angle_outside = ...)`.
#' @param label_angle_inside Orientation of inside labels. Accepts the same
#'   named modes or numeric degree values as
#'   `piechart(label_angle_inside = ...)`.
#' @param legend_pos Legend placement: `"outside"` retains the ggplot2 theme's
#'   normal placement, while `"inside"` sets `legend.position = "inside"`.
#' @param theme_args_add Additional named arguments appended to the theme
#'   arguments passed to [piechart()]. When `legend_pos = "inside"`, the inside
#'   legend setting is appended to this list.
#'
#' @return The same named list as [piechart()]: `plot`, containing the ggplot
#'   object, and `data`, containing the per-slice plotting data.
#' @export
#'
#' @examples
#' donutchart(x = setNames(c(10, 6, 3, 2), letters[1:4]))
#'
#' donutchart(
#'     x = c(rep("a", 10), rep("b", 6), rep("c", 3)),
#'     label_rel_pct = TRUE,
#'     legend_pos = "inside"
#' )
donutchart <- function(...,
                       radius_inside = 0.7,
                       label_radius_inside = 0.85,
                       label_angle_outside = "tangent_readable",
                       label_angle_inside = "tangent_readable",
                       legend_pos = c("outside", "inside"),
                       theme_args_add = list()) {

    legend_pos <- rlang::arg_match(legend_pos)


    if (legend_pos == "inside") {
        theme_args_add <- c(theme_args_add, list(legend.position = "inside"))
    }

    piechart(radius_inside = radius_inside,
             label_radius_inside = label_radius_inside,
             label_angle_outside = label_angle_outside,
             label_angle_inside = label_angle_inside,
             theme_args_add = theme_args_add,
             ...)
    # guides(colour = guide_legend(nrow = 2))
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

#' Choose Black or White Text for a Background Colour
#'
#' Selects a contrasting text colour based on the lightness of each background
#' colour. Light backgrounds receive black text, while dark backgrounds receive
#' white text.
#'
#' @param bg_col A character vector of colours in a format supported by
#'   [farver::decode_colour()], such as hexadecimal colour codes or colour names.
#' @param cutoff A numeric lightness threshold. Backgrounds with HCL lightness
#'   greater than this value receive black text. Defaults to `50`.
#'
#' @return A character vector containing `"black"` or `"white"`, with one value
#'   for each element of `bg_col`.
#' @export
#'
#' @examples
#' bw_txt("#253238")
#' bw_txt(c("#FFFFFF", "#000000", "#808080"))
#' bw_txt("#808080", cutoff = 60)
bw_txt <- function(bg_col, cutoff = 50) {
    lightness <- farver::decode_colour(bg_col, to = "hcl")[, "l"]
    unname(ifelse(lightness > cutoff, "black", "white"))
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
        if (any(tab$rel <= label_rel_cutoff)) {
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
        if (any(tab$rel < label_rel_cutoff)) {
            tab[[which]][which(tab$rel <= label_rel_cutoff)] <- ""
        }
    }
    return(tab)
}

make_pie_basis <- function(x, order) {
    # https://stackoverflow.com/questions/16184188/ggplot-facet-piechart-placing-text-in-the-middle-of-pie-chart-slices (ggforce)

    if (is.null(names(x))) {
        tab <- table(as.character(x), exclude = c())
        tab <- stats::setNames(as.numeric(tab), names(tab)) # to ordinary named numeric vector
        if (!is.null(order)) {
            tab <- tab[order(tab, decreasing = order)]
        } else if (is.factor(x)) {
            tab <- tab[levels(x)[which(levels(x) %in% names(tab))]]
        }
    } else if (is.numeric(x) && !is.null(names(x))) {
        tab <- x
        if (!is.null(order)) {
            tab <- tab[order(tab, decreasing = order)]
        }
    } else {
        stop("x must be numeric with names (=summarized data) or unnamed.")
    }
    tab <- data.frame(abs = unname(tab),
                      rel = as.numeric(tab/sum(tab)),
                      group = factor(names(tab), levels = names(tab)))

    tab$start_angle_rad <- c(0,cumsum(tab$rel))[-(length(tab$rel) + 1)]*pi*2
    tab$end_angle_rad <- c(cumsum(tab$rel))*pi*2
    tab$mid_angle_rad <-  0.5*(tab$start_angle_rad + tab$end_angle_rad)

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
                                        label_overlap,
                                        overlap_outside_radius) {


    tab$label_angle_inside <- resolve_label_angle(label_angle_inside, tab$mid_angle_rad)
    tab$label_angle_outside <- resolve_label_angle(label_angle_outside, tab$mid_angle_rad)

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

resolve_label_angle <- function(angle, mid_angle_rad) {
    n <- length(mid_angle_rad)

    if (n == 0L) {
        return(numeric())
    }

    if (anyNA(mid_angle_rad) || any(!is.finite(mid_angle_rad))) {
        stop("`mid_angle_rad` must contain finite, non-missing values.")
    }

    # User-supplied geom_text angles in degrees
    if (is.numeric(angle)) {
        if (length(angle) == 0L ||
            anyNA(angle) ||
            any(!is.finite(angle))) {
            stop("Numeric label angles must be finite and non-missing.")
        }

        if (n %% length(angle) != 0L) {
            stop(
                "The number of label angles must be 1, the number of slices, ",
                "or a divisor of the number of slices."
            )
        }

        return(rep_len(angle, n))
    }

    modes <- c("radial_readable", "radial", "tangent_readable", "tangent")

    if (!is.character(angle) || length(angle) != 1L || is.na(angle)) {
        stop(
            "`angle` must be a numeric vector or one of: ",
            paste(sprintf('"%s"', modes), collapse = ", "),
            "."
        )
    }

    angle <- match.arg(angle, modes)

    # ggforce angles start at 12 o'clock and increase clockwise.
    theta <- mid_angle_rad %% (2 * pi)
    mid_deg <- theta * 180 / pi

    switch(
        angle,
        radial = {90 - mid_deg},
        radial_readable = {
            radial_angle <- 90 - mid_deg
            ifelse(theta > pi, radial_angle + 180, radial_angle)},
        tangent = {-mid_deg},
        tangent_readable = {((-mid_deg + 90) %% 180) - 90}
    )
}




