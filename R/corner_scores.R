#' Find empty corners suitable for annotation
#'
#' We look along a thin horizontal band centered at the corner's y.
#' For left corners, we scan to the RIGHT. For right corners, we scan to the LEFT.
#'
#' @param x x coords
#' @param y y coords
#' @param pad fraction of x/y range to inset the corner coordinates
#' @param band_frac_y vertical band height as a fraction of the y-range (think: text height)
#' @param label_frac_x required label width as a fraction of the x-range
#' @param ggobj ggplot object to derive plot limits and x,y instead of with x,y
#' arguments
#'
#' @returns data frame with
#' gap_len   : clear horizontal run from the corner until the first obstructing point (in data units)
#' gap_frac  : gap_len / (x-range)
#' fits      : TRUE if gap_len >= label_frac_x * (x-range)
#' band_n    : number of points inside the band along the full ray
#' @export
#'
#' @examples
corner_scores <- function(x,
                          y,
                          ggobj = NULL,
                          pad = 0.04,
                          band_frac_y = 0.05,
                          label_frac_x = 0.5) {

    if (!is.null(ggobj)) {
        c(xr, yr) %<-% brathering::gg_lims(ggobj)
        mapping_chr <- sapply(ggobj$mapping, rlang::as_name)
        x <- ggobj$data[[mapping_chr[1]]]
        y <- ggobj$data[[mapping_chr[2]]]
    } else {
        # ggplot(data.frame(x,y), aes(x,y))
        stopifnot(length(x) == length(y), length(x) > 0)
        xr <- range(x)
        yr <- range(y)
    }

    dx <- diff(xr)
    dy <- diff(yr)

    corners <- list(
        bl  = c(xr[1] + pad*dx, yr[1] + pad*dy),
        tl  = c(xr[1] + pad*dx, yr[2] - pad*dy),
        br = c(xr[2] - pad*dx, yr[1] + pad*dy),
        tr = c(xr[2] - pad*dx, yr[2] - pad*dy)
    )

    half_band <- band_frac_y * dy
    need_len <- label_frac_x * dx

    assess_corner <- function(name, cxy) {
        cx <- cxy[1]; cy <- cxy[2]
        in_band <- abs(y - cy) <= half_band

        if (grepl("l$", name)) {
            # scan right
            xs <- x[in_band & x >= cx]
            first_block <- if (length(xs)) min(xs) else xr[2]
            gap_len <- max(0, first_block - cx)
            band_n  <- sum(in_band & x >= cx)  # points anywhere on the ray to the right
            direction <- "\u2192"
        } else {
            # scan left
            xs <- x[in_band & x <= cx]
            first_block <- if (length(xs)) max(xs) else xr[1]
            gap_len <- max(0, cx - first_block)
            band_n  <- sum(in_band & x <= cx)  # points anywhere on the ray to the left
            direction <- "\u2190"
        }

        data.frame(
          corner = name,
          x = cx,
          y = cy,
          x2 = ifelse(grepl("l$", name), -Inf, Inf),
          y2 = ifelse(grepl("^b", name), -Inf, Inf),
          hjust = ifelse(grepl("l$", name), 0, 1),
          vjust = ifelse(grepl("^b", name), 0, 1),
          gap_len = gap_len,
          gap_frac = gap_len / dx,
          fits = gap_len >= need_len,
          band_n = band_n,
          #direction = direction,
row.names = NULL
        )
    }

    out <- do.call(rbind, Map(assess_corner, names(corners), corners))
    return(out[order(-out$gap_len), ])
}
