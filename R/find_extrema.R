#' Find extrema by numerical procedure
#'
#' A number of parameters are needed to finetune extrema detection to the
#' desired needs. See examples. To make this completely unsupervised and
#' automatically meet your interpretation of what is a valid local maximum
#' may not be possible. Selecting a proper windowsize is crucial. The function
#' works with indices of x (no continuous xy-pairs). With respect to min_gap this
#' assumes that x values are roughly equally spaced (in case x came from
#' xy-paired values)
#'
#'
#' See gcplyr::find_local_extrema or pracma::findpeaks.
#'
#' @param x numeric vector
#' @param threshold min or max x value for calling an extreme, depending on type
#' @param min_gap minimum number of indices of x to call two separate
#' extrema, min_gap>windowsize makes no sense
#' @param type find maxima or minima
#' @param min_mono_width minimum number of (strictly) monotonous indices
#' left and right of window, either a vector of length 2 for different
#' left and right or one value for both
#' @param windowsizes windowsize(s) used for iteration over x. when there is a
#' plateau of n extreme values, a windowsize of min n is required to detect them
#' as extrema; for very sharp peaks with steep slopes, an appropriate
#' windowsize is smaller as for wider peaks.
#' @param strictly_mono should slopes left and right of window of width
#' min_mono_width be strictly monotonous? forced to TRUE for now.
#' @param type2 any one or multiple of "global_fallback", "local", "global";
#' global_fallback means that if no local extrema were found a global extreme
#' is returned in any case
#' @param stepsize step width to shift windows at; intended to speed the thing
#' up for very long x
#'
#' @return data frame with extrema
#' @export
#'
#' @examples
#' library(brathering)
#' x <- c(1,2,3,4,5,4,3,2,1,1,1,1,2,3,4,5,5,5,4,3,2,1,1,1,1,1,2,3,4,5,4,5,4,3,2,1,1,1,2,3,4,3,2,1,1,1,2,3,4,5,6,6,3,2,1,1,1,5,4,1)
#' # not every peak detected due to windowsize
#' res <- find_extrema(x = x, min_gap = 0, threshold = -Inf,
#'                     min_mono_width = 1,windowsizes = c(1))
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' # now all peaks but also neighbouring maxima
#' res <- find_extrema(x = x, min_gap = 0, threshold = -Inf,
#'                     min_mono_width = 1,windowsizes = c(1,3))
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' # use min_gap to filter consecutive extrema
#' res <- find_extrema(x = x, min_gap = 1, threshold = -Inf,
#'                     min_mono_width = 1,windowsizes = c(1,3))
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' res <- find_extrema(x = x, min_gap = 2, threshold = -Inf,
#'                     min_mono_width = 1,windowsizes = c(1,3))
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' # set a min x value: threshold
#' res <- find_extrema(x = x, min_gap = 1, threshold = 5,
#'                     min_mono_width = 1,windowsizes = c(1,3))
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' # minimum steps of monotony to call an extreme
#' res <- find_extrema(x = x, min_gap = 1, threshold = -Inf,
#'                     min_mono_width = 4,windowsizes = c(1,3))
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' # different mono left an right
#' res <- find_extrema(x = x, min_gap = 1, threshold = -Inf,
#'                     min_mono_width = c(4,1), windowsizes = c(1,3))
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' # global only
#' res <- find_extrema(x = x, min_gap = 0, threshold = -Inf,
#'                     type2 = "global")
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' # threshold only applies to local extrema
#' res <- find_extrema(x = x, min_gap = 0, threshold = 7,
#'                     type2 = "global")
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' # NULL returned when nothing was found based on parameters
#' res <- find_extrema(x = x, min_gap = 0, threshold = 7,
#'                     type2 = "local")
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' # minima
#' res <- find_extrema(x = x, type = "min", windowsizes = 4, min_gap = 0)
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
#' # add more values
#' x2 <- interpolate_vec(x, 600)
#' res <- find_extrema(x = x2,
#'                     min_gap = 0,
#'                     threshold = -Inf,
#'                     min_mono_width = 1,
#'                     windowsizes = c(1))
#' plot2(res, color = "extrm", cex = 10, legend = NULL)
find_extrema <- function(x,
                         min_gap = NULL,
                         threshold = max(x)/10,
                         type = c("max", "min"),
                         min_mono_width = NULL,
                         windowsizes = NULL,
                         stepsize = NULL,
                         strictly_mono = TRUE,
                         type2 = "global_fallback",
                         smooth = F,
                         verbose = TRUE) {

    type <- rlang::arg_match(type)
    type2 <- rlang::arg_match(type2, values = c("global_fallback", "local", "global"), multiple = T)

    if (!strictly_mono) {
        strictly_mono <- T
        message("strictly_mono muste be TRUE.")
    }

    if (type == "min") {
        x <- -1*x
        if (is.finite(threshold)) {
            # dont turn -Inf to Inf
            threshold <- -1*threshold
        }
    }

    if (anyNA(x) && verbose) {
        message("NA in x.")
    }
    xna <- brathering::na_rm(x)
    x <- xna[["x"]]

    x <- trim_zeros(x)
    lead0 <- attr(x, "lead0")
    trail0 <- attr(x, "trail0")


    # calculate loess anyway as it may be used multiple time below
    # x_loess <- NaN
    # span <- 0.05
    # while(all(is.na(x_loess))) {
    #     x_loess <- stats::loess(x ~ index, data = data.frame(index = seq_along(x), x = x), span = 0.05)$fitted
    #     span <- span + 0.05
    # }
    if (!verbose) {
        suppressWarnings(x_loess <- stats::loess(x ~ index, data = data.frame(index = seq_along(x), x = x), span = 0.05)$fitted)
    } else {
        x_loess <- stats::loess(x ~ index, data = data.frame(index = seq_along(x), x = x), span = 0.05)$fitted
    }

    if(all(is.na(x_loess))) {
        splm <- stats::smooth.spline(x = seq_along(x), y = x)
        x_loess <- stats::predict(splm, seq_along(x))$y
    }


    if (smooth) {
        # smoothing ?
        # 1)
        #x <- stats::smooth(x = xna[["x"]], kind = "3RS3R")
        # 2)
        #splm <- smooth.spline(x = seq_along(x), y = x)
        #x <- predict(splm, seq_along(x))$y
        # 3)
        x <- x_loess
        if (verbose) {
            plot2(x)
        }
    }

    # check smoothness
    # sd if residuals
    # small sd: smooth curve, large sd: noisy data
    if (is.null(min_mono_width)) {
        # if smooth = T, sdresid is 0
        sdresid <- sd(x - x_loess)
        if (sdresid < 0.3) {
            min_mono_width <- 3
        } else {
            min_mono_width <- 1
        }
    }

    if (is.null(windowsizes)) {
        peaks <- estimate_peak_position(x_loess = x_loess, min_gap = min_gap)
        if (!is.null(peaks)) {
            wid <- round(estimate_peak_widths(z = x, peak_positions = peaks))
            if (!length(wid)) {
                #actually an error
                wid <- ceiling(length(x)/100)
            }
            #windowsizes <- unique(round(seq(min(wid)/3, max(wid), length.out = 3)))
            windowsizes <- unique(sort(c(wid, ceiling(wid/2))))
        } else {
            windowsizes <- 1
        }
    }

    if (is.null(min_gap)) {
        if (exists("wid")) {
            min_gap <- max(ceiling(length(x)/40), ceiling(min(wid)/5))
        } else {
            min_gap <- ceiling(length(x)/40)
        }
    }


    if (is.null(stepsize)) {
        stepsize <- ceiling(windowsizes/10)
    } else if (length(stepsize) < length(windowsizes)) {
        stepsize <- brathering::recycle(stepsize, windowsizes)
    } else if (length(stepsize) > length(windowsizes)) {
        stepsize <- stepsize[1:length(windowsizes)]
    }
    if (verbose) {
        message("windowsizes: ", paste(windowsizes, collapse = ", "))
        message("stepsize: ", paste(stepsize, collapse = ", "))
        message("min_mono_width: ", paste(min_mono_width, collapse = ", "))
        message("min_gap: ", min_gap)
    }

    ## local extrema
    # global_fallback means local in any case and global only if no local were found
    if (any(c("global_fallback", "local") %in% type2)) {

        if (any(min_mono_width < 1)) {
            message("min_mono_width must be >=1")
            min_mono_width <- c(1, 1)
        }
        if (min_gap < 0) {
            message("min_gap must be >=0")
            min_gap <- 0
        }
        if (length(min_mono_width) == 1) {
            min_mono_width <- c(min_mono_width, min_mono_width)
        }
        # local max
        winxtrmvals <- purrr::map2(.x = windowsizes, .y = stepsize, function(size, step) {

            ## remain unchanged
            winsize <- size
            size <- size+sum(min_mono_width)
            starts <- seq(1, length(x)-size, 1)
            starts <- starts[seq(1, length(starts), step)]

            inds <- brathering::seq2(starts, starts+size-1)
            left <- purrr::map(inds, ~x[.x[1:(min_mono_width[1]+1)]])
            right <- purrr::map(inds, ~x[.x[(length(.x)-min_mono_width[2]):length(.x)]])
            win <- purrr::map(inds, ~x[.x[(min_mono_width[1]+1):(length(.x)-min_mono_width[2])]])
            wininds <- purrr::map(inds, ~.x[(min_mono_width[1]+1):(length(.x)-min_mono_width[2])])
            win <- purrr::map2(win, wininds, ~setNames(.x,.y))

            # subject to filter below
            winxtrmrelind <- purrr::map(win, brathering::which.max2)
            #winindsxtrm <- purrr::map(winxtrmrelind, ~as.numeric(names(.x)))
            #names(winindsxtrm) <- seq_along(winindsxtrm) # names are window indices
            winxtrmval <- purrr::map2(win, winxtrmrelind, ~.x[.y])

            #extremes, min_mono_width included
            is_xtrm <- purrr::map2_lgl(
                .x = left,
                .y = right,
                ~ check_loc_xtrm(leftvec = .x,
                                 rightvec = .y,
                                 strictly_mono = strictly_mono)
            )

            #winxtrmrelind <- winxtrmrelind[is_xtrm]
            #winindsxtrm <- winindsxtrm[is_xtrm]
            winxtrmval <- winxtrmval[is_xtrm]
            if (!length(winxtrmval)) return(NULL)

            #threshold
            is_below_thresh <- purrr::map_lgl(winxtrmval, ~any(.x >= threshold))
            #winxtrmrelind <- winxtrmrelind[is_below_thresh]
            #winindsxtrm <- winindsxtrm[is_below_thresh]
            winxtrmval <- winxtrmval[is_below_thresh]
            if (!length(winxtrmval)) return(NULL)

            return(unlist(winxtrmval))
        })
        # filter to close local extrema and finish up
        if (all(purrr::map_lgl(winxtrmvals, is.null))) {
            if (verbose) {
                message("no local extrema found. try other windowsizes.")
            }
            xtrmloc <- NULL
        } else {
            # combine local extrema from different window sizes and check for...
            # too close extrema
            vec <- unlist(unname(winxtrmvals))
            is_duplicate <- which(duplicated(names(vec)))
            if (length(is_duplicate)) {
                vec <- vec[-is_duplicate]
            }

            vec <- vec[order(as.numeric(names(vec)))]
            group_id <- cumsum(c(TRUE, diff(as.numeric(names(vec))) > min_gap)) # consecutive groups
            vecgrouped <- split(vec, group_id)
            xtrmpergroup <- purrr::map(vecgrouped, ~as.numeric(names(.x[brathering::which.max2(.x)]))) # max per group, continue with indices only
            xtrmpergroup <- sort(purrr::map_int(unname(xtrmpergroup), ~get_median_val(.x))) # which xtrm to select? first, last, mid, random

            # reinsert NA
            x2 <- brathering::na_insert(xna)

            # reinsert zeros
            x2 <- c(rep(0, lead0), x2, rep(0, trail0))
            xtrmpergroup <- xtrmpergroup + lead0

            # alter xtrmpergroup for NA
            xtrmpergroup <- brathering::na_fix_ind(xna[["x"]], x2, xtrmpergroup) # use xna[["x"]] in case x was smoothed
            xtrmloc <- data.frame(
                index = seq_along(x2),
                x = x2,
                #extrm = seq_along(x2) %in% xtrmpergroup,
                type = ifelse(seq_along(x2) %in% xtrmpergroup, type, NA),
                type2 = ifelse(seq_along(x2) %in% xtrmpergroup, "local", NA))
        }
    }

    ## global extrema
    if (any(type2 == "global") || (any(type2 == "global_fallback") && is.null(xtrmloc))) {
        # in this way global extrema are found in original, not smoothed data
        x2 <- brathering::na_insert(xna)
        x2 <- c(rep(0, lead0), x2, rep(0, trail0))

        if (length(unique(x)) != 1) {
            globxtrm <- brathering::which.max2(x2, na.rm = T)

            # filter close extremes
            group_id <- cumsum(c(TRUE, diff(globxtrm) > min_gap))
            vecgrouped <- split(globxtrm, group_id)
            xtrmpergroup <- sort(purrr::map_int(unname(vecgrouped), ~get_median_val(.x))) # which xtrm to select? first, last, mid, random
        } else {
            xtrmpergroup <- NULL
        }

        xtrmglob <- data.frame(
            index = seq_along(x2),
            x = x2,
            #extrm = seq_along(x2) %in% xtrmpergroup,
            type = ifelse(seq_along(x2) %in% xtrmpergroup, type, NA),
            type2 = ifelse(seq_along(x2) %in% xtrmpergroup, "global", NA)
        )

        if (any("local" == type2) && !is.null(xtrmloc)) {
            # this order of joining will prefer global over local
            xtrmfinal <- brathering::coalesce_join(xtrmglob, xtrmloc, by = c("index", "x"))
        } else {
            # no local extremes
            xtrmfinal <- xtrmglob
        }

    } else {
        # no global extremes to be found
        xtrmfinal <- xtrmloc
    }

    if (!is.null(xtrmfinal)) {
        xtrmfinal$extrm <- !is.na(xtrmfinal[["type2"]])
        if (type == "min") {
            xtrmfinal[["x"]] <- -1*xtrmfinal[["x"]]
        }
        if (smooth) {
            xtrmfinal[["x_loess"]] <- brathering::na_insert(x, xna[["na_inds"]])
        }

        # filter too close global and local
        vec <- xtrmfinal$index[which(xtrmfinal$extrm)]
        group_id <- cumsum(c(TRUE, diff(vec) > min_gap)) # consecutive groups
        vecgrouped <- split(vec, group_id)
        xtrmpergroup <- purrr::map(vecgrouped, ~.x[brathering::which.max2(xtrmfinal$x[.x])]) # top extrema based on not smoothed data
        xtrmpergroup <- sort(purrr::map_int(unname(xtrmpergroup), ~get_median_val(.x)))
        ii <- setdiff(1:nrow(xtrmfinal), xtrmpergroup)
        xtrmfinal$type[ii] <- NA
        xtrmfinal$type2[ii] <- NA
        xtrmfinal$extrm[ii] <- F

        attr(xtrmfinal, "min_gap") <- min_gap
        attr(xtrmfinal, "threshold") <- threshold
        attr(xtrmfinal, "type") <- type
        attr(xtrmfinal, "min_mono_width") <- min_mono_width
        attr(xtrmfinal, "windowsizes") <- windowsizes
        attr(xtrmfinal, "stepsize") <- stepsize
        attr(xtrmfinal, "strictly_mono") <- strictly_mono
        attr(xtrmfinal, "type2") <- type2
        attr(xtrmfinal, "smooth") <- smooth
    }





    return(xtrmfinal)
}
make_intervals <- function(start = 0,
                           end = 100,
                           step = 5,
                           lengths = c(20)) {
    if (identical(start, end)) {
        return(NULL)
    }
    intervals <- lapply(lengths, function(len) {
        starts <- seq(start, end - len, by = step)
        ends <- starts + len
        data.frame(start = starts, end = ends, length = len)
    })
    all_intervals <- do.call(rbind, intervals)
    return(all_intervals)
}

estimate_peak_position <- function(x_loess, min_gap) {
    df <- data.frame(x = x_loess, index = seq_along(x_loess))
    start = floor(min(x_loess))
    end = ceiling(max(x_loess))
    intervals <- make_intervals(start = start,
                                end = end,
                                step = (end-start)/20,
                                lengths = (end-start)/5)
    if (is.null(intervals)) {
        return(NULL)
    }
    if (is.null(min_gap)) {
        min_gap <- ceiling(length(x_loess)/40)
    }

    peaks <- purrr::map2(.x = intervals$start, .y = intervals$end, function(lo, hi) {
        df <- df[dplyr::between(df$x, lo, hi),]
        if (nrow(df) == 0) {
            return(NA)
        }
        df$group_id <- cumsum(c(TRUE, diff(as.numeric(df$index)) > 1))
        nn <- table(df$group_id)
        df <- dplyr::filter(df, group_id %in% names(nn[which(nn>3)]))
        if (nrow(df) == 0) {
            return(NA)
        }
        #plot2(df, color = "group_id")
        purrr::map_int(unique(df$group_id), function(id) {
            df <- dplyr::filter(df, group_id == id)
            linear_model <- lm(x ~ index, data = df)
            quadratic_model <- lm(x ~ index + I(index^2), data = df)
            suppressWarnings(linadjr <- summary(linear_model)$adj.r.squared)
            suppressWarnings(quadadjr <- summary(quadratic_model)$adj.r.squared)
            if (!is.na(linadjr) && !is.na(quadadjr) && linadjr<0.3 && quadadjr>0.8 && coefficients(quadratic_model)[3]<0) {
                # valid peak
                return(round(median(df$index)))
            } else {
                return(NA)
            }
        })
    })

    peaks <- unique(unlist(peaks))
    peaks <- sort(peaks[which(!is.na(peaks))])
    group_id <- cumsum(c(TRUE, diff(peaks) > min_gap))
    peaksgrouped <- split(peaks, group_id)
    peaksgrouped <- unlist(purrr::map(peaksgrouped, ~.x[which.max(x_loess[.x])]))
    if (!length(peaksgrouped)) {
        peaksgrouped <- brathering::which.max2(x_loess) # maxima or any point above thresh
    }
    return(peaksgrouped)
}

estimate_peak_widths <- function(z, peak_positions = NULL) {
    #z <- smooth(z, kind = "3RS3R")
    if (is.null(peak_positions)) {
        peak_positions <- brathering::which.max2(z) # maxima or any point above thresh
    }

    widths <- fwhm_index(y = z, peaks = peak_positions)
    widths <- widths[which(!is.na(widths))]
    widths <- widths[which(widths>0)]

    return(widths)
}

fwhm <- function(y, peaks, x = NULL) {
    n <- length(y)
    if (is.null(x)) x <- seq_along(y)  # use indices if x not provided

    fwhm_vals <- numeric(length(peaks))
    names(fwhm_vals) <- peaks

    for (i in seq_along(peaks)) {
        peak_idx <- peaks[i]
        peak_val <- y[peak_idx]
        half_max <- peak_val / 2

        # Search to the left
        left_idx <- peak_idx
        while (left_idx > 1 && y[left_idx] > half_max) {
            left_idx <- left_idx - 1
        }
        if (left_idx == 1) next  # skip if no crossing found

        # Linear interpolate left
        x_left <- x[left_idx] +
            (half_max - y[left_idx]) / (y[left_idx + 1] - y[left_idx]) * (x[left_idx + 1] - x[left_idx])

        # Search to the right
        right_idx <- peak_idx
        while (right_idx < n && y[right_idx] > half_max) {
            right_idx <- right_idx + 1
        }
        if (right_idx == n) next  # skip if no crossing found

        # Linear interpolate right
        x_right <- x[right_idx - 1] +
            (half_max - y[right_idx - 1]) / (y[right_idx] - y[right_idx - 1]) * (x[right_idx] - x[right_idx - 1])

        # Width at half maximum
        fwhm_vals[i] <- x_right - x_left
    }

    return(fwhm_vals)
}

fwhm_index <- function(y, peaks) {
    n <- length(y)
    fwhm_vals <- numeric(length(peaks))
    names(fwhm_vals) <- peaks

    for (i in seq_along(peaks)) {
        peak_idx <- peaks[i]
        peak_val <- y[peak_idx]
        half_max <- peak_val / 2

        # Search to the left
        left_idx <- peak_idx
        while (left_idx > 1 && y[left_idx] > half_max) {
            left_idx <- left_idx - 1
        }
        if (left_idx == 1) next  # skip if edge

        # Linear interpolation left (optional for sub-index accuracy, but skipped here)
        interp_left <- left_idx +
            (half_max - y[left_idx]) / (y[left_idx + 1] - y[left_idx])

        # Search to the right
        right_idx <- peak_idx
        while (right_idx < n && y[right_idx] > half_max) {
            right_idx <- right_idx + 1
        }
        if (right_idx == n) next  # skip if edge

        interp_right <- right_idx - 1 +
            (half_max - y[right_idx - 1]) / (y[right_idx] - y[right_idx - 1])

        # FWHM in number of indices
        fwhm_vals[i] <- interp_right - interp_left
    }

    return(fwhm_vals)
}


generate_synthetic_ts <- function(length = 1000, n_peaks = 10, noise_sd = 0.05,
                                  include_baseline = TRUE,
                                  include_periodic = TRUE,
                                  peak_types = c("gaussian", "lorentzian", "shoulder", "merged")) {
    # Time vector
    x <- seq(0, 100, length.out = length)
    y <- numeric(length(x))

    # Define peak shapes
    gaussian <- function(x, center, height, width) {
        height * exp(-((x - center)^2) / (2 * width^2))
    }

    lorentzian <- function(x, center, height, width) {
        height / (1 + ((x - center)/width)^2)
    }

    # Generate peaks
    for (i in seq_len(n_peaks)) {
        type <- sample(peak_types, 1)
        center <- runif(1, min = 5, max = 95)
        height <- runif(1, min = 0.5, max = 1.5)
        width <- runif(1, min = 1, max = 5)

        if (type == "gaussian") {
            y <- y + gaussian(x, center, height, width)
        } else if (type == "lorentzian") {
            y <- y + lorentzian(x, center, height, width)
        } else if (type == "shoulder") {
            y <- y + gaussian(x, center, height, width)
            y <- y + gaussian(x, center + runif(1, 1.5, 3), height * runif(1, 0.3, 0.7), width * 0.8)
        } else if (type == "merged") {
            y <- y + gaussian(x, center - width/2, height, width)
            y <- y + gaussian(x, center + width/2, height * runif(1, 0.7, 1), width)
        }
    }

    # Add baseline drift
    if (include_baseline) {
        drift <- approx(x = c(0, 100), y = runif(2, -0.2, 0.2), xout = x)$y
        y <- y + drift
    }

    # Add periodic component
    if (include_periodic) {
        freq <- runif(1, 0.05, 0.2)
        amp <- runif(1, 0.05, 0.2)
        y <- y + amp * sin(2 * pi * freq * x)
    }

    # Add noise
    y <- y + rnorm(length(x), sd = noise_sd)

    # Return data.frame for easy plotting
    #return(data.frame(time = x, signal = y))
    return(y)
}



check_loc_xtrm <- function(leftvec,
                           rightvec,
                           strictly_mono = TRUE) {

    # when !strictly_mono it may happen that a plateau of minima is called as maxima
    # as ldiff and rdiff is all 0. but thats not what we want.
    # to now detect stretches of maxima with ldiff and rdiff all 0,
    # adjustment of windowsize is required

    fun <- ifelse(strictly_mono, `>`, `>=`)
    ldiff <- diff(leftvec)
    rdiff <- diff(rightvec)*-1
    return(all(fun(ldiff,0)) && all(fun(rdiff,0)))
}

get_median_val <- function(y) {
    y <- as.numeric(y)
    mid <- round(median(y))
    val <- y[which.min(abs(mid-y))]
    return(val)
}




# catgpt:
# find_local_extrema_inds <- function(
        #         x,
#         threshold = -Inf,
#         min_gap = 0,
#         close_extremes = c("random", "both"),
#         type = c("maxima", "minima"),
#         min_mono_width = 0,
#         adaptive_window_sizes = NULL
# ) {
#     close_extremes <- rlang::arg_match(close_extremes)
#     type <- rlang::arg_match(type)
#
#     # Flip sign for minima search
#     x_proc <- if (type == "minima") -x else x
#
#     original_indices <- seq_along(x_proc)
#     na_mask <- !is.na(x_proc)
#     x_clean <- x_proc[na_mask]
#     index_clean <- original_indices[na_mask]
#     x_orig_clean <- x[na_mask]
#
#     n <- length(x_clean)
#     extrema_candidates <- integer(0)
#
#     i <- 2
#     while (i < n) {
#         if (x_clean[i] > x_clean[i - 1]) {
#             start <- i
#             while (i < n && x_clean[i] == x_clean[i + 1]) {
#                 i <- i + 1
#             }
#             if (i < n && x_clean[i] > x_clean[i + 1]) {
#                 mid <- round((start + i) / 2)
#                 value_at_mid <- x_orig_clean[mid]
#
#                 # Threshold condition
#                 if ((type == "maxima" && value_at_mid > threshold) ||
#                     (type == "minima" && value_at_mid < threshold)) {
#
#                     # Semi-monotonic trend check
#                     pass_prominence <- TRUE
#                     if (min_mono_width > 0) {
#                         left_start <- max(1, mid - min_mono_width)
#                         right_end <- min(n, mid + min_mono_width)
#
#                         left_trend <- all(diff(x_clean[left_start:mid]) >= 0)
#                         right_trend <- all(diff(x_clean[mid:right_end]) <= 0)
#
#                         if (!(left_trend && right_trend)) {
#                             pass_prominence <- FALSE
#                         }
#                     }
#
#                     # Adaptive window check
#                     pass_window <- FALSE
#                     if (!is.null(adaptive_window_sizes)) {
#                         for (w in adaptive_window_sizes) {
#                             if (mid > w && mid <= n - w) {
#                                 left <- x_clean[(mid - w):(mid - 1)]
#                                 right <- x_clean[(mid + 1):(mid + w)]
#                                 center <- x_clean[mid]
#
#                                 if (all(center > left) && all(center > right)) {
#                                     pass_window <- TRUE
#                                     break
#                                 }
#                             }
#                         }
#                     }
#
#                     # Accept if passes either strategy
#                     if (pass_prominence || pass_window) {
#                         extrema_candidates <- c(extrema_candidates, index_clean[mid])
#                     }
#                 }
#             }
#         }
#         i <- i + 1
#     }
#
#     # Merge close extrema
#     if (min_gap > 0 && length(extrema_candidates) > 1) {
#         kept <- logical(length(extrema_candidates))
#         i <- 1
#         while (i <= length(extrema_candidates)) {
#             group <- extrema_candidates[i]
#             j <- i + 1
#             while (j <= length(extrema_candidates) &&
#                    (extrema_candidates[j] - extrema_candidates[j - 1]) < min_gap) {
#                 group <- c(group, extrema_candidates[j])
#                 j <- j + 1
#             }
#
#             values <- x[group]
#             best_val <- if (type == "maxima") max(values) else min(values)
#             best_indices <- which(values == best_val)
#
#             if (length(best_indices) == 1 || close_extremes == "random") {
#                 keep_one <- if (length(best_indices) == 1) best_indices else sample(best_indices, 1)
#                 kept[i + keep_one - 1] <- TRUE
#             } else if (close_extremes == "both") {
#                 kept[i - 1 + best_indices] <- TRUE
#             }
#
#             i <- j
#         }
#         extrema_candidates <- extrema_candidates[kept]
#     }
#
#     # Fallback: global max/min
#     if (length(extrema_candidates) == 0 && any(!is.na(x))) {
#         fallback_index <- if (type == "maxima") which.max(x) else which.min(x)
#         extrema_candidates <- fallback_index
#     }
#
#     return(extrema_candidates)
# }
