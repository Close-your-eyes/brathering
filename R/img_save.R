#' image save with defaults
#'
#' filename on disk derives from plot var name and format (device) by default.
#' width derive from height and aspect ratio by default - easy scaling.
#' overwrite management.
#' path derives from global var by default - reduced redundancy.
#'
#' set im_path global option: options(im_path = file.path(...))
#'
#' @param plotdata plotdata
#' @param filename filename
#' @param device device
#' @param path filepath
#' @param width w
#' @param height h
#' @param device_args args to device
#' @param aspect_ratio set height relative to width: height = width/aspect_ratio
#' @param overwrite overwrite existing file on disk?
#' @param append_filename when overwrite = F, change filename to save plot?
#' @param plotfun function for plotting plotdata
#' @param units units of height and width
#' @param dpi resolution in dots per inch
#' @param plotfun_args arguments to plotfun
#'
#' @returns nothing
#' @export
#'
#' @examples
#' tt <- data.frame(x = rnorm(20), y = rnorm(20))
#' options(im_path = "~/Desktop")
#' img_save(plotdata = tt)
#' img_save(plotdata = tt, plotfun = brathering::plot2, plotfun_args = list(size = 1))
img_save <- function(plotdata,
                     plotfun = plot,
                     filename = paste0(deparse(substitute(plotdata)), ".", device),
                     device = c("png", "tiff", "jpeg", "bmp"),
                     path = getOption("im_path", default = getwd()),
                     width = 8,
                     height = NULL,
                     aspect_ratio = 4/3,
                     units = "in",
                     dpi = 200,
                     overwrite = F,
                     append_filename = T,
                     device_args = list(),
                     plotfun_args = list()) {

    if (is.null(height) && is.null(width)) {
        height <- 8
    }

    if (is.null(height)) {
        height <- width/aspect_ratio
    }

    if (is.null(width)) {
        width <- height*aspect_ratio
    }
    device <- rlang::arg_match(device)
    target <- path.expand(file.path(path, filename))
    device <- match.fun(device)

    dir.create(path, showWarnings = F, recursive = T)

    if (file.exists(target)) {
        if (overwrite) {
            # nothing
            # append_filename irrelevant
        } else if (!overwrite && append_filename) {
            target <- make_filepath_unique(target)
            filename <- basename(target)
        } else if (!overwrite && !append_filename) {
            message("img_save: cannot overwrite ", target)
            invisible(NULL)
        }
    }
    target <- path.expand(file.path(path, filename))

    do.call(what = device, args = c(list(filename = target,
                                         width = width,
                                         height = height,
                                         units = units,
                                         res = dpi),
                                    device_args))
    do.call(what = plotfun, args = c(list(x = plotdata), plotfun_args))
    dev.off()

    message(target)
    invisible(target)
}


