#' ggsave with defaults
#'
#' filename on disk derives from plot var name and format (device) by default.
#' width derive from height and aspect ratio by default - easy scaling.
#' overwrite management.
#' path derives from global var by default - reduced redundancy.
#'
#' set im_path global option: options(im_path = file.path(...))
#'
#' @param plot plot
#' @param filename filename
#' @param device device
#' @param path filepath
#' @param width w
#' @param height h
#' @param create.dir do it?
#' @param ... other arg to ggplot2::ggsave
#' @param aspect_ratio set height relative to width: height = width/aspect_ratio
#' @param overwrite overwrite existing file on disk?
#' @param append_filename when overwrite = F, change filename to save plot?
#'
#' @returns nothing
#' @export
#'
#' @examples
#' tt <- data.frame(x = rnorm(20), y = rnorm(20))
#' options(im_path = "~/Desktop")
#'
#' pp <- ggplot2::ggplot(tt, ggplot2::aes(x,y)) +
#'     ggplot2::geom_point()
#'
#' gg_save(pp)
gg_save <- function(plot = ggplot2::last_plot(),
                    filename = paste0(deparse(substitute(plot)), ".", deparse(substitute(device))),
                    device = png,
                    path = getOption("im_path", default = getwd()),
                    width = 8,
                    height = NULL,
                    aspect_ratio = 4/3,
                    create.dir = T,
                    overwrite = F,
                    append_filename = T,
                    ...) {

    if (is.null(height) && is.null(width)) {
        height <- 8
    }

    if (is.null(height)) {
        height <- width/aspect_ratio
    }

    if (is.null(width)) {
        width <- height*aspect_ratio
    }

    target <- file.path(path, filename)

    if (file.exists(target)) {
        if (overwrite) {
            # nothing
            # append_filename irrelevant
        } else if (!overwrite && append_filename) {
            target <- make_filepath_unique(target)
            filename <- basename(target)
        } else if (!overwrite && !append_filename) {
            message("gg_save: cannot overwrite ", target)
            invisible(NULL)
        }
    }

    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      device = device,
      path = path,
      width = width,
      height = height,
      create.dir = create.dir,
      ...
    )

    message(file.path(path, filename))
    invisible(file.path(path, filename))
}


