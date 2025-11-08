#' Make a tsne movie
#'
#' @param x numeric matrix
#' @param y data frame, the first column of which becomes color variable
#' @param max_iters numbers of iteration for tsne
#' @param fft_tsne_args args to fft_tsne
#' @param mc.cores parallel computing?
#' @param out_file output video file
#' @param animate_args args to gganimate::animate
#' @param ffmpeg_renderer_args args to gganimate::ffmpeg_renderer
#' @param y_col_fun color function for
#'
#' @returns raw tsne coordinates and gganimate-ggplot for re-rendering
#' @export
#'
#' @examples
#' tsneanim <- tsne_animation(x = hinze@reductions[["pca"]]@cell.embeddings,
#'                            y = hinze@meta.data[,"celltype_muto",drop = F],
#'                            mc.cores = 10)
#'                            # how to zoom smoothly between different time intervals
tsne_animation <- function(x,
                           y = NULL,
                           max_iters = c(1:9, seq(10,200,10), seq(250, 800, 50)),
                           fft_tsne_args = list(rand_seed = 42),
                           mc.cores = 1,
                           out_file = file.path(getwd(), "tsne_anim.mp4"),
                           y_col_fun = colrr::scale_color_custom(),
                           view_fun = gganimate::view_follow(),
                           animate_args = list(nframes = 200,
                                               fps = 25,
                                               height = 600,
                                               width = 800,
                                               units = "px",
                                               device = "png"),
                           ffmpeg_renderer_args = list(ffmpeg = "/opt/local/bin/ffmpeg8",
                                                       format = "mp4",
                                                       options = list(pix_fmt = "yuv420p",
                                                                      #vf = "scale=800:800",
                                                                      #crf = 20,
                                                                      #r = 25,
                                                                      preset = "faster",
                                                                      `c:v` = "libx264"))) {
    # y same rows as x
    # y df with names
    names(max_iters) <- max_iters
    out <- parallel::mclapply(X = max_iters,
                              function(z) as.data.frame(Gmisc::fastDoCall(what = fft_tsne,
                                                                          args = c(list(X = x, max_iter = z),
                                                                                   fft_tsne_args))),
                              mc.cores = mc.cores)

    out <- purrr::map_dfr(out, ~cbind(.x, y), .id = "max_iter") |>
        dplyr::mutate(max_iter = as.numeric(max_iter))

    if (!is.null(y)) {
        p <- ggplot2::ggplot(out, ggplot2::aes(x = tSNE_1, y = tSNE_2, color = !!rlang::sym(names(y)[1]))) +
            y_col_fun
    } else {
        p <- ggplot2::ggplot(out, ggplot2::aes(x = tSNE_1, y = tSNE_2))
    }
    p <- p +
        ggplot2::geom_point() +
        ggplot2::labs(title = "max_iter: {frame_time}") +
        gganimate::transition_time(max_iter) +
        gganimate::ease_aes("linear") +
        view_fun

    tempfilepath <- tryCatch(expr = {
        renderer <- Gmisc::fastDoCall(what = gganimate::ffmpeg_renderer,
                                      args = ffmpeg_renderer_args)

        Gmisc::fastDoCall(what = gganimate::animate,
                          args = c(list(plot = p,
                                        renderer = renderer),
                                   animate_args))
    }, error = function(err) {
        message("error in plotting. returning data only.")
        print(err)
        return(out)
    })


    tryCatch(expr = {
        fs::file_move(tempfilepath, out_file)
        message(out_file)
    }, error = function(err) {
        message(tempfilepath)
    })


    invisible(list(data = out, plot = p))
}




#
#
# dat <- tsneanim$data |>
#     dplyr::mutate(phase = cut(max_iter, breaks = c(0,20,40,60,80,100,300,900))) %>%
#     dplyr::group_by(phase) %>%
#     dplyr::mutate(t_within = scales::rescale(dplyr::row_number(), to = c(0, 1))) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(time_smooth = as.numeric(phase) + 0.1 * t_within)
# # dplyr::mutate(phase = dplyr::case_when(max_iter<50~"ph1",
# #                                        max_iter>50~"ph2",
# #                                        max_iter>100~))
#
# p <- ggplot2::ggplot(dat, ggplot2::aes(x = tSNE_1, y = tSNE_2)) +
#     ggplot2::geom_point() +
#     ggplot2::labs(title = "max_iter: {frame_time}") +
#     gganimate::transition_time(time_smooth) +
#     gganimate::view_follow(fixed_x = FALSE, fixed_y = FALSE)
# p
#
# summ <- dat |>
#     dplyr::summarise(t1min = min(tSNE_1), t1max = max(tSNE_1), t2min = min(tSNE_2), t2max = max(tSNE_2), .by = max_iter) |>
#     tidyr::pivot_longer(cols=-max_iter, names_to = "axis", values_to = "value")
#
# ggplot(summ, aes(x = max_iter, y = abs(value), color = axis)) +
#     geom_line(aes(group = axis)) +
#     scale_y_log10()
