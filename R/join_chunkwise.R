join_chunkwise <- function(data_frame_list,
                           join_fun = dplyr::full_join,
                           join_by,
                           chunks = 10,
                           max_final_size = 20) {
    # chunk wise joining can be much faster

    join_fun <- match.fun(join_fun)
    while (length(data_frame_list) > max_final_size) {
        data_frame_list <- purrr::map(split(c(1:length(data_frame_list)), ceiling(seq_along(c(1:length(data_frame_list)))/chunks)),
                          function(x) purrr::reduce(data_frame_list[x], join_fun, by = join_by))
    }
    return(data_frame_list)
}
