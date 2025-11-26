#' Images to pptx file
#'
#' @param img_paths paths to files on disk, filtered for pdf, png, tiff, bmp,
#' jpg, jpeg
#' @param pptx_path path to save pptx to
#'
#' @returns nothing
#' @export
#'
#' @examples
#' \dontrun{
#' img_to_pptx(imgs[-1], pptx_path = "/Users/chris/Documents/2025_DRFZ_Daten/20250823_GZMB_GZMK_induction/R_images/img_to_pptx.pptx")
#' }
img_to_pptx <- function(img_paths,
                        pptx_path) {

    if (!requireNamespace("officer", quietly = T)) {
        utils::install.packages("officer")
    }

    ppt <- officer::read_pptx()
    for (i in seq_along(img_paths)) {
        if (fs::is_dir(img_paths[i])) {
            message("skipped ", img_paths[i])
            next
        }
        if (!tolower(tools::file_ext(img_paths[i])) %in% c("pdf", "png", "bmp", "tiff", "jpg", "jpeg")) {
            message("skipped ", img_paths[i])
            next
        }
        readfun <- ifelse(tools::file_ext(img_paths[i]) == "pdf", magick::image_read_pdf, magick::image_read)
        img_info <- magick::image_info(readfun(img_paths[i]))
        wid <- 8
        hei <- wid*img_info$height/img_info$width
        if (hei > 7) {
            hei <- 7
            wid <- hei*img_info$width/img_info$height
        }
        ppt <- ppt |>
            officer::add_slide(layout = "Title and Content", master = "Office Theme") |>
            officer::ph_with(
                officer::external_img(img_paths[i], width = wid, height = hei),
                use_loc_size = F,
                location = officer::ph_location_fullsize())
    }
    dir.create(dirname(pptx_path), recursive = T, showWarnings = F)
    print(ppt, target = pptx_path)
}

