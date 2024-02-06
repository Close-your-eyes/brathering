## get images from a video file

# simple example:
'av::av_video_images("/Volumes/GoPro64/DCIM/124GOPRO/GOPR0006.MP4",
                    destdir = "/Users/vonskopnik/Documents/20240127_Baum_Kai_img_seq",
                    format = "mjpeg",
                    fps = 1)'
formats <- av_encoders()
unique(formats$type)
# png is slow and huge compared to jpg

# use future for parallel processing of video files
# https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html

root_path <- "/Volumes/CMS_SSD_2TB/20240127_Baumfaellung"
mp4_files <- list.files(path = root_path,
                        pattern = "\\.mp4$",
                        ignore.case = T,
                        full.names = T)

# mtime is modified time (https://www.howtogeek.com/517098/linux-file-timestamps-explained-atime-mtime-and-ctime/)
# order files by mtime
mp4_files <- mp4_files[order(file.mtime(mp4_files))]
file.rename(mp4_files, new_names_in_order <- file.path(dirname(mp4_files), paste0(sprintf("%03d", seq_along(mp4_files)), "_", basename(mp4_files))))

mp4_files <- list.files(path = root_path,
                        pattern = "\\.mp4$",
                        ignore.case = T,
                        full.names = T)


image_folders <- file.path(root_path, sapply(strsplit(basename(mp4_files), "\\."), "[", 1))
for (i in image_folders) {
    dir.create(i, recursive = T, showWarnings = F)
}

## inside of av_video_images
'  stopifnot(length(video) == 1)
  vfilter <- ifelse(length(fps), paste0("fps=fps=", fps),
    "null")
  framerate <- av_media_info(video)$video$framerate
  dir.create(destdir)
  codec <- switch(format, jpeg = "mjpeg", jpg = "mjpeg", format)
  output <- file.path(destdir, paste0("image_%6d.", format))
  av_encode_video(input = video, output = output, framerate = framerate,
    codec = codec, vfilter = vfilter)
  list.files(destdir, pattern = paste0("image_\\d{6}.", format),
    full.names = TRUE)'

# manually write the command for av::av_encode_video
# implicit futures:
library(future)
'future::plan(future::multisession)
v %<-% av::av_encode_video(input = mp4_files[1],
                           output = file.path(image_folders[1], paste0("image_%6d.", "tiff")),
                           framerate = av::av_media_info(mp4_files[1])$video$framerate,
                           codec = "tiff",
                           vfilter = "fps=fps=1")
v2 %<-% av::av_encode_video(input = mp4_files[2],
                           output = file.path(image_folders[2], paste0("image_%6d.", "tiff")),
                           framerate = av::av_media_info(mp4_files[2])$video$framerate,
                           codec = "tiff",
                           vfilter = "fps=fps=1")
v3 %<-% av::av_encode_video(input = mp4_files[3],
                            output = file.path(image_folders[3], paste0("image_%6d.", "tiff")),
                            framerate = av::av_media_info(mp4_files[3])$video$framerate,
                            codec = "tiff",
                            vfilter = "fps=fps=1")
v4 %<-% av::av_encode_video(input = mp4_files[4],
                            output = file.path(image_folders[4], paste0("image_%6d.", "tiff")),
                            framerate = av::av_media_info(mp4_files[4])$video$framerate,
                            codec = "tiff",
                            vfilter = "fps=fps=1")
v5 %<-% av::av_encode_video(input = mp4_files[5],
                            output = file.path(image_folders[5], paste0("image_%6d.", "tiff")),
                            framerate = av::av_media_info(mp4_files[5])$video$framerate,
                            codec = "tiff",
                            vfilter = "fps=fps=1")
v6 %<-% av::av_encode_video(input = mp4_files[6],
                            output = file.path(image_folders[6], paste0("image_%6d.", "tiff")),
                            framerate = av::av_media_info(mp4_files[6])$video$framerate,
                            codec = "tiff",
                            vfilter = "fps=fps=1")'

# fps=fps=4 --> 4 images per second are saved to disk
## write it with furrr:
future::plan(future::multisession, workers = 8)
furrr::future_map2(mp4_files, image_folders, function(x,y) {
    av::av_encode_video(input = x,
                        output = file.path(y, paste0("image_%5d.", "png")),
                        framerate = av::av_media_info(x)$video$framerate,
                        codec = "png",
                        vfilter = "fps=fps=4")
})


# tiff: less compression --> larger files --> faster
# mjpeg: smallest file size but with a lot compression artifact
# jpeg2000 makes 2.3mb files; png makes 3.2 mb files
'av::av_video_images(mp4_files[1],
                    destdir = image_folders[1],
                    format = "jpg",
                    fps = 1)

av::av_encode_video(input = mp4_files[1],
                    output = file.path(image_folders[1], paste0("image_%8d.", "jpg")),
                    framerate = av::av_media_info(mp4_files[1])$video$framerate,
                    codec = "mjpeg",
                    vfilter = "fps=fps=1")

av::av_encode_video(input = mp4_files[1],
                    output = file.path(image_folders[1], paste0("image_%8d.", "jpg")),
                    framerate = av::av_media_info(mp4_files[1])$video$framerate,
                    codec = "jpeg2000",
                    vfilter = "fps=fps=1")

av::av_encode_video(input = mp4_files[1],
                    output = file.path(image_folders[1], paste0("image_%8d.", "tiff")),
                    framerate = av::av_media_info(mp4_files[1])$video$framerate,
                    codec = "tiff",
                    vfilter = "fps=fps=1")

av::av_encode_video(input = mp4_files[1],
                    output = file.path(image_folders[1], paste0("image_%7d.", "fits")),
                    framerate = av::av_media_info(mp4_files[1])$video$framerate,
                    codec = "fits",
                    vfilter = "fps=fps=1")

'

