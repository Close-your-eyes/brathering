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

# tiff: less compression --> larger files --> faster
av::av_video_images(mp4_files[1],
                    destdir = image_folders[1],
                    format = "jpg",
                    fps = 1)

av_encode_video(input = mp4_files[1],
                output = file.path(image_folders[1], paste0("image_%8d.", "jpg")),
                framerate = av::av_media_info(mp4_files[1])$video$framerate,
                codec = "mjpeg",
                vfilter = "fps=fps=1")

av_encode_video(input = mp4_files[1],
                output = file.path(image_folders[1], paste0("image_%8d.", "jpg")),
                framerate = av::av_media_info(mp4_files[1])$video$framerate,
                codec = "jpeg2000",
                vfilter = "fps=fps=1")

av_encode_video(input = mp4_files[1],
                output = file.path(image_folders[1], paste0("image_%8d.", "tiff")),
                framerate = av::av_media_info(mp4_files[1])$video$framerate,
                codec = "tiff",
                vfilter = "fps=fps=1")

av_encode_video(input = mp4_files[1],
                output = file.path(image_folders[1], paste0("image_%7d.", "fits")),
                framerate = av::av_media_info(mp4_files[1])$video$framerate,
                codec = "fits",
                vfilter = "fps=fps=1")



