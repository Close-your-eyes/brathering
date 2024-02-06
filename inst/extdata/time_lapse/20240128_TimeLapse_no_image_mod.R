
library(exifr)
library(av)
library(gganimate)
#library(transformr)
#library(gifski)
library(lubridate)
library(magick)
library(pbapply)
library(parallel) # for mclapply
#library(animation)

# OCR in R:
#install.packages("tesseract")

# magick: https://cran.r-project.org/web/packages/magick/vignettes/intro.html#The_grid_package
# av: https://www.r-bloggers.com/2020/02/working-with-audio-in-r-using-av/, https://docs.ropensci.org/av/index.html

formats <- av_encoders()
unique(formats$type)

# image folder and duration of image sequence
root_path <- "/Volumes/CMS_SSD_2TB/20240127_Baumfaellung"
mp4_files <- list.files(path = root_path,
                        pattern = "\\.mp4$",
                        ignore.case = T,
                        full.names = T)

# mtime is modified time (https://www.howtogeek.com/517098/linux-file-timestamps-explained-atime-mtime-and-ctime/)
# order files by mtime
mp4_files <- mp4_files[order(file.mtime(mp4_files))]

image_folders <- file.path(root_path, sapply(strsplit(basename(mp4_files), "\\."), "[", 1))
images <- unlist(lapply(image_folders, list.files, full.names = T))

# select framerate and get length of output video in seconds
framerate <- 300
length(images)/framerate/60 # length in min


# optionally: select audio and cut sequence
#audio.in <- "/Users/vonskopnik/Desktop/1 _NSYNC - Bye Bye Bye.mp3"
#audio.out <- "/Users/vonskopnik/Desktop/1 _NSYNC - Bye Bye Bye short.mp3"
#av::av_audio_convert(audio.in, audio.out, start_time = 5, total_time = 5)


new.folder <- paste0(root_path, "/modified")
dir.create(new.folder)


# find time points of tree fell and increase number of frames there


# save time lapse video
av::av_encode_video(input = images[seq(1, length(images), 4)], # new.folder
                    output = paste0(new.folder, "/", "TimeLapse_video_one_fourth_of_img.mp4"),
                    framerate = framerate,
                    verbose = F) #  audio = audio.out)

av::av_video_info("/Volumes/CMS_SSD_2TB/20240127_Baumfaellung/modified/TimeLapse_video.mp4")$video$framerate


## attention: do not play with QuickTime player. Quicktime decided himself to reduce the playback speed since it thinks it is a slow motion video
## hence the 1:25 video becomes 8:15 min (or so)
