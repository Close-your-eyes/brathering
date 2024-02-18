
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

# every 20th image
images_sub <- images[seq(1, length(images), 20)]
new.folder <- paste0(root_path, "/modified")
dir.create(new.folder)

mclapply(seq_along(images_sub), function(x) {
    magick::image_write(magick::image_annotate(
        # optionally rezise images; use quotations marks to define new size
        # make sure that height (and width?!) is divisible by 2; required for for av::av_encode_video below!!
        #magick::image_resize(magick::image_read(x), "804x"),
        #text = lubridate::as_datetime(exifr::read_exif(x, tags = "CreateDate")$CreateDate),
        image = magick::image_read(images_sub[x]),
        text = paste0("frame:", x),
        size = 40,
        color = "white",
        gravity = "southwest",
        location = paste0("+50+50"),
        font = "Courier"),
        path = file.path(new.folder, paste0(basename(dirname(images_sub[x])), "_", basename(images_sub[x]))),
        format = "png")
}, mc.cores = 8)



# optionally: select audio and cut sequence
#audio.in <- "/Users/vonskopnik/Desktop/1 _NSYNC - Bye Bye Bye.mp3"
#audio.out <- "/Users/vonskopnik/Desktop/1 _NSYNC - Bye Bye Bye short.mp3"
#av::av_audio_convert(audio.in, audio.out, start_time = 5, total_time = 5)

images2 <- list.files("/Volumes/CMS_SSD_2TB/20240127_Baumfaellung/modified", pattern = "\\.png", full.names = T)
# select framerate and get length of output video in seconds
framerate <- 300
length(images2)/framerate/60 # length in min

# save time lapse video
av::av_encode_video(input = images2, # new.folder
                    output = paste0(new.folder, "/", "TimeLapse_search_Faellung_with_frames.mp4"),
                    framerate = framerate,
                    verbose = T) #  audio = audio.out)
av::av_video_info("/Volumes/CMS_SSD_2TB/20240127_Baumfaellung/modified/TimeLapse_video.mp4")$video$framerate


# find time points of tree fell in video with every 20th image
# around these images: increase frame rate
# approx. frames:
# 770
# 1500
# 3110
# 3135

## test it with first tree fell only
# 770*20
# every 20th image until 760, then every 2nd image
images_select1 <- images[1:(760*20)][seq(1, (760*20), 20)]
images_select2 <- images[((760*20)+1):((860*20))]
images_select2 <- images_select2[seq(1, length(images_select2), 1)]
new.folder <- paste0(root_path, "/modified")
dir.create(new.folder)

av::av_encode_video(input = c(images_select1, images_select2), # new.folder
                    output = paste0(new.folder, "/", "TimeLapse_first_fell_slowed.mp4"),
                    framerate = 200,
                    verbose = T)



## slow down video around tree fells
one_in_20_frames <- c(770, 1500, 3110, 3135)
original_frames <- one_in_20_frames*20
slow_range <- 400
default_image_gap <- 20
slow_image_gap <- 1
framerate <- 300


images_sub_01 <- images[seq(1,original_frames[1]-slow_range,default_image_gap)]
images_sub_02 <- images[seq(original_frames[1]-slow_range+1,original_frames[1]+slow_range,slow_image_gap)]
images_sub_03 <- images[seq(original_frames[1]+slow_range+1,original_frames[2]-slow_range,default_image_gap)]
images_sub_04 <- images[seq(original_frames[2]-slow_range+1,original_frames[2]+slow_range,slow_image_gap)]
images_sub_05 <- images[seq(original_frames[2]+slow_range+1,original_frames[3]-slow_range,default_image_gap)]
images_sub_06 <- images[seq(original_frames[3]-slow_range+1,original_frames[4]+slow_range,slow_image_gap)]
# tree 3 and 4 are overlapping with slow_range
#images_sub_07 <- images[seq(original_frames[3]+slow_range+1,original_frames[4]-slow_range,default_image_gap)]
#images_sub_08 <- images[seq(original_frames[4]-slow_range+1,original_frames[4]+slow_range,slow_image_gap)]
images_sub_09 <- images[seq(original_frames[4]+slow_range+1,length(images),default_image_gap)]

image_final <- c(images_sub_01,
                 images_sub_02,
                 images_sub_03,
                 images_sub_04,
                 images_sub_05,
                 images_sub_06,
                 images_sub_09)

av::av_encode_video(input = image_final, # new.folder
                    output = paste0(new.folder, "/", "TimeLapse_all_fell_slowed.mp4"),
                    framerate = framerate,
                    verbose = T)

'## add frame number to video
av::av_encode_video(input = images[seq(1, length(images), 20)], # new.folder
                    output = paste0(new.folder, "/", "TimeLapse_search_Faellung.mp4"),
                    framerate = framerate,
                    verbose = T) #  audio = audio.out)

av::av_video_info("/Volumes/CMS_SSD_2TB/20240127_Baumfaellung/modified/TimeLapse_video.mp4")$video$framerate
'

## attention: do not play with QuickTime player. Quicktime decided himself to reduce the playback speed since it thinks it is a slow motion video
## hence the 1:25 video becomes 8:15 min (or so)





