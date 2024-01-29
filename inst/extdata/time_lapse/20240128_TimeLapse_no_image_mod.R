
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



# image folder and duration of image sequence
image.folder <- "/Users/vonskopnik/Documents/20240127_Baum_Kai_img_seq"
datetimes <- lubridate::as_datetime(exifr::read_exif(list.files(image.folder, full.names = T), tags = "CreateDate")$CreateDate)
max(datetimes) - min(datetimes)

#get image info: magick::image_info(img)

# select framerate and get length of output video in seconds
framerate <- 30
length(datetimes)/framerate

# optionally: select audio and cut sequence
audio.in <- "/Users/vonskopnik/Desktop/1 _NSYNC - Bye Bye Bye.mp3"
audio.out <- "/Users/vonskopnik/Desktop/1 _NSYNC - Bye Bye Bye short.mp3"
av::av_audio_convert(audio.in, audio.out, start_time = 5, total_time = 5)

# optionally: add text annotation to images (e.g. CreateDate)
# caution: this is very slow and may be sped up with mclapply - maybe there is another option to speed it up
# it was also attempted to not save the new images in between and to use image_write_video from an vector obejct of img, but it did not work
new.folder <- paste0(image.folder, "/modified")
dir.create(new.folder)

mclapply(list.files(image.folder, full.names = T, pattern = ".jpg"), function(x) {
  magick::image_write(magick::image_annotate(
    # optionally rezise images; use quotations marks to define new size
    # make sure that height (and width?!) is divisible by 2; required for for av::av_encode_video below!!
    magick::image_resize(magick::image_read(x), "804x"),
    text = lubridate::as_datetime(exifr::read_exif(x, tags = "CreateDate")$CreateDate),
    size = 40,
    color = "white",
    gravity = "southwest",
    location = paste0("+50+50"),
    font = "Courier"),
    path = file.path(new.folder, basename(x)),
    format = "png")
}, mc.cores = 2)

# save time lapse video
av::av_encode_video(input = list.files(new.folder, full.names = T, pattern = ".jpg"), # new.folder
                    output = paste0(new.folder, "/", "TimeLapse_video.mp4"),
                    framerate = framerate) #  audio = audio.out)



# vectorzied images (~video)
img <- image_read(list.files(image.folder, pattern = "\\.JPG", full.names = T))

# Customize text
img <- image_annotate(
  img,
  text = lubridate::as_datetime(exifr::read_exif(list.files(image.folder, pattern = "\\.JPG", full.names = T), tags = "CreateDate")$CreateDate),
  size = 40,
  color = "white",
  gravity = "southwest",
  location = paste0("+50+50"),
  font = "Courier"
)


image_write(img[3], path = "/Users/vonskopnik/Desktop/img3.png", format = "png")

##### does not work:
image_write_video(img, path = "/Users/vonskopnik/Desktop/out3.mp4", framerate = framerate)


