
library(tesseract);library(magick)
eng <- tesseract("eng")

codeElem <- remDr$findElement('id','imageCode')
remDr$screenshot(display = F,file = "C:/Users/WXY/Documents/R_learning/judgement/data/code.png")

input <- image_read("data/code.png")
imag <- input %>%image_crop(., "118x50+652+220")
restrict <- tesseract(options = list(tessedit_char_whitelist = "[a-z]|0123456789"))
text <- imag %>%
  image_convert(type = 'Grayscale') %>%
  image_write(format = 'png', density = '300x300') %>%
  tesseract::ocr(.,engine = restrict) 

cat(text)
