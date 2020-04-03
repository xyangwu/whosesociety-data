
pacman::p_load(openxlsx, tidyverse)

options(scipen = 999)
setwd("C:/Users/WXY/Documents/R_learning/whosedata/migrant/baidu_migrant")

##################### load in data #####################

## city location
city_location <- readRDS("C:/Users/WXY/Documents/data/DATA_R/city_location.rds")

## read citys' name and code
city_list <- read.xlsx("citylist.xlsx", sheet = 1)

############## get population flow intensity ################

flow_intensity <- NULL
file.intensity <- list.files("baidu_migrant_index/")

for(i in 1:nrow(city_list)){
  citycode  = city_list$city_code[i]
  try({
    data <- paste0("http://huiyan.baidu.com/migration/historycurve.jsonp?dt=province&id=", citycode, "&type=move_in") %>%
      readLines(encoding = "utf-8") %>% 
      gsub("cb\\(|\\)", "", .) %>%
      jsonlite::fromJSON(., simplifyDataFrame = T)
    data = data.frame(date = names(data$data$list), index = data$data$list %>% unlist(.), row.names = NULL)
    data$city_name <- city_add$city_name[i]
    flow_intensity <- rbind(flow_intensity, data)
    })
  paste0(i,"-", city_list$city_name[i]) %>% print()
}

# city that missing in flow_intensity"4-巢湖"
city_list[, 2:3][!city_list$city_name%in%unique(flow_intensity$city_name), ]

write.xlsx(flow_intensity, "flow_intensity.xlsx")

################## get population flow percent data ####################

city_data <- NULL

for(i in 1:nrow(city_list)){
  citycode  = city_list$city_code[i]
  try({
    data <- paste0("http://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=", citycode, "&type=move_in&date=3") %>%
      readLines(encoding = "utf-8") %>% 
      gsub("cb\\(|\\)", "", .) %>%
      jsonlite::fromJSON(.)
    data <- data$data$list
    data$city_to <- city_list$city_name[i]
    city_data <- rbind(city_data, data)
  })
    paste0(i,"-", city_list$city_name[i]) %>% print()
}

# data that missing "4-巢湖"
city_list[, 2:3][!city_list$city_name%in%unique(city_data$city_to), ]

write.xlsx(city_data, "city_data.xlsx")

##################### coordinates of citys #####################

city_list <- dplyr::left_join(city_list, city_location[,c('city','long.2','lat.2')] %>% 
                                mutate(city = gsub("市", "", city)),
                              by=c('city_name'='city'))

names(city_list)[4:5] <- c("long","lat")

# find citys cannot matched with coordinate
city_list[is.na(city_list$long), ]

## get coordinates with baidumap
pacman::p_load(baidumap)
options(baidumap.key = 'YourAPIkey')
for (i in 1:nrow(city_add)) {
  
  city = city_add$city_name[i]
  trynext <- try({
    city_add[i, c('long', 'lat')] <- getPlace(city, pages = 1)[1, c('lon', 'lat')]
  }) 
  if('try-error' %in% class(trynext)) next
  }

# cannot be located:  4:"巢湖市", 320:"神农架地区"
city_list[320, 4:5] <- c(110.520296, 31.649129)

# join province name
city_list$city_name <- gsub("地区", "", city_list$city_name)
city_list <- left_join(city_list, city_data[!duplicated(city_data[,1:2]), 1:2] %>%
                         mutate(city_name = gsub("市", "", city_name)), 
                       by = c("city_name" = "city_name"))
which(is.na(city_list$province_name))
city_list <- city_list[!duplicated(city_list$city_name), ]

write.csv(city_list, "city_list.csv")

city_list$city_name <- as.character(city_list$city_name)

