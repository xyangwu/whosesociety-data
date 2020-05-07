# # # # # # # # # # # # # # # # # # # # # #
################ {r packages} #############
# # # # # # # # # # # # # # # # # # # # # #

library(RSQLite)
library(tidyverse)
library(httr)
library(rvest)

tids <- openxlsx::read.xlsx("data/tids.xlsx", sheet = 1)

# # # # # # # # # # # # # # # # # # # # # # # # #
################# set parameter #################
# # # # # # # # # # # # # # # # # # # # # # # # #

cookies = "CURRENT_FNVAL=16; _uuid=E9A9643E-1B2F-5E8C-8DF9-D8629D9B60E412446infoc; LIVE_BUVID=AUTO2515860900164163; buvid3=C8A8993C-6B3B-4AD4-8762-CBDDF16B4ECB155821infoc; rpdid=|(umRk)~Yuku0J'ul)Y|m||m|; bfe_id=393becc67cde8e85697ff111d724b3c8; sid=b89bxm0y; DedeUserID=21723848; DedeUserID__ckMd5=c3aa6101b1a1a987; SESSDATA=f67e4a64%2C1603802125%2C8be00*41; bili_jct=f93765e3541d410aaca08131b09bc6fd"
header = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36'
options(add_headers(header))
options(set_cookies(cookies))

url = paste0("https://search.bilibili.com/all?keyword=%E5%86%9C%E6%9D%91",
             "&order=", "click",
             "&duration=", 1, # 1: <10m,
             "&tids_1=", tids$tids1[i],
             "&tids_2=", tids$tids2[i])

session <- url %>%
  html_session(., add_headers(header), set_cookies(cookies))

finalp = session %>%
  html_nodes(., xpath = '//*[@class="pagination-btn"]') %>%
  html_text() %>%
  str_trim() %>%
  as.numeric()
if(is_empty(finalp)){
  print("Only 1 page.")
  finalp = 1}else{
    if(finalp!=50){print(paste0(finalp, "--Return less than 50 pages!"))}
  }

row.names(data_video)<- 1:nrow(data_video)


# # # # # # # # # # # # # # # # # # # # # # # # #
################# get urls of video #############
# # # # # # # # # BV id   # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # #

for (i in 1:finalp) {
  
  session <- paste0(url, "&page=", i) %>% 
    html_session(., add_headers(header), set_cookies(cookies))
  
  Sys.sleep(1)
  
  title = session %>%
    html_nodes(., css = '#all-list > div.flow-loader > ul > li > div > div.headline.clearfix > a') %>%
    html_text()
  
  tag = session %>%
    html_nodes(., '#all-list > div.flow-loader > ul > li > div > div.headline.clearfix > span') %>%
    html_text()
  
  views = session %>%
    html_nodes(., '#all-list > div.flow-loader > ul > li > div > div.tags > span.so-icon.watch-num') %>%
    html_text() %>%
    str_trim()
  
  danmu = session %>%
    html_nodes(., '#all-list > div.flow-loader > ul > li > div > div.tags > span.so-icon.hide') %>%
    html_text() %>%
    str_trim()
  
  duration = session %>%
    html_nodes(., '#all-list > div.flow-loader > ul > li > a > div > span.so-imgTag_rb') %>%
    html_text()
  
  dates = session %>%
    html_nodes(., '#all-list > div.flow-loader > ul > li > div > div.tags > span.so-icon.time') %>%
    html_text() %>%
    str_trim()
  
  urls = session %>%
    html_nodes(., css = '#all-list > div.flow-loader > ul > li > div > div.headline.clearfix > a') %>%
    html_attr("href") %>% gsub("from=search", "", .)
  
  uper = session %>%
    html_nodes(., '#all-list > div.flow-loader > ul > li > div > div.tags > span:nth-child(4) > a') %>%
    html_text()
  
  uper_id = session %>%
    html_nodes(., '#all-list > div.flow-loader > ul > li > div > div.tags > span:nth-child(4) > a') %>%
    html_attr("href") %>%
    gsub("from=search.+|from=search", "", .) %>%
    paste0("https:", .)
  
  data_page = cbind(title, tag, views, danmu, duration, dates, urls, uper, uper_id, filter = tids[length(tids)])
  data_video = rbind(data_video, data_page) %>% as.data.frame()
  
  print(i)
}

for (i in 1:4) {
  url = paste0("https://search.bilibili.com/all?keyword=%E5%86%9C%E6%9D%91",
               "&order=", "click",
               "&duration=", i)
  
  for (j in area) {
    
  }
}

saveRDS(data_video, "C:/Users/WXY/Documents/R_learning/atoolbox/data/data_video.rds")

openxlsx::write.xlsx(data_video, "C:/Users/WXY/Documents/R_learning/atoolbox/data/data_video.xlsx")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
###################### get detailed info ####################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#saveRDS(data_video[1351:nrow(data_video)], "data/data_videoinfo_1351.rds")
data_video <- data_video[1351:nrow(data_video), ]
data_video <- data_video_1_19011
length(which(duplicated(data_video$cid)))

for (i in which(is.na(data_video$cid))) {
  Sys.sleep(1)
  url_bv = data_video$urls[i]

  session <- url_bv %>%
    html_session(., add_headers(header), set_cookies(cookies))
  
  # # # # # # # # # # # # # # # # # # # # # # # # #
  # return no value if video is missing/deleted   #
  # otherwise, collect data of video.             #
  # # # # # # # # # # # # # # # # # # # # # # # # #
  try({ # 
    error = session %>%
    html_nodes(., css = '#app > div > div > div.error-panel > div.error-msg > div.error-prompt > div') %>%
    html_text()
    if(is_empty(error)){
      error="Not Missing"
    }
    })
  
  if(error=="啊叻？视频不见了？"){
    data_video[i, c(11:28)] <- error
    print(error)
  }else{

    # release time
    data_video$time_rel[i] = session %>%
    html_nodes(., css = '#viewbox_report > div:nth-child(2) > span:nth-child(2)') %>%
    html_text()
 
    data_video$tags[i] = session %>% 
      html_nodes(., '#v_tag > ul > li') %>%
      html_text() %>% list()
    data_video$intro[i] = session %>% 
      html_nodes(., '#v_desc > div') %>%
      html_text()
    data_video$fan[i] = session %>% 
      html_nodes(., '#v_upinfo > div.btn-panel > div.default-btn.follow-btn.b-gz.not-follow > span > span') %>%
      html_text() %>%
    ifelse(is_empty(.), 0, .)

    data_video$av_id[i] = session %>% 
      html_nodes(., 'head > meta:nth-child(11)') %>%
      html_attr("content") %>%
      str_extract(., "av.+") %>%
      gsub("av|/", "", .)
    
    data_video$cid[i] = session %>% 
      html_nodes(., xpath = '/html/head/script[3]/text()') %>%
      html_text() %>%
      str_extract(., "upgcxcode[^&]{1,17}") %>%
      str_sub(., start =  17) %>%
      gsub("/.+|/", "", .)
    if(is.na(data_video$cid[i])){
      data_video$cid[i] = session %>% 
        html_nodes(., xpath = '/html/head/script[3]/text()') %>%
        html_text() %>%
        str_extract(., "cid[^&]{1,10}") %>%
        str_sub(., start =  6) %>%
        gsub("/.+|/", "", .)
    }
    
    data_video$recommend[i] = session %>% 
      html_nodes(., '#reco_list > div.rec-list > div> div > div.info > a') %>%
      html_attr('href') %>% list()

  # # # # # # # # # # # # # # # # # # # #
  # video info: views, likes, danmu...  #
  # # # # # # # # # # # # # # # # # # # #
    Sys.sleep(1)
    info_json <- paste0('https://api.bilibili.com/x/web-interface/archive/stat?aid=', data_video$av_id[i]) %>%
      jsonlite::read_json(., simplifyVector = T)
  
    data_video$views[i] = info_json$data$view
    data_video$danmu[i] = info_json$data$danmaku
    data_video$like[i] = info_json$data$like
    data_video$coin[i] = info_json$data$coin
    data_video$favorite[i] = info_json$data$favorite
    #data_video$reply[i] = info_json$data$reply
    data_video$share[i] = info_json$data$share
    data_video$now_rank[i] = info_json$data$now_rank
    data_video$his_rank[i] = info_json$data$his_rank
  
  Sys.sleep(1)
    url_comment = paste0("http://api.bilibili.com/x/reply?jsonp=jsonp&type=1&sort=0&pn=1&nohot=0&oid=", data_video$av_id[i])
    json_data <- jsonlite::read_json(url_comment, simplifyVector = T)
    data_video$comments_main[i] = ifelse(is.null(json_data$data$page$count), 0, json_data$data$page$count)
    data_video$comments[i] = ifelse(is.null(json_data$data$page$acount), 0, json_data$data$page$acount)
    data_video$comment_page[i] = ifelse(is_empty(ceiling(json_data$data$page$count/20)), 0, ceiling(json_data$data$page$count/20))
  
    if(data_video$danmu[i]=="0"){
      # if there is no danmu in this video
        data_video$danmuku[i] <- NA
      }else{
        Sys.sleep(1)
        url_danmu = paste0("https://comment.bilibili.com/", data_video$cid[i],".xml")
        tryurl <- try({ danmu_xml <- read_xml(url_danmu, encoding = "UTF-8") })
        danmu_text <- danmu_xml %>%
          html_nodes("d") %>%
          html_text()
        danmu_attr <- danmu_xml %>%
          html_nodes("d") %>%
          html_attr("p")
        data_video$danmuku[i] <- data.frame(danmu_text = danmu_text, danmu_attr = danmu_attr, stringsAsFactors = F) %>% list()
      }
    print(i)
  }
}

for (i in which(data_video$av_id%in%c(which(is.na(dt_video_2max$danmuku)) %>% dt_video_2max$av_id[.]))) {
  Sys.sleep(1)
  url_danmu = paste0("https://comment.bilibili.com/", data_video$cid[i],".xml")
  tryurl <- try({ danmu_xml <- read_xml(url_danmu, encoding = "UTF-8") })
  danmu_text <- danmu_xml %>%
    html_nodes("d") %>%
    html_text()
  danmu_attr <- danmu_xml %>%
    html_nodes("d") %>%
    html_attr("p")
  data_video$danmuku[i] <- data.frame(danmu_text = danmu_text, danmu_attr = danmu_attr, stringsAsFactors = F) %>% list()
  print(i)
}

############## save file ################

data_video_1 <- readRDS("data/data_video.rds")
data_video_1_1350 <- readRDS("data/data_videoinfo.rds")[1:1350, ]
data_video_1_15000 <- rbind(data_video_1_1350, data_video)[1:15000, ]
data_video_15001_19011 <- readRDS("data/data_videoinfo_15001_19011.rds")
data_video_1_19011 <- rbind(data_video_1_15000, data_video_15001_19011)


saveRDS(data_video, "data/data_video_2.rds")
openxlsx::write.xlsx(data_video, "data/data_video_2.xlsx")


