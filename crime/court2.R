
library(rvest);library(tidyverse);library(XML);library(RCurl)

pages <- paste0("http://www.shezfy.com/page/sssw/index.html?lm=d0&pm=d0&data_p=",1:6379)
court_2 <- data.frame(page = pages, stringsAsFactors = F)
court_2$title <- NA
court_2$number <- NA
court_2$links <- NA


for(i in 732:nrow(court_2)){
  
  Sys.sleep(1)
  trynext<- try({
  html <- html_session(court_2$page[i], timeout(60),
                       (add_headers(`User-Agent`="mozilla/5.0 (Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.100 Safari/537.36")))
  court_2$links[i] <- html %>% html_nodes(xpath = '//*[@class="content14"]')%>%html_attr('onclick')%>%list(.)
  court_2$number[i] <- html %>% html_nodes(xpath = '//*[@class="content14"]')%>%html_text()%>%list()
  court_2$title[i] <- html %>% html_nodes(xpath = '//*[@width="97%"]')%>%html_text(.)%>%gsub(" ","",.)%>%
    gsub("\r\n\t[^&]{1,20}\r\n\t","",.)%>%list()
  
  })
  if ('try-error' %in% class(trynext)) next
  print(i)
  
}

saveRDS(court_2,"data/court_2.rds")

court_2_info <- data.frame(number=unlist(court_2$number),title=unlist(court_2$title),link=unlist(court_2$links),stringsAsFactors = F)

for(i in 26017:nrow(court_2_info)){
  
  Sys.sleep(0.9)
  trynext<- try({
    
    content_link <- court_2_info$link[i]%>%gsub("[)']|javascript:open_flws\\('","",.)%>%paste0('http://www.shezfy.com',.)

    html <- read_html(content_link, encoding = 'UTF-8')
    # html <- html_session(content_link, timeout(60),
    #                     (add_headers(`User-Agent`="mozilla/5.0 (Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.100 Safari/537.36")))
    court_2_info$content[i] <- html %>% html_node('body > p:nth-child(7) > font')%>%html_text(.)
    court_2_info$judges[i] <- html %>% html_node('body > p:nth-child(9) > font')%>%html_text(.)
    court_2_info$pub_date[i] <- html %>% html_node('body > p:nth-child(10) > font')%>%html_text(.)
    court_2_info$secretary[i] <- html %>% html_node('body > p:nth-child(11) > font')%>%html_text(.)
  })
  if ('try-error' %in% class(trynext)) next
  print(i)
}

object.size(court_2_info)
saveRDS(court_2_info,"data/court_2_info.rds")
