
library(rvest)
library(RSelenium)
library(tidyverse)
library(XML)
library(httr)

remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444L
                      , browserName = "chrome")

# java -jar selenium-server-standalone-3.9.1.jar

remDr$open()

url <- "http://www.a-court.gov.cn/platformData/infoplat/pub/no1court_2802/cpws_32416/cpws_list.jsp"
remDr$navigate(url)

court_1 <- data.frame(page = 1:1208, stringsAsFactors = F)
court_1$title <- NA
court_1$links <- NA

for(i in 223:nrow(court_1)){
  
  Sys.sleep(2)
  trynext<- try({
      
    pagesource <- remDr$getPageSource()[[1]]
    html <- pagesource %>% read_html()

    court_1$links[i] <- html %>% html_nodes(xpath = '//*[@height="30"]//a')%>%html_attr('href')%>%list()
    court_1$title[i] <- html %>% html_nodes(xpath = '//*[@height="30"]//a')%>%html_text(.)%>%gsub("\n\t\t\t\t\t\t","",.)%>%list()
    
    b <- remDr$findElement("link text", "下一页")
    b$clickElement()
  })
  if ('try-error' %in% class(trynext)) next
  print(i)
  
}


########################## get content ##########################
court_1 <- readRDS('data/court1')
court_1_info <- data.frame(title=unlist(court_1$title),links=unlist(court_1$links),stringsAsFactors = F)

for(i in 11825:nrow(court_1_info)){
  
  Sys.sleep(0.8)
  trynext<- try({
    
    content_link <- paste0('http://www.a-court.gov.cn/platformData/infoplat/pub/no1court_2802/cpws_32416/',
       court_1_info$links[i])
    html <- read_html(content_link,encoding = 'GB18030')
    #html <- html_session(content_link, timeout(60),
    #                     (add_headers(`User-Agent`="mozilla/5.0 (Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.100 Safari/537.36")))
    court_1_info$content[i] <- html %>% html_node(xpath = '//div[@align="left"]')%>%html_text(.)
    court_1_info$judges[i] <- html %>% html_node(xpath = '//td[@class="smain"]')%>%html_text(.)%>%
      gsub(" ","",.)%>%str_extract(.,"\r\n\r\n[^&]{1,60}\r\n\r\n\r\n【关闭窗口】")%>%gsub("二[^&]{1,20}日\r","",.)
    court_1_info$pub_date[i] <- html %>% html_node(xpath = '//td[@class="smain"]')%>%html_text(.)%>%
      gsub(" ","",.)%>%str_extract(.,"\r\n\r\n[^&]{1,60}\r\n\r\n\r\n【关闭窗口】")%>%str_extract(.,"二[^&]{1,20}日\r")
    
    })
  if ('try-error' %in% class(trynext)) next
  print(i)
  
}

saveRDS(court_1_info,"data/court_1_info.rds")

