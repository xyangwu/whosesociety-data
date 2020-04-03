library(rvest)
library(RSelenium)
library(tidyverse)
library(XML)

remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444L
                      , browserName = "chrome")

# java -jar selenium-server-standalone-3.9.1.jar
# java -jar selenium-server-standalone-3.141.59.jar
remDr$open()

# 18767308370 mypasswordwu

'http://www.hshfy.sh.cn/shfy/gweb2017/flws_view.jsp?pa='
url <- 'http://www.hshfy.sh.cn/shfy/gweb2017/flws_list.jsp?'
remDr$navigate(url)
################### test javascripts ##################
for(j in 1:2){      
  remDr$executeScript(paste("scroll(0,",1*6000,");"))
  Sys.sleep(1)
}

remDr$executeScript("javascript:goPage('6')")
remDr$executeScript('return document.getElementById("imageCode");')
remDr$executeScript('mini.hideMessageBox(messageBox);') # close the box
remDr$executeScript('return imageCodeLoad()')
remDr$executeScript('returndocument.getElementsByTagName("script")') # show the input code

remDr$executeScript('return $("#userCode").val()') # show the input code

scripts <- "window.scrollTo(0, document.body.scrollHeight)"
remDr$executeScript(scripts)

remDr$setWindowSize(200,200)
remDr$screenshot_as_file(display = TRUE)
driver.get_screenshot_as_file('.//1.png')

ahElem <- remDr$findElement(using = "css", '#ah\\$value') # 案号
ahElem$sendKeysToElement(list("nmsl", key = "enter"))
jarqksElem <- remDr$findElement(using = "css", '#jarqks')

remDr$executeScript('mini.getbyName("jarqks").setValue("2001-01-02")') # 开始时间
remDr$executeScript('mini.getbyName("jarqjs").setValue("2019-11-25")') # 截至时间


########################## scrape selenium ###############################

judgement <- data.frame(page = seq(1,18342,1), stringsAsFactors = F)
judgement$table <- NA
judgement$link <- NA

for(i in 67:nrow(judgement)){ #######input page numnber
    
    remDr$executeScript(paste0("javascript:goPage('",i,"')"))
    Sys.sleep(2.5)
    
    trynext<- try({
      
      pagesource <- remDr$getPageSource()[[1]]
      
      html <- pagesource %>% read_html()
      
      curpage <- html %>% html_nodes("span.current") %>%html_text()

      print(paste(curpage, nrow(judgement), sep="-"))
      
      remDr$close()
      remDr$open();remDr$navigate(url)
      judgement$table[i] <- html %>% html_nodes("#flws_list_content > table") %>%html_table()
      judgement$link[i] <- html %>% html_nodes("#flws_list_content > table:nth-child(1) > tbody:nth-child(1) tr") %>%
        html_attr('onclick') %>% list()

      #next_leng <- html %>% html_nodes(".meneame a") %>% length()
      #next_elemt <- paste0('/html/body/div[3]/form/center/div/a[', next_leng - 1, ']')
      
      #b <- remDr$findElement('xpath', next_elemt)
      #b$clickElement()
    })
    if ('try-error' %in% class(trynext)) next
    print(i)
    
  }


judgement_injury_df <- cbind.data.frame(do.call(rbind.data.frame, judgement_injury$table),
                                       link = unlist(judgement_injury$link))

names(judgement_injury_df) <- c(judgement_injury_df[1,][1:7], "link")
judgement_injury_df <- judgement_injury_df[-which(judgement_injury_df[,1]=='案号'), ]
rownames(judgement_injury_df) <- 1:nrow(judgement_injury_df)
judgement_injury_df$link <- as.character(judgement_injury_df$link) %>% str_extract_all(., '\\([^&].+\\)') %>% gsub("\\(|\\)|'",'',.)
saveRDS(judgement_injury_df, 'data/judgement_injury_df.rds')

## get content info --------------------------------------------------------
library(httr)
judgement_hoju_df <- rbind.data.frame(judgement_homicide_df, judgement_injury_df, stringsAsFactors = F)
judgement_hoju_df$content <- NA
for(i in 317:nrow(judgement_hoju_df)){
  
  Sys.sleep(3)
  
  trynext<- try({
    
    #search <- remDr$findElement('xpath', '//*[@id="ah$value"]')
    #search$sendKeysToElement(list(judgement_rape_df$案号[i]))
    #search_click <- remDr$findElement('xpath', '/html/body/div[3]/form/table/tbody/tr[2]/td[2]/img[1]')
    #search_click$clickElement()
    jud_page <- paste0('http://www.hshfy.sh.cn/shfy/gweb2017/flws_view.jsp?pa=', judgement_hoju_df$link[i])
    
    #Sys.sleep(3)
    
    #pagesource <- remDr$getPageSource()[[1]]
    #html <- pagesource %>% read_html()
    #html <- read_html(jud_page)
    html <- html_session(jud_page, timeout(60),(add_headers(`User-Agent`="mozilla/5.0 (Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.100 Safari/537.36")))
    
    judgement_hoju_df$content[i] <- html %>% html_node('#wsTable') %>% html_text()
    
    #b <- remDr$findElement('xpath', next_elemt)
    
    #b$clickElement()
  })
  if ('try-error' %in% class(trynext)) next
  print(i)
  
}

saveRDS(judgement_hoju_df, 'data/judgement_hoju_df.rds')

# 中国法律文书网 china legal instrument #####################################

library(rvest);library(tidyverse)
#wenshu_url <- 'http://wenshu.court.gov.cn/website/wenshu/181107ANFZ0BXSK4/index.html?docId=038eb438363a47c48b54aac400e055bc'
#lawsdata_url <- "http://www.lawsdata.com/?q=eyJtIjoiYWR2YW5jZSIsImEiOnsiY2FzZVR5cGUiOlsiMSJdLCJpbnN0cnVtZW50VHlwZUlkIjpbIjEiXSwicmVhc29uSWQiOlsiMDAxMDA0IiwiMDAxMDA1Il0sImZ1enp5TWVhc3VyZSI6IjAiLCJyZWFzb24iOiLkvrXniq/lhazmsJHkurrouqvmnYPliKnjgIHmsJHkuLvmnYPliKks5L6154qv6LSi5LqnIn0sInNtIjp7InRleHRTZWFyY2giOlsic2luZ2xlIl0sImxpdGlnYW50U2VhcmNoIjpbInBhcmFncmFwaCJdfX0=&s="
lawsdata_url <- "http://www.lawsdata.com/?q=eyJtIjoiYWR2YW5jZSIsImEiOnsiY2FzZVR5cGUiOlsiMSJdLCJpbnN0cnVtZW50VHlwZUlkIjpbIjEiXSwicmVhc29uSWQiOlsiMDAxMDA0IiwiMDAxMDA1Il0sInByb3ZpbmNlSWQiOlsiNzA5MiJdLCJjb3VydElkIjpbIjcwOTIiXSwiZnV6enlNZWFzdXJlIjoiMCIsInJlYXNvbiI6IuS+teeKr+WFrOawkeS6uui6q+adg+WIqeOAgeawkeS4u+adg+WIqSzkvrXniq/otKLkuqciLCJjb3VydCI6IuS4iua1t+W4giJ9LCJzbSI6eyJ0ZXh0U2VhcmNoIjpbInNpbmdsZSJdLCJsaXRpZ2FudFNlYXJjaCI6WyJwYXJhZ3JhcGgiXX19&s="
lawsdata_url2 <- "http://www.lawsdata.com/?q=eyJtIjoiYWR2YW5jZSIsImEiOnsiY2FzZVR5cGUiOlsiMSJdLCJpbnN0cnVtZW50VHlwZUlkIjpbIjEiXSwicmVhc29uSWQiOlsiMDAxMDA0IiwiMDAxMDA1Il0sInByb3ZpbmNlSWQiOlsiNzA5MiJdLCJjb3VydElkIjpbIjcwOTIiXSwiZnV6enlNZWFzdXJlIjoiMCIsInJlYXNvbiI6IuS+teeKr+WFrOawkeS6uui6q+adg+WIqeOAgeawkeS4u+adg+WIqSzkvrXniq/otKLkuqciLCJjb3VydCI6IuS4iua1t+W4giJ9LCJzbSI6eyJ0ZXh0U2VhcmNoIjpbInNpbmdsZSJdLCJsaXRpZ2FudFNlYXJjaCI6WyJwYXJhZ3JhcGgiXX19&s="
remDr$navigate(lawsdata_url2) # increasing time
remDr$maxWindowSize()
remDr$screenshot(display = TRUE)

judge_ls <- data.frame(page = seq(1,6000,1), stringsAsFactors = F)
judge_ls$title <- NA
judge_ls$court <- NA
judge_ls$type <- NA
judge_ls$content <- NA
judge_ls$link <- NA

for(i in 51:nrow(judge_ls)){
  
  Sys.sleep(2)
  
  # log in
  login <- try(remDr$findElement(using = "css", "#mobile"),silent = T)
  if("try-error"%in%class(login)==FALSE){
    logtElem <- remDr$findElement("xpath", '//*[@class="mt-checkbox mt-checkbox-outline"]')
    logtElem$clickElement()
    mobiElem <- remDr$findElement(using = "css", "#mobile")
    mobiElem$sendKeysToElement(list("18767308370", key = "enter"))
    passElem <- remDr$findElement(using = "css", "#password")
    passElem$sendKeysToElement(list("18767308370", key = "enter"))
    
  }
  #Sys.sleep(1)
  # time decreasing
  #down <- try(remDr$findElement("xpath", '//*[@class="fa fa-arrow-down"]'))
  #if("try-error"%in%class(down)){
   #sortElem <- remDr$findElement("xpath", '//span[@data-sort-field="judgementDate"]')
   #sortElem$clickElement()
  #}
  
  trynext<- try({
    
    PageSource <- remDr$getPageSource()[[1]]
    html <- PageSource %>% read_html()

    #resultList <- html %>% html_node("#resultListDiv > div:nth-child(2) > div") %>% html_text()
    #result_ls <- c(result_ls, resultList)
    judge_ls$title[i] <- html %>% html_nodes(xpath = '//*[@class="caption-subject"]') %>% html_text()%>%list()
    judge_ls$court[i] <- html %>% html_nodes(xpath = '//*[@class="row case-footer"]') %>% html_text()%>%str_trim()%>%list()
    judge_ls$type[i] <- html %>% html_nodes(xpath = '//*[@class="col-md-8 case-footer-lineSecond"]') %>% html_text()%>%str_trim()%>%list()
    judge_ls$content[i] <- html %>% html_nodes(xpath = '//*[@class="tab-pane fade  active  in font-color"]') %>% html_text()%>%str_trim()%>%list()
    judge_ls$link[i] <- html %>% html_nodes(xpath = '//*[@class="detail-instrument-link"]') %>% html_attr("href")%>%list()
    #link <- html %>% html_nodes(xpath = '//*[@class="detail-instrument-link"]') %>% html_attr("href")
    #link_ls <- c(link_ls, link)
    
    b <- remDr$findElement("link text", "下一页")
    b$clickElement()
    
  })
  if ('try-error' %in% class(trynext)) next
  print(i)
  
}
judge_ls$title[200]
saveRDS(judge_ls,"data/judge_ls")
result_ls <- readRDS("data/result_ls.rds")
result_txt <- result_ls%>%str_split(.,"收藏")
result_txt[50:52]%>%unlist()
saveRDS(link_ls, "data/link_ls.rds")

format(object.size(link_ls),units = "MB")
result_ls[40000]
remDr$close()resuk
