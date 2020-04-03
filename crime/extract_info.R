Sys.getenv()
Sys.setenv(LC_CTYPE = "zh_CN.UTF-8")
Sys.setlocale("LC", 'en_US.UTF-8')
#GB2312 GB18030 SHIFT-JIS

library(tidyverse)
library(stringi)
setwd("C:/Users/WXY/Documents/R_learning/judgement")

############################# load in data ##############################

list.files("data/")
judgement_rape <- readRDS('data/judgement_rape_df.rds')
judgement_rape_info <- readRDS('data/judgement_rape_info.rds')
obscene_w <- readRDS('data/judgement_obscene_w_df.rds')
obscene_c <- readRDS('data/judgement_obscene_c_df.rds')

############################# tidy data ##############################

names(judgement_rape)[1:7] <- c("judge_num", "judge_title", "judge_type", "cause", "court", "process", "verdict_date")
verdict_rape <- judgement_rape[judgement_rape$judge_type=="判决书", ] # verdict 判决书
verdict_rape <- verdict_rape[!duplicated(verdict_rape[, c("judge_num")]), ] # delete duplicated case number
verdict_rape$content <- str_trim(verdict_rape$content)

# example #
test <- '公诉机关上海市浦东新区人民检察院。被告人柯某某，男，1965年10月20日生，回族，户籍地安徽省，暂住上海市浦东新区。'
stri_sub(test, #tried to extract the accused's name but failed
         stri_locate(test, regex = '被告人[^&]{1,4}，'))
# It seems stri_locate and str_extract cannot identify comma as a character

stri_subset(test, regex="被告人[^&]{1,4}。")
# stri_subset_regex and str_extract also cannot extract the strings that I need from 'test'.
# After the comma is replaced with a full stop, searching engines above can extract the name. 

############################# extract info ##############################

verdict_rape$indict_num <- str_extract(verdict_rape$content, '以[^&]{1,20}起诉书指控') %>%
  gsub('指控|以','',.)
verdict_rape$indict_date <- str_extract(verdict_rape$content, '于[^&]{1,20}提起[^&]{1,2}') %>%
  gsub("于|向本院提起公诉|日", "", .) %>%
  gsub("年|月", "-", .) %>% 
  as.Date()

## the accused's info

verdict_rape$accused_name <- str_extract(verdict_rape$content, '被告人[^&]{1,4}犯') %>%
  gsub("被告人|犯","",.)
verdict_rape$accused_hukou <- stri_locate(verdict_rape$content, regex='户籍[^&]{1,10}。|户籍[^&]{1,10}，') %>%
  stri_sub(verdict_rape$content,.) %>%
  gsub("户籍所在地|户籍地|。|，", "", .)
verdict_rape$accused_birth <- str_extract(verdict_rape$content, '\\d+年\\d+月\\d+日[^&]{0,3}生|\\d+年\\d+月[^&]{0,5}生') %>%
  gsub("出生|生", "", .)
which(is.na(verdict_rape$accused_birth)) %>% length()
verdict_rape$accused_residence <- str_extract(verdict_rape$content, '暂住[^&].{1,7}。') %>%
  gsub("暂住", "", .)
verdict_rape$accused_ethnic <- str_extract(verdict_rape$content, '[^&]{1,2}族') %>%
  gsub("，","",.)

# whether has convinctions before: 1 是, 0否, 2 是且为性侵
verdict_rape$accused_recorded <- 0

# whether tried to kill victim: 1, 0
verdict_rape$accused_murder <- 0


## victim's info

# name of victim(pseudonymous) 受害者姓名(经过保密处理)
verdict_rape$victim_name <- str_extract(verdict_rape$content, '被害[^&]{1,4}') %>%
  gsub("被害人|\\(", "", .)
verdict_rape$victim_birth <- lapply(1:nrow(verdict_rape), FUN = function(x){
  pattern1 <- paste0(verdict_rape$victim_name[x], "[^&]{0,2}", "\\d+年\\d+月\\d+[^&]{0,4}生")
  pattern2 <- paste0(verdict_rape$victim_name[x], "\\d+[^&]{0,4}年出生")
  pattern3 <- paste0(verdict_rape$victim_name[x], "[^&]{1,3}岁|", verdict_rape$victim_name[x], "[^&]{1,3}周岁")
  paste0(pattern1, pattern2, pattern3) %>%
    str_extract(verdict_rape$content[x], .) %>%
    gsub(paste0("出生|年生|", verdict_rape$victim_name[x], "|\\("), "", .)
  }
  ) %>% c(.) %>%
  unlist()
verdict_rape$victim_hukou <- NA

# state of victim: sleep, drunk/unconscious, disabled, amentia
# 性侵发生时受害者状态: 睡觉, 醉酒/无意识, 残疾, 智力障碍
verdict_rape$victim_status <- NA

## relation of the accused and victim
# stranger, relative_blood, relative_law, friend, neighbor(in the same neighborhood)
# 与受害者关系: 陌生人, 亲属—非血缘, 亲属-血缘, 朋友, 邻居
verdict_rape$victim_relation <- NA

## verdict result
# 判决结果

verdict_rape$sentence_term <- str_extract(verdict_rape$content, '判处有期徒刑[^&]{1,8}月|判处有期徒刑[^&]{1,8}年|判处判处无期徒刑[^&]{1,8}') %>%
  gsub("判处","",.)
verdict_rape$sentence_from <- str_extract(verdict_rape$content, '自[^&]{8,12}日起') %>%
  gsub("自|起","",.) %>%
  gsub("年|月", "-", .) %>% as.Date()
verdict_rape$sentence_to <- str_extract(verdict_rape$content, '至[^&]{8,12}日止') %>%
  gsub("至|止","",.) %>%
  gsub("年|月", "-", .) %>% as.Date()
verdict_rape$compenst <- str_extract(verdict_rape$content, '赔偿[^&]{1,20}元|赔偿[^&]{1,20}元')
verdict_rape$court <- str_extract(verdict_rape$content, '[^&]{0,10}法院') %>%
  str_squish()

## crime spot 罪案地点

verdict_rape$spot <- str_extract(verdict_rape$content, '[^&]{0,3}区[^&]{1,7}路[^&]{1,5}|[^&]{0,3}区[^&]{1,7}镇[^&]{1,5}')
# replace Na with name of local area
verdict_rape$spot[grep("本区", verdict_rape$spot)] <- grep("本区", verdict_rape$spot) %>%
  verdict_rape$court[.] %>%
  gsub("上海市|人民法院","",.) %>% 
  paste0(., gsub("[^&]{0,4}本区", "", verdict_rape$spot[grep("本区", verdict_rape$spot)]))

na_spot <- which(is.na(verdict_rape$spot))
length(na_spot) # number of na

verdict_rape$spot[na_spot] <- verdict_rape[na_spot, c("judge_num", "spot")] %>%
  left_join(., judgement_rape_info[, c("案号", "place")], by = c("judge_num" = "案号")) %>% 
  filter(!duplicated(judge_num)) %>%
  .[, c("place")] %>% as.character()

# I cannot pick out all spots with code, so recognize them manually...
# 'place' is the result of that manually recognition
# place <- readLines("data/place.txt", encoding = "UTF-8")
# place <- place %>% str_split(.,",") %>% unlist()

which(is.na(verdict_rape$spot)) %>% 
  verdict_rape[., ] %>%
  export::table2excel(., "export/verdict_spot_na.xlsx") # check it at outside

verdict_rape$spot[which(is.na(verdict_rape$spot))] <- c("崇明县长兴镇怡清苑南侧泥土小路100米处的沟渠","天山西路XXX号商务楼710室",
                                    "青浦区新阳村XXX号南侧20米处一无证废品收购站")

# str_extract_all(verdict_rape$content[na_spot], '[^&]{0,3}市[^&]{0,2}区[^&]{1,6}')

verdict_rape$spot <- verdict_rape$spot%>% gsub("[^&]{0,3}本区","上海市",.)
verdict_rape$spot[which(str_sub(verdict_rape$spot, 1, 1)=="市")] <- which(str_sub(verdict_rape$spot, 1, 1)=="市") %>%
  verdict_rape2$spot[.] %>% 
  str_sub(., 2)

## spot time 罪案发生时间

p_sentence <- verdict_rape$content %>% str_split(., '。')
verdict_rape$spot_time <- NA
for (i in which(is.na(verdict_rape$spot_time))) {
  
  trynext<- try({
    accused <- verdict_rape$name_accused[i]
    victim <- verdict_rape$name_victim[i]
    pattern <-  paste0("\\d+年(.*?)", accused, "(.*?)")
    spot_time <- p_sentence[[i]] %>% str_squish() %>% .[grep(pattern, .)] %>% 
      str_extract(., '\\d+年[^&]{0,3}月\\d+日\\d+时|\\d+年[^&]{0,3}月\\d+日')
    
    if(length(spot_time[!is.na(spot_time)])==1){
      verdict_rape$spot_time[i] <- spot_time[!is.na(spot_time)]
    }else{
      pattern2 <- paste0("\\d+年(.*?)", "被告人", accused)
      spot_time <- p_sentence[[i]] %>% .[grep(pattern2, .)] %>% 
        str_extract(., '\\d+年\\d+月\\d+日')
      if(length(spot_time[!is.na(spot_time)])==1){
        verdict_rape$spot_time[i] <- spot_time[!is.na(spot_time)]
        }else{
          pattern3 <- paste0("被告人(.*?)于", "\\d+年\\d+月\\d+日", "(.*?)", victim)
          spot_time <- p_sentence[[i]] %>% .[grep(pattern3, .)] %>%
            str_extract(., '\\d+年\\d+月\\d+日')
          verdict_rape$spot_time[i] <- spot_time[!is.na(spot_time)]
    }
    }
  })
  
  if ('try-error' %in% class(trynext)) next
  print(i)
}

na_spottime <- which(is.na(verdict_rape$spot_time))
length(na_spottime)

# check it at outside
verdict_rape[na_spottime, c("content")] %>% str_squish() %>% as.data.frame() %>%
  export::table2doc(., "export/spotime_na.docx") 
verdict_rape$content[350]
verdict_rape$spot_time[na_spottime[14]] <- c("2013年8月24日1时")
verdict_rape$spot_time[na_spottime[c(6, 8,9,11, 12,13,14)]] <- c("2007年8月10日13时","2015年7月20日9时","2010年","2008年","2015年9月2日5时","2014年","2013年8月24日1时")


#############################  DO NOT RUN ############################# 
# 执行变更案件刑事裁定书
verdict_rape$judgtime <- str_extract(verdict_rape$content, '于[^&]{1,20}日作出')%>%gsub('作出|于','',.)
verdict_rape$judg_num <- str_extract(verdict_rape$content, '作出[^&]{1,20}刑事判决')%>%gsub('作出|了','',.)
sum(is.na(verdict_rape$place))
verdict_rape$sentence_to[1:5]
verdict_rape$content[3]
verdict_rape$accused[is.na(verdict_rape$accused)] <- str_extract(verdict_rape$content[is.na(verdict_rape$accused)], '罪犯[^&]{1,10}男|罪犯[^&]{1,10}现[^&]{1,10}|罪犯[^&]{1,4}')%>%gsub('罪犯','',.)

############################# output data ##############################
saveRDS(verdict_rape, 'export/verdict_rape.rds')
write.csv(verdict_rape2, "export/verdict_rape2.csv")
write.csv(verdict_rape$content[1:50],"export/content1_50.csv")

