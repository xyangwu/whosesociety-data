library(magrittr)
library(dplyr)
library(data.table)
library(tidyr)
library(igraph)
library(ggplot2)

# # # # # # # # # # # # # # # # # # # # # # # # #
################# read data in ##################
# # # # # # # # # # # # # # # # # # # # # # # # #

data_video <- readRDS("data/data_video_2.rds")
tids <- openxlsx::read.xlsx("data/tids.xlsx", sheet = 1)


################################# tidy #################################

dt_video  <- data_video %>%
  mutate(views = as.numeric(views),
         danmu = as.numeric(danmu)) %>%
  filter(danmu > 1&views > 1) %>%
  as.data.table(.) %>%
  .[grep("农村|乡村", tags), ]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#         the number of tags labelled on each video (tag_num)         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

tag_num <- sapply(dt_video[,tags], length)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   unlist the 'tags' coloumn, data is transform to longer (dt_tags)  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dt_tags <- data.table(av_id = rep(dt_video$av_id, tag_num), 
                      views = rep(dt_video$views, tag_num),
                      danmu = rep(dt_video$danmu, tag_num),
                      tag = rep(dt_video$tag, tag_num),
                      tags = unlist(lapply(dt_video[, tags], unlist)))

nodes <- data.frame(nodes = c(unique(dt_tags$av_id), unique(dt_tags$tags)),
                    stringsAsFactors = FALSE)
nodes <- left_join(nodes, dt_video[, c("av_id", "tag")], by = c("nodes" = "av_id")) %>%
  left_join(., tids[, c("tid2name", "tid1name")], by = c("tag"="tid2name"))

links <- dt_tags %>% select(av_id, tags) %>% as.matrix()

g_videotag <- graph_from_data_frame(d=links, vertices=nodes, directed = F)
V(g_videotag)$type <- V(g_videotag)$name %in% links[,1]
unique(V(g_videotag)$tid1name)
V(g_videotag)$color <- ifelse(
  V(g_videotag)$tid1name=="生活", alpha("firebrick3", 0.6),
  ifelse(V(g_videotag)$tid1name=="娱乐", alpha("deepskyblue3", 0.6),
         ifelse(V(g_videotag)$tid1name=="纪录片", alpha("darkorange", 0.6),
                ifelse(V(g_videotag)$tid1name=="影视", alpha("lightgreen", 0.6), "honeydew3")
         )
  )
  )

par(mar=rep(0.1,4))
plot(g_videotag,
     vertex.label = NA,
     vertex.frame.color = NA,
     vertex.size = 1,
     edge.width = 0.3,
     edge.color = 'grey80')

# another way to create two-mode network
dt_tags <- data.table(av_id = rep(dt_video$av_id, tag_num), 
                      views = rep(dt_video$views, tag_num),
                      danmu = rep(dt_video$danmu, tag_num),
                      tags = unlist(lapply(dt_video[, tags], unlist)),
                      value = 1) %>%
  pivot_wider(., names_from = tags,
              values_from = value)
g_videotag <- graph.incidence(dt_tags[, -c(2:3)], mode=c("all"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#           the frequency of occurrence of each tag (tag_freq)        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

tag_freq <- data.frame(tag = names(dt_tags)[-(1:4)],
                       freq = apply(dt_tags[,-(1:4)], 2, function(x){sum(x, na.rm = TRUE)}),
                       stringsAsFactors = FALSE) %>%
  arrange(desc(freq))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#           two videos that has most danmus in each category          #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dt_video_2max <- dt_video %>%
  group_by(tag) %>%
  filter(danmu >= sort(danmu, decreasing = TRUE)[2]) %>%
  ungroup(.) %>%
  as.data.table(.)

danmu_num = sapply(dt_video_2max[, danmuku], FUN = function(x){
 xdf <- as.data.frame(x)
 xrow = nrow(xdf)
 return(xrow)
})

danmuku <- lapply(dt_video_2max[, danmuku], as.data.frame) %>% do.call(rbind, .)
danmuku$av_id <- rep(dt_video_2max$av_id, danmu_num)
danmuku$title <- rep(dt_video_2max$title, danmu_num)
danmuku$tag <- rep(dt_video_2max$tag, danmu_num)
for (i in 1:nrow(danmuku)) {
  timeunix = str_split(danmuku$danmu_attr[i], ",", simplify = TRUE)[5] %>% trimws(.)
  danmuku$time[i] = as.POSIXlt(as.numeric(timeunix), origin="1970-01-01", tz="GMT") %>% as.character()
  print(i)
}

openxlsx::write.xlsx(danmuku, "data/danmuku.xlsx")



# Reference
# extract infomation from 'danmu_attr': https://www.bilibili.com/read/cv1023411/
#

