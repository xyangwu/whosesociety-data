library(RColorBrewer)
library(showtext)
library(networkD3)
library(ggalluvial)
library(ggtext)

font_add("simyou", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/SIMYOU.TTF")
font_add("stsong", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/STSONG.TTF")
font_add("stzhongs", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/STZHONGS.TTF")
font_add("gothic", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/GOTHIC.TTF")

tids <- openxlsx::read.xlsx("data/tids.xlsx", sheet = 1)

############# plot ##############


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  #
#                        ######## diagrams #######                                   #
# The amount of videos under main areas and subareas which are in the 'tids1' coloum #
# and 'tids2' coloum in data 'tids'.                                                 #
#                                                                                    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  # # # # # # #
# Reference
# https://stackoverflow.com/questions/50395027/beautifying-sankey-alluvial-visualization-using-r

############# sanky diagrams 1. all devisions ##############

data_diagram <- dt_video %>%
  group_by(tag) %>%
  summarise(tag_n = n()) %>%
  arrange(desc(tag_n)) %>%
  as.data.frame() %>%
  left_join(., tids, by = c("tag"="tid2name")) %>%
  select(tids1name, tag, tag_n)

sum(data_diagram$value)

colnames(data_diagram) <- c("source", "target", "value")
data_diagram$target <- paste(data_diagram$target, " ", sep="")

nodes <- data.frame(name=c(as.character(data_diagram$source), as.character(data_diagram$target)) %>% unique())

data_diagram$IDsource=match(data_diagram$source, nodes$name)-1 
data_diagram$IDtarget=match(data_diagram$target, nodes$name)-1

# prepare colour scale
viridis_pal(begin = 0, end = 1, option = "D")(67)
ColourScal ='d3.scaleOrdinal() .range(["#440154FF","#46075AFF","#470D60FF","#471264FF","#481769FF","#481C6EFF","#482173FF","#482677FF","#472C7AFF","#46307EFF","#453581FF","#443A83FF","#433E85FF","#414287FF","#3F4788FF","#3E4C8AFF","#3C508BFF","#3A548CFF","#38598CFF","#365C8DFF","#34608DFF","#32648EFF","#31688EFF","#2F6C8EFF","#2D708EFF","#2C738EFF","#2A768EFF","#297A8EFF","#277E8EFF","#26828EFF","#25858EFF","#23898EFF","#228D8DFF","#21908CFF","#20938CFF","#1F978BFF","#1E9B8AFF","#1F9F88FF","#1FA287FF","#21A685FF","#24AA83FF","#26AD81FF","#2BB07FFF","#2FB47CFF","#35B779FF","#3BBB75FF","#41BE71FF","#49C16DFF","#51C56AFF","#59C765FF","#61CA60FF","#69CD5BFF","#73D056FF","#7BD250FF","#85D54AFF","#8FD744FF","#99D83DFF","#A3DA37FF","#ADDC30FF","#B8DE29FF","#C2DF23FF","#CCE11EFF","#D7E219FF","#E0E318FF","#EBE51AFF","#F4E61EFF","#FDE725FF"])'
#ColourScal = paste0("d3.scaleOrdinal() .range([", paste(viridis_pal(option = "D")(67), sep = "",collapse = '","'), '"])')

# Make the Network
sankeyNetwork(Links = data_diagram, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name",
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=30, fontSize=13, nodePadding=10)


############# sanky diagrams 2. fewer devisions ##############

data_diagram2 <- dt_video %>%
  mutate(tag2 = case_when(
    tag %in% tids$tid2name[tids$tid1name%in%c('音乐','游戏','娱乐','数码','国创','舞蹈','鬼畜','时尚','广告','动画')] ~ "其它二级分类",
    TRUE ~ as.character(tag))) %>%
  group_by(tag2) %>%
  summarise(tag_n = n()) %>%
  left_join(., tids, by = c("tag2"="tid2name")) %>%
  select(tid1name, tag2, tag_n) %>%
  mutate(tid1name = factor(tid1name, levels = c("生活","纪录片","科技","影视","其它一级分类")),
         tag2 = factor(tag2, levels = tag2[order(-tag_n)])) %>%
  arrange(tid1name, desc(tag2))

## add label coloumn
diagram_label <- data_diagram2 %>%
  left_join(., data_diagram2 %>%
              group_by(tid1name) %>%
              summarise(tag_sum = sum(tag_n)),
            by = c("tid1name"="tid1name")) %>%
  mutate(tid1name = paste0(tid1name, ": ", tag_sum),
         tag2 = paste(tag2, ": ", tag_n)) %>%
  mutate(tid1name = factor(tid1name, levels = unique(tid1name))) %>%
  arrange(tid1name, desc(tag_n))

diagram_label$tag2[order(-diagram_label$tag_n)][13:21] <- " "
diagram_label$tag2 <- factor(diagram_label$tag2, levels = c(diagram_label$tag2[order(-diagram_label$tag_n)][1:12], " "))

unique(diagram_label$tid1name)
col1 <- "firebrick3"
col2 <- "darkorange"
col3 <- "deepskyblue3"
col4 <- "lightgreen"
col5 <- "honeydew3"

showtext_auto()
ggplot(diagram_label,
       aes(y = tag_n,
           axis1 = tid1name, axis2 = tag2)) +
  geom_alluvium(aes(fill = tid1name, color = tid1name), 
                width = 1/12, alpha = 0.7, knot.pos = 0.4) +
  geom_stratum(width = 1/4, color = "grey") +
  geom_text(stat = "stratum", label.strata = TRUE, family = "stsong", size = 3) +
  scale_x_continuous(breaks = 1:2,
                     labels = c("一级分类\nMain Category", "二级分类\nSub-Category"),
                     expand = expansion(add = c(0.1, 0.1))) +
  scale_fill_manual(values  = c(col1, col2, col3, col4, col5)) +
  scale_color_manual(values = c(col1, col2, col3, col4, col5)) +
  labs(
    title = glue::glue("Bilibili上有关农村的视频在不同类别间的分布"),
    subtitle = paste0("N = ", sum(data_diagram2$tag_n)),
    caption = "Source: bilibili.com"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "stzhongs", 
                              face = "bold", 
                              colour = "grey50", 
                              size = 12,
                              hjust = 0.12,
                              vjust = -5),
    plot.subtitle = element_text(family = "stsong",
                                 colour = "grey60",
                                 size = 12,
                                 hjust = 0.07,
                                 vjust = -5),
    plot.caption = element_text(family = "gothic", 
                                colour = "grey50", 
                                size = 8),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "stzhongs", size = 10, face = "bold", vjust = 8)
  )

ggsave("fig/fig_sankey2.pdf", width = 7, height = 7, dpi = 300)

# percent of videos labelled by 'food' in each group
boxplot(tag_freq$freq)
ggplot(tag_freq, aes(factor(),国内旅游收入在GDP中占比)) +
  geom_boxplot(aes(group=时间)) +
  geom_text_repel(data=city_lable,aes(label= paste0(地区,':',`国内旅游收入在GDP中占比(%)`),x=factor(时间),y= 国内旅游收入在GDP中占比),
                  vjust = 0.5, hjust=0.5,size=3) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75),
                     labels = scales::percent(c(0.25,0.5,0.75),1))+
  theme_pander() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = 'tourism income/GDP(%)',
       x='')
