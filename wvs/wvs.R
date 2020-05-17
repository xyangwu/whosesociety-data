library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)
library(RColorBrewer)
library(showtext)
library(ggrepel)


##################### load in data #####################

# # # # # # # # # # # # # # # # #
#              font             #
# # # # # # # # # # # # # # # # #
font_add("stsong", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/STSONG.TTF")
font_add("stzhongs", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/STZHONGS.TTF")
font_add("gothic", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/GOTHIC.TTF")

# # # # # # # # # # # # # # # # #
#     world value survey        #
# # # # # # # # # # # # # # # # #
# http://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp
wvs <- readRDS("C:/Users/WXY/Documents/data/DATA_BASE/WVS/WVS_Longitudinal_1981_2016/WVS_Longitudinal_1981_2016.rds")

#code <- xlsx::read.xlsx("C:/Users/WXY/Documents/data/DATA_BASE/WVS/WVS_Longitudinal_1981_2016/F00003843_WVS_EVS_Integrated_Dictionary_Codebook_v_2014_09_22.xls", sheetIndex = 3, header=FALSE)
#code <- data.frame(code = str_split(code$X1, "\n") %>% unlist(),
#                    country = NA,
#                    stringsAsFactors = FALSE)
#code[, 1:2] <- str_split(code$code, ":", simplify = TRUE)
#code$code = as.numeric(code$code)
code = read.csv("data/code.csv")[,-1]


##################### proportion of justified #####################

##'X001	Sex
##'X002	Year of birth
##'x002_01	Having [countrys] nationality
##'x002_01a	Respondents nationality - ISO 3166-1 code
##'X003	Age
##'S001	Study "1:EVS 2:WVS -5:Missing; Unknown -4:Not asked in survey -3:Not applicable -2:No answer-1:Don´t know"
##'S002	Wave "1:1981-1984 2:1989-1993 3:1994-1998 4:1999-2004 5:2005-2009 6:2010-2014"
##'s002evs	EVS-wave
##'S003	Country/region
##'S003A	Country/regions [with split ups]
##'S004	Set
##'S006	Original respondent number
##'S007	Unified respondent number
##'S009: The country code (alphanumeric)
##'S025: The country and year
##'S017: The original weight
##'S020	Year survey
##'F118 Justifiable: homosexuality (wave6 V203) "1:Never justifiable ~ 10:Always justifiable"
##'A124_09 which you would not like to have as neighbors Homosexuals V40
##'d026_01	Homosexual couples - adopt children

prop_homo <- wvs %>%
  select(S003, S002, F118) %>%
  mutate(justi_homo = case_when(
    F118%in%c(1:5) ~ 0,
    F118%in%c(6:10) ~ 1) %>% as.factor(.)
  ) %>%
  mutate(n = 1) %>%
  aggregate(n ~ S002 + S003 + justi_homo, data = ., FUN = length) %>%
  group_by(S002, S003) %>%
  mutate(sum_country_year = sum(n),
         prop_justi_homo = n/sum_country_year) %>%
  ungroup(S002, S003) %>%
  group_by(S003) %>%
  mutate(n_wave = length(unique(S002))) %>%
  filter(n_wave > 3&justi_homo == 1) %>%
  ungroup(S003) %>%
  left_join(., code[, c("code", "country")], by = c("S003"="code"))

# set color
prop_homo <- left_join(prop_homo, prop_homo %>%
                           filter(S002==6) %>%
                           arrange(desc(prop_justi_homo)) %>%
                           mutate(color = rep(brewer.pal(11,"Spectral"),2)[1:nrow(.)]) %>%
                           select(S003, color), by = c("S003"="S003"))

showtext_auto()
prop_homo %>%
  ggplot(aes(x = S002, y = prop_justi_homo, group = S003, color = color)) +
  geom_line(size = 0.4) +
  geom_line(data = . %>% filter(country=="China"), # highlight China
            size = 0.5, color = "grey60") +
  geom_point(size = 1, shape = 21, 
             stroke = 0.8) +
  geom_point(color = "#fefdfb",
             size = 0.5) +
  geom_point(data = . %>% filter(S002%in%6) %>%
               arrange(desc(prop_justi_homo)), # hignlight point of last wave
             size = 2, 
             stroke = 0.8) +
  geom_text_repel(data = . %>% filter(S002==6) %>%
                    arrange(desc(prop_justi_homo)),
                  aes(label = paste0(country, ":", scales::percent(prop_justi_homo, 2))),
                  family = "gothic",
                  size = 3.5, color = rep(brewer.pal(11,"Spectral"), 2)[1:16],
                  hjust = -0.4, direction = "y", nudge_x = 2, # force = 2
                  segment.alpha = 0.3, segment.size = 0.1, segment.color = "grey60",
                  show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1, 6, 1),
                     labels = c("1981-1984", "1989-1993", "1994-1998", "1999-2004", "2005-2009", "2010-2014"),
                     limits = c(1, 6.4)) +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2), 
                     labels = scales::percent(seq(0, 0.8, 0.2), accuracy = 1), 
                     limits = c(0, 0.9)) +
  scale_color_identity() +
  labs(title = '你支持同性恋合法化吗？',
       subtitle = "在部分地区的抽样中赞成合法化的占比",
       y = NULL,
       x = NULL,
       caption = "Source: World Value Survey|数据来源：世界价值观调查") +
  theme_classic() +
  theme(
    plot.title = element_text(family = "stsong", face = "bold", colour = "grey40", size = 12),
    plot.subtitle = element_text(family = "stsong", vjust = -1.5, colour = "grey40", size = 10),
    plot.caption = element_text(family = "stsong", colour = "grey50", size = 6),legend.position = "none",
    panel.grid = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#fefdfb", colour = "#fefdfb"),
        axis.title = element_blank(),
        axis.ticks = element_line(color = "gray60", size = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 10, family = "gothic", color = "grey50"),
        axis.text.y = element_text(family = "gothic", size = 8, color = "grey50"),
        axis.ticks.length = unit(0.2, "cm"),
    axis.line = element_line(color = "gray60"),
    plot.background = element_rect(fill = "#fefdfb", colour = "#fefdfb"))

ggsave("prop.pdf", dpi = 300, width = 8, height = 6)  


##################### proportion of psychotrical disability #####################

disability <- read.xlsx("C:/Users/WXY/Downloads/disability.xlsx", sheet = 1)
data.frame(cate2_1prop[cate2_1prop$year==2018, c("cate2_1prop", )])

cate2_1prop <- disability %>%
  select(category2_1, category2_2, 地区, year, `已办理残疾人证(人)`) %>%
  filter(!地区%in%c("新疆兵团","黑龙江垦区")) %>%
  mutate(cate2_1prop = category2_1/`已办理残疾人证(人)`)

# set color
cate2_1prop <- left_join(cate2_1prop, cate2_1prop %>%
                           filter(year==2018) %>%
                           arrange(desc(cate2_1prop)) %>%
                           mutate(color = rep(brewer.pal(11,"Spectral"), 3)[1:nrow(.)]) %>%
                           select(地区, color), by = c("地区"="地区"))

showtext_auto()
cate2_1prop %>%
  ggplot(aes(x = year, y = cate2_1prop, group = 地区, color = color)) +
  geom_line(size = 0.4) +
  geom_text_repel(data = . %>% filter(year==2018) %>%
                    arrange(desc(cate2_1prop)),
                  aes(label = paste0(地区, ":", scales::percent_format(cate2_1prop))),
                  family = "stzhongs",
                  size = 3.5, color = rep(brewer.pal(11,"Spectral"), 3)[1:32],
                  hjust = -0.4, direction = "y", nudge_x = 2, # force = 2
                  segment.alpha = 0.3, segment.size = 0.1, segment.color = "grey60",
                  show.legend = FALSE) +
  scale_color_identity() +
  scale_x_continuous(breaks = seq(2011, 2018, 1),
                     labels = seq(2011, 2018, 1),
                     limits = c(2011, 2019)) +
  scale_y_continuous(breaks = seq(0, 0.2, 0.05), 
                     labels = scales::percent(seq(0, 0.2, 0.05), accuracy = 1), 
                     limits = c(0, 0.2)) +
  labs(title = '精神残疾人在领取证件的残疾人中占比',
       subtitle = "32个省市的历时变化",
       y = NULL,
       x = NULL,
       caption = "数据来源: 中国残疾人事业统计年鉴|Source: China Statistical Yearbook on the Work for Persons with Disabilities") +
  theme_classic() +
  theme(
    plot.title = element_text(family = "stsong", face = "bold", colour = "grey40", size = 12),
    plot.subtitle = element_text(family = "stsong", vjust = -1.5, colour = "grey40", size = 10),
    plot.caption = element_text(family = "stsong", colour = "grey50", size = 6),legend.position = "none",
    panel.background = element_rect(fill = "#fefdfb", colour = "#fefdfb"),
    axis.title = element_blank(),
    axis.ticks = element_line(color = "gray60", size = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 10, family = "gothic", color = "grey50"),
    axis.text.y = element_text(family = "gothic", size = 8, color = "grey50"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line = element_line(color = "gray60"),
    plot.background = element_rect(fill = "#fefdfb", colour = "#fefdfb"))

ggsave("disability.pdf", dpi = 300, width = 8, height = 6)

