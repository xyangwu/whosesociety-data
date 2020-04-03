library(tidyverse)
library(colorblindr)
library(RColorBrewer)
library(showtext)
library(ggrepel)

setwd("C:/Users/WXY/Documents/R_learning/whosedata")
# fonts
font_add("simyou", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/SIMYOU.TTF")
font_add("gothic", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/GOTHIC.TTF")


############ read in data ###############

salary <- openxlsx::read.xlsx('occupation/data/职业溢价.xlsx', sheet = 1)
industry_perc <- openxlsx::read.xlsx('occupation/data/职业溢价.xlsx', sheet = 2)
occupt <- openxlsx::read.xlsx('occupation/data/职业溢价.xlsx', sheet = 3)


############ salary premium ###############
names(salary)[1] <- "industry"
salary2 <- salary %>%
  pivot_longer(., cols = c(2:17), names_to = "year", values_to = "value") %>%
  group_by(year) %>% 
  mutate(premium = (value-value[industry=="农、林、牧、渔业"])/value[industry=="农、林、牧、渔业"])

salary2$year <- as.numeric(salary2$year)

# show salary rank in 2018
salary2$premium[salary2$year==2018] %>%
  order(decreasing = T) %>%
  salary2[salary2$year==2018, c("industry", "premium")][.,]

salary2 <- salary2 %>%
  mutate(colour = case_when(
      industry == "信息传输、软件和信息技术服务业" ~ "#9c4b28",
      industry == "金融业" ~ "#b94d5c",
      industry == "科学研究和技术服务业" ~ "#FF7F00",
      industry == "电力、热力、燃气及水生产和供应业" ~ "#6BAED6",
      industry == "文化、体育和娱乐业" ~ "#3182BD",
      T ~ "#5d5e62")
      ) %>%
  unnest()

display.brewer.all()
display.brewer.pal(5, "Set1");  brewer.pal(5, "Set1")

theme_custom  <- theme(legend.position = "none",
                       plot.background = element_rect(fill = "NA", colour = "NA"),
                       plot.margin = unit(c(0.2, 0.2, 0.2, 0.6), "cm"),
                       panel.grid = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       panel.grid.minor.y = element_blank(),
                       panel.grid.major.x = element_blank(),
                       panel.grid.major.y = element_line(color = "gray75", size = 0.3),
                       text = element_text(size = 6),
                       axis.title = element_blank(),
                       axis.ticks.x = element_line(color = "gray75", size = 0.1),
                       axis.ticks.y = element_line(color = "gray75", size = 0.1),
                       axis.text.x = element_text(family = "gothic", size = 7),
                       axis.text.y = element_text(family = "gothic", size = 7),
                       axis.ticks.length.x = unit(0.2, "cm"),
                       axis.ticks.length.y = unit(0.1, "cm"),
                       axis.line.x = element_line(size = 0.4, color = "gray50"),
                       axis.line.y = element_line(size = 0.4, color = "gray50"),
                       plot.title = element_text(family = "simyou", face = "bold", colour = "grey20", size = 10),
                       plot.subtitle = element_text(family = "simyou", vjust = 2, colour = "grey20", size = 8),
                       plot.caption = element_text(family = "simyou", colour = "grey20", size = 8)
)

showtext_auto()
ggplot(salary2 %>% filter(industry!="合计"&industry%in%c("信息传输、软件和信息技术服务业", "金融业", "科学研究和技术服务业",
                                                       "电力、热力、燃气及水生产和供应业", "文化、体育和娱乐业", "批发和零售业",
                                                       "卫生和社会工作", "教育", "制造业", "建筑业", "交通运输、仓储和邮政业")),
       aes(year, premium, group = industry, color = colour)) +
  geom_line(size = 0.3) +
  geom_point(data = . %>% filter(premium > 1.5&year==2018),
             aes(col = colour),
             shape = 21,
             size = 0.8,
             stroke = 0.8) +
  geom_text_repel(data = . %>% filter(year==2018) %>% 
              mutate(label = paste0(industry)), #, round(premium)
            aes(label = label, hjust = -0.08),
            family = "simyou",
            size = 2,
            hjust = 0,
            direction = "y",
            nudge_x = 1,
            segment.color = "grey60", segment.alpha = 0.5,
            segment.size = NA, 
            show.legend = FALSE) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_manual(values = c(0.4, 1), guide = F) +
  scale_size_manual(values = c(0.2, 0.5), guide = F) +
  scale_x_continuous(breaks = seq(2003, 2018, 3), 
                     labels = seq(2003, 2018, 3), 
                     limits = c(2003, 2022)
                     ) +
  # the lower limit is expanded by add[1]
  # and the upper limit by add[2].
  scale_y_continuous(position = "left",  
                     limits = c(0, 4)
                     #expand = expansion(add = c(0, 1))
                     ) + 
  labs(title = "不同行业相对于第一产业的技能溢价",
       subtitle = "2003-2018",
       caption = "数据：中国人口与就业年鉴") +
  theme_classic() +
  theme_custom +
  theme(panel.grid.major.y = element_blank(),
        axis.line.x = element_line(size = 0.5, color = "black")
  )


ggsave("occupation/plot_salary.pdf", width = 6, height = 4, dpi = 300)

############ industry ###############

industry_perc2 <- industry_perc %>% select(-合计) %>%
  pivot_longer(., cols = c(3:21), names_to = "industry", values_to = "value") %>%
  pivot_wider(names_from = gender, values_from = value) %>%
  mutate(d = (男/100 - 女/100)*100)

salary2$year <- as.numeric(salary2$year)

salary2$premium[salary2$year==2018] %>%
  order(decreasing = T) %>%
  salary2[salary2$year==2018, c("industry", "premium")][.,]

industry_perc2 <- industry_perc2 %>%
  mutate(colour = case_when(
    industry == "信息传输、软件和信息技术服务业" ~ "#9c4b28",
    industry == "金融业" ~ "#b94d5c",
    industry == "科学研究和技术服务业" ~ "#FF7F00",
    industry == "电力、热力、燃气及水生产和供应业" ~ "#6BAED6",
    industry == "文化、体育和娱乐业" ~ "#3182BD",
    T ~ "#5d5e62")
  ) %>%
  unnest()

showtext_auto()
ggplot(industry_perc2 %>% filter(industry%in%c("信息传输、软件和信息技术服务业", "金融业", "科学研究和技术服务业",
                                               "电力、热力、燃气及水生产和供应业", "文化、体育和娱乐业", "批发和零售业",
                                               "卫生和社会工作", "教育", "制造业", "建筑业", "交通运输、仓储和邮政业")) %>% arrange(d),
       aes(year, d, group = industry, color = colour)) +
  geom_line(size = 0.3, alpha = 0.7) +
  #industry%in%c("信息传输、软件和信息技术服务业", "金融业", "科学研究和技术服务业","电力、热力、燃气及水生产和供应业", "文化、体育和娱乐业")
  geom_point(data = . %>% filter(year==2018),
             aes(col = colour),
             shape = 21,
             size = 0.8,
             stroke = 0.8) +
  geom_text_repel(data = . %>% filter(year==2018&
                                        industry%in%c("信息传输、软件和信息技术服务业", "金融业", "科学研究和技术服务业",
                                                      "电力、热力、燃气及水生产和供应业", "文化、体育和娱乐业", "批发和零售业",
                                                      "卫生和社会工作", "教育", "制造业", "建筑业", "交通运输、仓储和邮政业")) %>% 
                    mutate(label = paste0(industry)),# ": ", round(d)))
                  aes(label = label),
                  family = "simyou",
                  size = 2,
                  hjust = -0.4,
                  direction = "y",
                  nudge_x = 1,
                  segment.color = "grey60", segment.alpha = 0.5,
                  segment.size = 0.1, 
                  show.legend = FALSE) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_manual(values = c(0.4, 1), guide = F) +
  scale_size_manual(values = c(0.2, 0.5), guide = F) +
  scale_x_continuous(breaks = seq(2004, 2018, 3), 
                     labels = seq(2004, 2018, 3), 
                     limits = c(2003, 2022)) +
  scale_y_continuous(position = "left",               
                     expand = expansion(add = c(0.5, 1))) + 
  labs(title = "不同行业男性化与女性化的程度",
       subtitle = "2004-2018",
       caption = "数据：中国人口与就业年鉴") +
  theme_classic() +
  theme_custom +
  theme(panel.grid.major.y = element_blank()
  )

ggsave("occupation/plot_line_seg4.pdf", width = 6, height = 4, dpi = 300)

############ occupation ###############

occupt2 <- occupt %>%
  filter(area=="total"&gender!="总计"&year>2001) %>%
  select(-`就业人员(%)`,-area)

# emp: ratio of female or male in employed population
occupt2[, c(3:9)] <- occupt2[, c(3:9)]*occupt2$emp

occupt3 <- occupt2 %>%
  select(-emp) %>%
  pivot_longer(., cols = c(3:9), names_to = "occupation", values_to = "value") %>%
  pivot_wider(names_from = gender, values_from = value) %>%
  mutate(percent_f = 女/(男+女)) %>%
  select(year, occupation, percent_f) %>%
  left_join(., occupt2[occupt2$gender=="女", c("year", "emp")], by = c("year"="year"))

xlsx::write.xlsx(occupt3 %>% select(year, occupation, percent_f) %>%
                   pivot_wider(names_from = occupation, values_from = percent_f), "occupation/female_ratio.xlsx")

occupt3$occupation[occupt3$occupation=="生产运输设备操作人员及有关人员"] <- "生产运输设备操作人员"
occupt3$occupation[occupt3$occupation=="其他"] <- "其他人员"

occupt3 <- occupt3 %>%
  mutate(colour = case_when(
    occupation == "单位负责人" ~ "#9c4b28",
    occupation == "专业技术人员" ~ "#b94d5c",
    occupation == "办事人员和有关人员" ~ "#FF7F00",
    occupation == "商业、服务业人员" ~ "#6BAED6",
    occupation == "农林牧渔水利业生产人员" ~ "#3182BD",
    occupation == "生产运输设备操作人员" ~ "#4DAF4A",
    T ~ "#5d5e62")
  ) %>%
  unnest()

showtext_auto()
ggplot(occupt3,
       aes(year, percent_f, group = occupation, color = colour)) +
  geom_line(size = 0.3, 
            alpha = 0.7, 
            linejoin = "mitre") +
  geom_line(aes(year, emp/100), 
            color = "grey60", 
            linetype = "dashed",
            linejoin = "mitre",
            size = 0.3) +
  geom_point(data = . %>% filter(year==2018),
             aes(col = colour),
             shape = 21,
             size = 1,
             stroke = 1) +
  geom_text_repel(data = . %>% filter(year==2018) %>% 
                    mutate(label = paste0(occupation)), #, ": ", scales::percent(percent_f))), 
                  aes(label = label),
                  family = "simyou",
                  size = 2,
                  hjust = 0,
                  direction = "y",
                  nudge_x = 1,
                  segment.color = "grey60", segment.alpha = 0.5,
                  segment.size = NA, 
                  show.legend = FALSE) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_manual(values = c(0.4, 1), guide = F) +
  scale_size_manual(values = c(0.2, 0.5), guide = F) +
  scale_x_continuous(breaks = seq(2002, 2018, 2), 
                     labels = seq(2002, 2018, 2), 
                     limits = c(2002, 2020)) +
  scale_y_continuous(position = "left", 
                     breaks = seq(0, 0.7, 0.1), 
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.7)) + 
  labs(title = "按职业大类分的女性从业者比例",
       subtitle = "7个职业大类，2002-2018",
       caption = "数据：中国人口与就业统计年鉴") +
  theme_classic() +
  theme_custom +
  theme(panel.grid.major.y = element_blank(),
        axis.line.x = element_line(size = 0.5, color = "black")
        )

ggsave("plot_line_ocp.pdf", width = 6, height = 4, dpi = 300)

