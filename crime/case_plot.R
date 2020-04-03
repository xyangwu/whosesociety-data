library(tidyverse)
library(showtext)
library(ggrepel)
library(RColorBrewer)
library(ggforce)
library(prismatic)

options(scienpen = 999)
############################# load in data ###############################
case_cn <- openxlsx::read.xlsx("data/cases.xlsx", sheet = 1)
case_city <- openxlsx::read.xlsx("data/cases.xlsx", sheet = 2)

"中国统计年鉴+中国法律年鉴+"
font_add("simyou", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/SIMYOU.TTF")
font_add("gothic", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/GOTHIC.TTF")

############################# tidy data ###############################
### NOT RUN ###
case_cn <- case_cn %>% 
  select(时间, 指标, 数值, 单位) %>%
  mutate(时间 = gsub("年", "", 时间) %>% as.numeric(),
         指标 = gsub("公安机关立案的", "", 指标) %>% gsub(",构成", "构成", .) %>% 
           gsub("案件数|案件立案", "案件", .) %>%  gsub("刑事案件,其他", "其他刑事案件", .) %>% as.character())

case_cn <- case_cn[!duplicated(case_cn[, c("时间", "指标")]), ]

write.csv(case_cn, "data/case_cn.csv")
### NOT RUN ###

# data for plotting
data = case_cn %>% select(1:12) %>%
  pivot_longer(., 
               3:12, 
               names_to = "case", values_to = "value") %>%
  mutate(percent = round(value/`立案（起）`, 4),
         color = case_when(
           case == "强奸案件" ~ "#FB6A4A",
           T ~ "grey60")
         )
 
data$case[data$case=="伪造、变造货币，出售、购买、运输、持有、使用假币案件"] <- "假币相关案件"

# city
case_city <- case_city %>% pivot_longer(., 
                          3:5, 
                          names_to = "city", values_to = "value") %>%
  mutate(color = case_when(
    city == "上海" ~ "#377EB8",
    city == "东莞" ~ "#FF7F00",
    city == "合肥" ~ "#984EA3")
  )

case_sh <- pivot_longer(case_sh, 
                        3:9, 
                        names_to = "name", values_to = "value") %>%
  mutate(percent = round(value/总计, 4),
         percent2 = percent^2) %>%
  group_by(year) %>%
  mutate(case = row_number()) %>%
  ungroup()

display.brewer.all()
display.brewer.pal(5, "Set1"); brewer.pal(5,"Set1")

######################### plot theme ########################

theme_custom <- theme(legend.position = "none",
      panel.grid = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", size = 0.5),
      panel.background = element_rect(fill = "#fefdfb", colour = "#fefdfb"),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.x = element_line(color = "grey85", size = 0.5),
      axis.text.x = element_text(family = "gothic", size = 8),
      axis.text.y = element_text(family = "gothic", size = 8),
      axis.ticks.length = unit(0.2, "cm"),
      axis.line.x = element_line(size = NA, color = "gray85"),
      plot.title = element_text(family = "simyou", face = "bold", colour = "grey40", size = 12),
      plot.subtitle = element_text(family = "gothic", vjust = 2, colour = "grey40", size = 10),
      plot.caption = element_text(family = "simyou", colour = "grey50", size = 6),
      plot.background = element_rect(fill = "#fefdfb", colour = "#fefdfb"),
      plot.margin = unit(c(0.2, 0.2, 0.2, 0.6), "cm")
)

############################# line plot 1 ###############################

showtext_auto()
ggplot(data = data[order(data$case, decreasing = T), ],
       aes(year, value, group = case, color = color)) +
  geom_line(size = 0.4) +
  geom_point(size = 0.8 # shape = 21, stroke = 0.8
             ) +
  geom_point(color = "#fefdfb",
             size = 0.5) +
  geom_line(data = . %>% filter(case == "强奸案件"),  # highlight line
            size = 0.4) +
  geom_point(data = . %>% filter(case == "强奸案件"), # highlight point
             size = 0.8,
             stroke = 0.8) +
  geom_point(data = . %>% filter(case == "强奸案件"), # highlight
             color = "#fefdfb",
             size = 0.5) +
  geom_text_repel(data = . %>% filter(year==2018) %>%
                    arrange(desc(value)),
                  aes(label = paste0(case, ":", value), color = color),
                  family = "simyou",
                  size = 3.5,
                  hjust = -0.4,
                  direction = "y",
                  nudge_x = 2, # force = 2
                  segment.alpha = 0.5, segment.size = 0.1,
                  show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1995, 2018, 2),
                     labels = seq(1995, 2018, 2),
                     limits = c(1995, 2024)) +
  scale_y_continuous(breaks = seq(0, 5000000, 1000000), 
                     labels = c(0, paste0(seq(1, 5, 1), ",000,000")), 
                     limits = c(0, 5000000)) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "公安机关立案的刑事案件(全国)",
       subtitle = "1995-2018",
       caption = "来源：中国统计年鉴&中国法律年鉴") +
  theme_minimal() +
  theme_custom

ggsave("export/case_cn5.pdf", dpi = 300, width = 8, height = 5)

############################# line plot 2 ###############################

library(scales)
showtext_auto()
ggplot(data = data[data$case=="强奸案件", ] ,
       aes(year, percent, group = case, color = color)) +
  geom_line(size = 0.4) +
  geom_point(size = 0.8,
             shape = 21, 
             stroke = 0.8) +
  geom_point(color = "#fefdfb",
             size = 0.5) +
  geom_point(data = . %>% filter(year%in%c(1995, 2018)), # hignlight
             size = 2, 
             stroke = 0.8,
             color = "#DE2D26") +
  geom_text(data = . %>% filter(year%in%c(1995, 2018)),
                  aes(label = paste0(year, ":", percent(percent))),
                  family = "gothic",
                  size = 3.5, color = "#DE2D26",
                  hjust = 0.3, vjust = 2, 
                  show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1995, 2018, 2),
                     labels = seq(1995, 2018, 2),
                     limits = c(1995, 2020)) +
  scale_y_continuous(breaks = seq(0, 0.03, 0.01), 
                     labels = scales::percent(seq(0, 0.03, 0.01), accuracy = 1), 
                     limits = c(0, 0.03)) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "公安机关立案中强奸案件占比(全国)",
       subtitle = "1995-2018",
       caption = "来源：中国统计年鉴&中国法律年鉴") +
  theme_minimal() +
  theme_custom

############################# line plot 3 ###############################

library(scales)
showtext_auto()
ggplot(data = data[data$case=="强奸案件", ] ,
       aes(year, percent, group = case, color = color)) +
  geom_line(size = 0.4) +
  geom_point(size = 0.8,
             shape = 21, 
             stroke = 0.8) +
  geom_point(color = "#fefdfb",
             size = 0.5) +
  geom_point(data = . %>% filter(year%in%c(1995, 2018)), # hignlight
             size = 2, 
             stroke = 0.8,
             color = "#DE2D26") +
  geom_text(data = . %>% filter(year%in%c(1995, 2018)),
            aes(label = paste0(year, ":", percent(percent))),
            family = "gothic",
            size = 3.5, color = "#DE2D26",
            hjust = 0.3, vjust = 2, 
            show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1995, 2018, 2),
                     labels = seq(1995, 2018, 2),
                     limits = c(1995, 2020)) +
  scale_y_continuous(breaks = seq(0, 0.03, 0.01), 
                     labels = scales::percent(seq(0, 0.03, 0.01), accuracy = 1), 
                     limits = c(0, 0.03)) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "公安机关立案中强奸案件占比(全国)",
       subtitle = "1995-2018",
       caption = "来源：中国统计年鉴&中国法律年鉴") +
  theme_minimal() +
  theme_custom

ggsave("export/sexualcrime_case.pdf", dpi = 300, width = 8, height = 5)

############################# line plot 4 ###############################

showtext_auto()
ggplot(data = case_city[case_city$indicator=="发生率", ],
       aes(year, value, group = city, color = color)) +
  geom_line(size = 0.4) +
  geom_point(size = 0.8,
             shape = 21, 
             stroke = 0.8) +
  geom_point(color = "#fefdfb",
             size = 0.5) +
  geom_point(data = . %>% filter(!is.na(value)) %>%
               group_by(city) %>% filter(year%in%range(year)), # hignlight
             size = 1.5, 
             stroke = 0.8,
             color = "grey60") +
  geom_text(data = . %>% filter(!is.na(value)) %>%
              group_by(city) %>% filter(year%in%range(year)),
            aes(label = paste0(year, ": ", round(value, 4))),
            family = "gothic",
            size = 3.5, color = "grey40",
            hjust = 0.3, vjust = -1, 
            show.legend = FALSE) +
  geom_text(data = .  %>% filter(!is.na(value)) %>%
              group_by(city) %>% filter(year%in%max(year)),
            aes(label = city),
            family = "simyou",
            size = 4,
            hjust = 0.3, vjust = 2, 
            show.legend = FALSE) +
  scale_x_continuous(breaks = seq(2000, 2018, 2),
                     labels = seq(2000, 2018, 2),
                     limits = c(2000, 2020)) +
  scale_y_continuous(breaks = seq(0, 0.04, 0.01), 
                     labels = paste0(seq(0, 0.04, 0.01)), 
                     limits = c(0, 0.04)) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "强奸案犯罪率(用公安机关立案数量计算，部分城市)",
       subtitle = "2000-2018",
       caption = "来源：安徽省、上海市、东莞市的统计年鉴") +
  theme_minimal() +
  theme_custom
  

case_city[case_city$indicator=="发生率", ] %>%  filter(!is.na(value)) %>%
  group_by(city) %>% filter(year%in%max(year, na.rm = T))

ggsave("export/sexualcrime_city1.pdf", dpi = 300, width = 8, height = 5)

############################# flower shape ###############################

f = 0.5  # change to change shape of the "balloon"
cate_num = unique(case_sh$name) %>% length()

flower_shape <- case_sh %>% 
  filter(year%in%c(seq(2000, 2015, 3), 2017)) %>%
  rowwise() %>% 
  mutate(
    # Calculate points on circle for the "balloons", we need 4 x-y pairs for geom_bspline_closed
    x = list(c(0,
               f * percent * sin(case*2*pi/cate_num - pi/4),
               percent * sin(case*2*pi/cate_num), 
               f * percent * sin(case * 2*pi/cate_num + pi/5),
               0)
             ),
    y = list(c(0,
               f * percent * cos(case * 2 * pi / cate_num - pi/5),
               percent * cos(case * 2 * pi / cate_num), 
               f * percent * cos(case * 2 * pi / cate_num + pi/4),
               0)
             )
  ) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(year), names_from = case, values_from = c(x, y)) %>%
  unnest(x_1:y_7)

# Category colors
pal <- c("#3cb44b",
         "#e6194B",
         "#469990",
         "#42d4f4",
         "#4363d8",
         "#800000",
         "#f032e6")

casecate <- unique(case_sh$name)

showtext_auto()
ggplot(flower_shape) + 
  geom_point(aes(0, 0), size = 0.01, colour = "grey30")  + 

  geom_bspline_closed(aes(x_1, y_1, group = year, fill = pal[1]), alpha = 0.7) +
  geom_bspline_closed(aes(x_2, y_2, group = year, fill = pal[2]), alpha = 0.7) +
  geom_bspline_closed(aes(x_3, y_3, group = year, fill = pal[3]), alpha = 0.7) +
  geom_bspline_closed(aes(x_4, y_4, group = year, fill = pal[4]), alpha = 0.7) +
  geom_bspline_closed(aes(x_5, y_5, group = year, fill = pal[5]), alpha = 0.7) +
  geom_bspline_closed(aes(x_6, y_6, group = year, fill = pal[6]), alpha = 0.7) +
  geom_bspline_closed(aes(x_7, y_7, group = year, fill = pal[7]), alpha = 0.7) +

  scale_fill_identity(guide = guide_legend(title = "", nrow = 2, override.aes = list(alpha = 0.7, shape = 2)),
                      breaks = pal, labels = factor(casecate)) +
  coord_fixed() +
  facet_wrap(vars(year), ncol = 3) +
  labs(
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_void(base_family = "", base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(family = "simyou"),
    legend.margin = margin(20, 0, 0, 0),
    plot.background = element_rect(fill = "grey97", 
                                   colour = NA),
    strip.text.x = element_text(family = "simyou", 
                                size = 20, 
                                colour = "grey20", 
                                margin = margin(0, 0, 10, 0)),
    plot.title = element_text(margin = margin(20, 0, 10, 0), 
                              hjust = 0.5, 
                              size = 25, 
                              family = "simyou"),
    plot.subtitle = element_text(margin = margin(0, 0, 55, 0), 
                                 hjust = 0.5, 
                                 size = 18, 
                                 colour = "grey20"),
    plot.caption = element_text(margin = margin(40, 0, 0, 0), 
                                hjust = 0.5, 
                                colour = "grey20", 
                                family = "simyou"),
    plot.margin = margin(20, 20, 35, 20)
  ) 

ggsave("export/caseplot.png", dpi = 320, width = 15, height = 13.25)

######## reference
# background color: https://www.color-hex.com/color/faebd7
# https://www.color-hex.com/color-palette/809
