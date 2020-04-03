library(tidyverse)
library(colorblindr)
library(RColorBrewer)
library(showtext)

setwd("C:/Users/WXY/Documents/R_learning/atoolbox/income_ditribution")
# fonts
font_add("simyou", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/SIMYOU.TTF")
font_add("gothic", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/GOTHIC.TTF")

############ read in data ###############

income_total <- xlsx::read.xlsx("shanghai.xlsx", sheetIndex = 1, encoding = "UTF-8")
income_urban <- xlsx::read.xlsx("shanghai.xlsx", sheetIndex = 2, encoding = "UTF-8")
income_rural <- xlsx::read.xlsx("shanghai.xlsx", sheetIndex = 3, encoding = "UTF-8")

class(income_urban$year)
income_total <- income_total[!is.na(income_total$group), c(1:5)]
income_urban <- income_urban[!is.na(income_urban$group), c(1:5)]
income_urban$group <- as.character(income_urban$group)
income_urban$value <- as.numeric(as.character(income_urban$value))
income_urban$year <- as.numeric(as.character(income_urban$year))
income_urban$indicator <- as.character(income_urban$indicator)

display.brewer.pal(5, "Blues");  brewer.pal(5, "Blues")


##################### urban #####################

income_urban2 <- income_urban %>%
  select(group, indicator, year, value, area) %>%
  mutate(
    colour = case_when(
      group == "高收入户" ~ "#9c4b28",
      group == "中高收入户" ~ "#b94d5c",
      group == "中等收入户" ~ "#eceac7",
      group == "中低收入户" ~ "#b0c3c9",
      group == "低收入户" ~ "#3182BD",
      T ~ "#5d5e62"
    )
  )


showtext_auto()
ggplot(income_urban2 %>% filter(group!="总平均"&indicator=="可支配收入(元/人)"),
       aes(year, value, group = group, color = colour)) +
  geom_step(aes(size = colour == "#FF2B4F")) +
  geom_point(data = . %>% group_by(group) %>%
      top_n(1, value) %>%
      arrange(desc(value)) %>%
      head(5),
    shape = 21,
    aes(col = colour),
    size = 1,
    stroke = 1) +
  geom_text(data = . %>% group_by(group) %>%
              top_n(1, value) %>%
              arrange(desc(value)) %>%
              mutate(label = paste0(group, ": ", value)),
            aes(label = label, hjust = -0.08),
            family = "simyou",
            size = 2,
            fontface = "bold") +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_manual(values = c(0.4, 1), guide = F) +
  scale_size_manual(values = c(0.2, 0.5), guide = F) +
  scale_x_continuous(breaks = seq(1985, 2015, 5), 
                     labels = seq(1985, 2015, 5), 
                     limits = c(1985, 2020)) +
  # the lower limit is expanded by add[1]
  # and the upper limit by add[2].
  scale_y_continuous(position = "right",               
                     expand = expansion(add = c(0, 2500))) + 
  labs(title = "按收入水平分组城市居民家庭人均可支配收入, 元/人",
       subtitle = "1985-2014",
       caption = "\nSource: 上海统计年鉴 | @Yang") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#8894a1", colour = "#8894a1"),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.6), "cm"),
    panel.grid = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", size = 0.3),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.x = element_line(color = "gray85", size = 0.1),
    axis.text.x = element_text(family = "gothic", size = 6),
    axis.text.y = element_text(family = "gothic", size = 6),
    axis.ticks.length = unit(0.2, "cm"),
    text = element_text(size = 6),
    axis.line.x = element_line(size = 0.3, color = "gray85"),
    plot.title = element_text(family = "simyou", face = "bold", colour = "white", size = 10),
    plot.subtitle = element_text(family = "gothic", vjust = 2, colour = "white", size = 8),
    plot.caption = element_text(family = "simyou", colour = "white", size = 8)
  )

ggsave("plot_urban.pdf", width = 6, height = 4, dpi = 300)


##################### rural #####################
income_rural <- income_rural[, c(1:9)]
income_rural2 <- income_rural %>%
  tidyr::gather(., key = group, value = value, 总平均:高收入户, factor_key=FALSE)

class(income_rural2$indicator)
income_rural2$indicator <- as.character(income_rural2$indicator)

income_rural2 <- income_rural2 %>% 
  select(group, indicator, year, area, value) %>%
  mutate(
    colour = case_when(
      group == "高收入户" ~ "#9c4b28",
      group == "中高收入户" ~ "#b94d5c",
      group == "中等收入户" ~ "#eceac7",
      group == "中低收入户" ~ "#b0c3c9",
      group == "低收入户" ~ "#3182BD",
      T ~ "#5d5e62"
    )
  ) %>%
  unnest()

showtext_auto()
ggplot(data = income_rural2 %>% filter(!is.na(value)&group!="总平均"&indicator=="可支配收入(元/人)"),
       aes(year, value, group = group, color = colour)) +
  geom_step(aes(size = colour == "#FF2B4F")) +
  geom_point(data = . %>% group_by(group) %>%
               top_n(1, value) %>%
               arrange(desc(value)) %>%
               head(5),
             shape = 21,
             aes(col = colour),
             size = 1,
             stroke = 0.8) +
  geom_text(data = . %>% group_by(group) %>%
              top_n(1, value) %>%
              arrange(desc(value)) %>%
              mutate(label = paste0(group, ": ", value)),
            aes(label = label, hjust = -0.08),
            family = "simyou",
            size = 2,
            fontface = "bold") +
  geom_segment(aes(x = 1994, y = 0, xend = 1994, yend = 7000), 
               linetype = "longdash",
               colour = "grey40") +
  geom_text(aes(x = 1989, y = 3500, label = "无收入分组数据"),
            colour = "grey40", size = 3) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_manual(values = c(0.4, 1), guide = F) +
  scale_size_manual(values = c(0.2, 0.5), guide = F) +
  scale_x_continuous(breaks = seq(1985, 2014, 5), 
                     labels = seq(1985, 2014, 5), 
                     limits = c(1985, 2020)) +
  # the lower limit is expanded by add[1]
  # and the upper limit by add[2].
  scale_y_continuous(position = "right",               
                     expand = expansion(add = c(0, 2500))) + 
  labs(title = "按收入水平分组农村居民家庭人均可支配收入, 元/人",
       subtitle = "1994-2014",
       caption = "\nSource: 上海统计年鉴 | @Yang") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#8894a1", colour = "#8894a1"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.6), "cm"),
        panel.grid = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray85", size = 0.3),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.x = element_line(color = "gray85", size = 0.1),
        axis.text.x = element_text(family = "gothic", size = 7),
        axis.text.y = element_text(family = "gothic", size = 7),
        axis.ticks.length = unit(0.2, "cm"),
        text = element_text(size = 6),
        axis.line.x = element_line(size = 0.3, color = "grey85"),
        plot.title = element_text(family = "simyou", face = "bold", colour = "white", size = 10),
        plot.subtitle = element_text(family = "simyou", vjust = 2, colour = "white", size = 8),
        plot.caption = element_text(family = "simyou", colour = "white", size = 8)
  )

ggsave("plot_rural.pdf", width = 6, height = 4, dpi = 300)

# https://colorpalettes.net/color-palette-4098/


##################### comparison #####################
income_ru_ur <- rbind(
  income_rural2 %>% filter(group=="总平均"&indicator=="可支配收入(元/人)") %>%
    select(year, value, area), 
  income_urban2 %>% filter(group=="总平均"&indicator=="可支配收入(元/人)") %>%
    select(year, value, area),
  income_total %>% filter(group=="总平均"&indicator=="可支配收入(元/人)") %>%
    select(year, value, area) 
  )

income_ru_ur$area <- as.character(income_ru_ur$area)

income_ru_ur <- income_ru_ur %>% 
  mutate(
    area_label = case_when(
      area == "rural" ~ "农村",
      area == "urban" ~ "城市",
      area == "total" ~ "全市"),
      colour = case_when(
      area == "rural" ~ "#3182BD",
      area == "urban" ~ "#FB6A4A",
      T ~ "grey40"
    )
  )

library(ggtext)
library(lemon)
ggplot() +
  geom_line(data = income_ru_ur %>% filter(year>=1985),
                 aes(x = year, y = value, group = area, color = colour),
                 alpha = 0.8,
                 size = 1, 
                 linejoin = "mitre") +
  scale_x_continuous(breaks = seq(1985, 2019, 2), 
                     labels = seq(1985, 2019, 2), 
                     limits = c(1985, 2024)) +
  scale_y_continuous(breaks = seq(20000, 80000, 20000), 
                     labels = seq(20000, 80000, 20000),
                     limits = c(0, 80000)) +
  geom_text(data = income_ru_ur %>% filter(year>=1985)%>% group_by(area) %>%
              top_n(1, value) %>%
              arrange(desc(value)) %>%
              mutate(label = paste0(area_label, ": ", value)),
            aes(x = year, y = value, label = label, hjust = -0.08, color = colour),
            family = "simyou",
            size = 3,
            fontface = "bold") +
  scale_color_identity() +
  scale_alpha_manual(values = c(0.15, 1)) +
  labs(
    title = "分城乡的人均可支配收入，元/人",
    subtitle = glue::glue("1985-2019 未剔除价格因素，<span style='color:#FB6A4A'>城市</span>与<span style='color:#FD8D3C'>农村</span>以及<span style='color:#7A7979'>全市</span>人均可支配收入"),
    caption = "\nSource: 上海统计年鉴 | @Yang"
  ) +
  theme_minimal(base_family = "simyou", base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 30, 20, 20),
    plot.background = element_rect(fill = "#8894a1", colour = "#8894a1"),
    axis.title = element_blank(),
    axis.ticks.x = element_line(color = "grey85", size = 0.1),
    axis.text.x = element_text(family = "gothic", color = "grey30", size = 6),
    axis.text.y = element_text(family = "gothic", color = "grey30", size = 6),
    axis.line.x = element_line(size = 0.3, color = "grey85"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "gray85"),
    panel.grid.minor.y = element_blank(),
    plot.title = element_markdown(family = "simyou", face = "bold", colour = "white", size = 10),
    plot.caption = element_text(family = "simyou", colour = "white", size = 8),
    plot.subtitle = element_markdown(family = "simyou", vjust = 2, colour = "white", size = 8)
  )

ggsave("plot_comparison.pdf", width = 6, height = 4, dpi = 300)

##################### share of life expense #####################

expense_urban <- income_urban %>%
  tidyr::spread(., indicator, 5) %>%
  filter(year>=1985&year<=2014&group!="总平均") %>%
  select(group, area, year, `可支配收入(元/人)`, `消费支出(元/人)`) %>%
  mutate(share = (`消费支出(元/人)`/`可支配收入(元/人)`),
         label = glue::glue("{round(share * 100, 0)}%"),
    colour = case_when(
      group == "高收入户" ~ "#9c4b28",
      group == "中高收入户" ~ "#b94d5c",
      group == "中等收入户" ~ "#eceac7",
      group == "中低收入户" ~ "#b0c3c9",
      group == "低收入户" ~ "#3182BD",
      T ~ "#5d5e62"
    )
  ) 
  

quantile(expense_urban$share[which(expense_urban$group=="高收入户")],na.rm = T)
quantile(expense_urban$share[which(expense_urban$group=="中高收入户")],na.rm = T)
quantile(expense_urban$share[which(expense_urban$group=="低收入户")],na.rm = T)

expense_urban$group <- factor(expense_urban$group, levels = c("高收入户", "中高收入户", "中等收入户", "中低收入户", "低收入户"))

showtext_auto()
ggplot(data = expense_urban %>% 
         filter(year%in%c(seq(1985,2014,5),2014,2019))) +
  geom_bar(aes(x = '', y = share, fill = colour, color = colour),
    stat = 'identity', show.legend = FALSE) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_polar('y', start = 0) +
  geom_text(aes(x = 0, y = 0.65, label = label),
            family = "gothic",
            size = 3.5,
            color = "grey90",
            fontface = "bold") +
  facet_grid(group ~ year, switch = 'y') +
  labs(
    x = '',
    y = '',
    title = "城市家庭人均生活消费支出在人均可支配收入中的占比，%",
    subtitle = "1985-2014",
    caption = '\nSource: 上海统计年鉴 | @Yang'
  ) +
  theme_minimal(base_family = "simyou", base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.x = unit(0.9, 'lines'),
    panel.spacing.y = unit(0.9, 'lines'),
    axis.title = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(angle = 0, 
                                hjust = 0, size = 9,
                                margin = margin(b = 10)),
    strip.text.y = element_text(angle = 90, 
                                hjust = 1, size = 8,
                                margin = margin(r = 10, l =12)),
    plot.margin = margin(20, 30, 20, 20),
    plot.background = element_rect(fill = "#8894a1", colour = "#8894a1"),
    plot.title = element_text(family = "simyou", 
                                  face = "bold", 
                                  colour = "white", 
                                  size = 10),
    plot.caption = element_text(family = "simyou", 
                                colour = "white", 
                                size = 8),
    plot.subtitle = element_markdown(family = "gothic", 
                                     vjust = 2,
                                     colour = "white", size = 8)
  )

ggsave("plot_share.pdf", width = 8, height = 6, dpi = 300)

# Reference
# https://github.com/jwatzek/tidytuesday/blob/master/scripts/2020-08_food_carbon.R
# https://github.com/gkaramanis/FIBA-Basketbal-World-Cup/blob/master/R/teampoints-perc.R
# https://github.com/Z3tt/TidyTuesday/blob/master/R/2020_07_HotelBookings.Rmd
# [colour] https://colorpalettes.net/color-palette-4176/

