library(showtext)
library(ggrepel)


paw_img <- "data/paw_icon.png"
magick::image_read(paw_img) %>% # red image
  magick::image_colorize(., 50, "red") %>% 
  magick::image_write(path = "data/paw_icon2.png")
paw_img2 <- "data/paw_icon2.png"

font_add("simyou", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/SIMYOU.TTF")
font_add("gothic", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/GOTHIC.TTF")

# export region
export_flow <- region_liv %>% 
  filter(Exporter=="CN"&Quantity >= 100000) %>% 
  select(Importer, admin_im, Quantity) %>%
  arrange(Quantity) %>% 
  mutate(Quantity2 = round(Quantity/100000)) %>%
  mutate(qnt = map2(0, Quantity2, seq, by = 1)) %>%
  unnest(qnt) 

sum(export_flow2$Quantity)

quantile(country_flow$Quantity[country_flow$type=="import"])

# regions from which import animals
import_flow <- country_flow %>% 
  filter(type=="import"&Quantity >= 100000) %>% 
  select(admin_ex, Quantity) %>%
  mutate(
    Quantity2 = round(Quantity/100000)) %>%
  mutate(
    qnt = map2(0, Quantity2, seq, by = 1)
  ) %>%
  arrange(Quantity) %>%
  unnest(qnt) 

# compositions by class
quant_byclass <- cn_live %>% 
  filter(cn_live$Importer%in%unique(export_flow$Importer)) %>% 
  select(Class, Quantity) %>% 
  group_by(Class) %>% summarise_all(list(sum)) %>% 
  arrange(desc(Quantity))

quant_byclass <- data.frame(y = c(1, 4, 7, 10, 12, 14), 
                            quantity = quant_byclass$Quantity, 
                     img = paste0("data/",c( "snake", "bird", "deer", "fish", "frog", "shark"),"_icon.png"),
                     text = c("爬行纲", "鸟纲", "哺乳纲", "辐鳍鱼纲", "两栖纲", "板鳃亚纲"),
                     text_en = quant_byclass$Class, stringsAsfactor = F)


make_plot <- function(data=NULL, var.x=NULL, var.y=NULL, ordercol=NULL, img=NULL){
  ggplot(data, 
         aes(data[,var.x], factor(fct_reorder(data[,var.y], data[,ordercol])))) +
    geom_image(aes(image = img), size = 0.035, asp = 1.2) +
    scale_x_continuous(breaks = seq(0,max(data[,var.x]),round(max(data[,var.x])/5)), 
                       #labels =as.character(seq(0, 10, 2)),
                       limits =  c(0, max(data[,var.x])+2)) +
    labs( x = "交易量(×100000只)") +
    theme_minimal() +
    theme(
      text = element_text(family = "simyou", 
                          size = 8),
      axis.text.x = element_text(color = "#9c4b28", 
                                 size = 10),
      axis.text.y = element_text(family = "gothic",
                                 color = "white", 
                                 size = 10),
      axis.title.x = element_text(family = "simyou", 
                                  color = "#9c4b28",
                                  size = 10, 
                                  lineheight = 1,
                                  margin = margin(10, 0, 0, 0)),
      axis.title.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "#e4815d",
                                        size = 0.8),
      panel.grid.major.y = element_blank(),
      plot.background = element_rect(fill = "#8894a1", 
                                     color = "#8894a1"), #c6ccd8
      plot.caption = element_text(family = "gothic",
                                  color = "white", 
                                  size = 8,
                                  hjust = 1, vjust = 1)
    )
}
par(mar=rep(0.1,4))
showtext_auto()
expflow_plot <- make_plot(data=export_flow, var.x = "qnt", var.y="admin_im", ordercol="Quantity", img=paw_img)
expflow_plot
impflow_plot <- make_plot(data=import_flow, var.x = "qnt", var.y="admin_ex", ordercol="Quantity", img=paw_img2)
impflow_plot


class_plot <- ggplot(data = quant_byclass) + 
  geom_point(aes(x = 2.5, y = y, size = quantity), 
             colour = "grey80", alpha = 0.7) +
  geom_text(aes(x = 2.5, y = y, label = quantity), # point label
            check_overlap = TRUE, 
            family = "gothic", size = 4, color = "#e4815d") +
  scale_size_continuous(guide = FALSE, range = c(1, 20)) +
  geom_text(aes(x = 1, y = y, label = text),       # class names
            family = "simyou", 
            color = "white",size = 3) +
  geom_text(aes(x = 1, y = y, label = text_en), 
            family = "gothic", vjust = 2,
            color = "white",size = 3) +
  geom_image(aes(x = -1, y = y, image = quant_byclass$img), 
             size = 0.1, asp = 1.2) +
  xlim(-2, 3) +
  ylim(-1, 14) +
  labs(x = "表中12个地区进口的六个总纲和亚纲的活体动物，\n以出口方向濒危动物公约秘书处的报告数加总") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#8894a1", 
                                   color = "#8894a1"),
    axis.title.x = element_text(size = 10, 
                                face = "bold", 
                                color = "#9c4b28",
                                family = "simyou", 
                                lineheight = 1,
                                margin = margin(15, 0, 0, 0)),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank())
class_plot
title <- ggdraw() + 
  draw_text("其他地区从中国进口的野生动物数目                              \n1976-2018年间交易总量10万只以上的地区(左)，及其物种构成(右)",
             size = 12, family = "simyou", colour = "white") +
  theme(
    plot.background = element_rect(fill="#8894a1", color = "#8894a1"),
    text = element_text(lineheight = 1, hjust = 2)
  )
title
caption <- ggdraw() +
  draw_label("Source: CITES | @Yang",
             size = 8, 
             fontfamily = "gothic", 
             colour = "white",
             hjust = -2.5) +
  theme(plot.background = element_rect(fill="#8894a1", color = "#8894a1") )
caption
p <- plot_grid(expflow_plot, class_plot, rel_widths = c(2, 1)) +
  theme(plot.margin = margin(10, 30, 10, 30),
    plot.background = element_rect(fill="#8894a1", color = "#8894a1"))
p
grid_plot <- plot_grid(title, p, caption, ncol = 1, rel_heights = c(0.15, 1, 0.15))

ggsave("fig/grid_plot8.pdf", grid_plot, dpi = 300, height = 7, width = 10)


# Reference
# [code] https://github.com/gkaramanis/tidytuesday
# [colour] https://colorpalettes.net/color-palette-4176/
# [image] https://commons.wikimedia.org/wiki/File:Paw_(Animal_Rights_symbol).png
# 
