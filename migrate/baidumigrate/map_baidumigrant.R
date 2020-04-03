pacman::p_load(geosphere, maps, maptools, sf, ggspatial)
pacman::p_load(openxlsx, tidyverse, magrittr, showtext, RColorBrewer, shadowtext, ggrepel, igraph)

options(scipen = 999)
setwd("C:/Users/WXY/Documents/R_learning/whosedata/migrant/baidu_migrant")

##################### load in data #####################

city_data <- read.xlsx("city_data.xlsx", sheet = 1)
flow_intensity <- read.xlsx("flow_intensity.xlsx", sheet = 1)
city_list <- read.xlsx("city_list.xlsx")[,-1]

## map layers
st_layers("C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", do_count = TRUE)
border <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", layer = "BOUL国界线")
city <- st_read(dsn = "C:/Users/WXY/Documents/R_learning/spatial/data/全国基础地理数据库2017版.gdb", layer = "地级市")

# add font
font_add("simyou", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/SIMYOU.TTF")
font_add("gothic", "C:/Users/WXY/AppData/Local/Microsoft/Windows/Fonts/GOTHIC.TTF")
font_add("fzyaoti", "C:/Windows/Fonts/FZYTK.TTF")


##################### tidy data #####################

## calculate average population flow intesity
ave_index <- flow_intensity %>%
  mutate(date = as.numeric(date),
         city_name = gsub("地区", "", city_name)) %>%
  filter(date >= 20200125) %>%
  group_by(city_name) %>%
  summarise(ave_index = mean(index))

city_flow <- city_data %>% 
  filter(value > 0.01) %>%  # delete flow less than 0.01
  mutate(city_name = gsub("市|地区", "", city_name),
         city_to = gsub("市|地区", "", city_to)) %>%
  left_join(., ave_index, by = c("city_to" = "city_name")) %>%
  mutate(value_weighted = value*ave_index) %>%
  select(city_name, city_to, value_weighted, province_name) %>%
  left_join(., city_list[, c("city_name", "long", "lat")], by = c("city_name" = "city_name")) %>%
  left_join(., city_list[, c("city_name", "long", "lat")], by = c("city_to" = "city_name"))


##################### flow network #####################
## transform data.frame to network
g <- graph_from_data_frame(city_flow[, c("city_name", "city_to")], directed = T, vertices = city_list$city_name)
E(g)$weight <- city_flow$value_weighted
g
is_weighted(g)

degree_df <- data.frame(city = V(g)$name, degree_in = strength(g, mode = "in"),
                        degree_out = strength(g, mode = "out"), stringsAsFactors = F)  %>%
  left_join(., city_list[, c("city_name", "province_name")], by = c("city" = "city_name")) %>%
  arrange(desc(degree_in))

#g.undir <- as.undirected(g, mode = 'collapse', edge.attr.comb = "mean")
#g.undir
#city_flow_undir <- igraph::as_edgelist(g.undir)


##################### flow routes #####################

## calculate routes between flow-in city and flow-out city 
routes <- gcIntermediate(city_flow[, c("long.x","lat.x")], city_flow[, c("long.y","lat.y")], 100, breakAtDateLine=FALSE, addStartEnd=FALSE, sp=TRUE)
class(routes) # SpatialLines object
saveRDS(routes, "routes.rds")
#routes <- readRDS("routes.rds")

# Convert a SpatialLines object into SpatialLinesDataFrame
#ids <- data.frame(ID=NULL)
#for (i in 1:length(routes)) {
#  id <- data.frame(ID = routes@lines[[i]]@ID) # data frame with IDs for each line
#  ids <- rbind(ids, id) 
#}
# or:
ids <- data.frame(ID = as.character(1:length(routes@lines)), stringsAsFactors = F)

# convert SpatialLines into SpatialLinesDataFrame using IDs 
routes <- SpatialLinesDataFrame(routes, data = ids, match.ID = T)


##################### map #####################

## route
routes_sf <- st_as_sf(routes) %>% st_transform(crs = st_crs(city))
city_flow$id <- as.character(c(1:nrow(city_flow)))
routes_sf <- routes_sf %>% left_join(., city_flow[, c("id", "value_weighted", "city_name", "city_to")],
                                     by= c("ID"="id"))
st_crs(routes_sf)

## point
city_list.sf <- city_list %>% 
  st_as_sf(coords = c("long", "lat"), dim = "XY") %>% 
  st_set_crs(4326) %>%
  st_cast("POINT")

display.brewer.pal(9, "Blues"); brewer.pal(9, "Blues")

theme_custom <- theme(legend.position= "none",
                      text = element_text(family = "simyou"),
                      panel.spacing = unit(c(0, 0,0,0),"cm"),
                      plot.margin = unit(seq(0.05, 4),"cm"),
                      plot.background = element_blank(),
                      panel.background = element_blank(), 
                      panel.border = element_blank(), 
                      panel.grid.minor = element_blank(), 
                      panel.grid.major=element_blank(), 
                      axis.ticks = element_blank(), 
                      axis.text.x = element_blank(), 
                      axis.text.y = element_blank(),
                      axis.title = element_blank(),
                      plot.title = element_text(family = "fzyaoti", face = "bold", colour = "#C6DBEF", size = 14),
                      plot.subtitle = element_text(family = "gothic", vjust = 2, colour = "#C6DBEF", size = 12),
                      plot.caption = element_text(family = "gothic", colour = "white", size = 10)
                      
)

# plot lines
par(mar = seq(0.1, 4))
showtext_auto()

# #363b74 #280a46 #133072
p <- ggplot() + 
  geom_sf(data = routes_sf, 
          aes(alpha = value_weighted, size = (value_weighted)/max(value_weighted), colour = (value_weighted)/max(value_weighted)),
          linetype = "solid") +
  scale_colour_gradient2(low = "#08519C", mid = "#DEEBF7", high = "white") +
  scale_size_continuous(guide = FALSE, range = c(0.01, 0.5)) + # scale for widths
  scale_alpha_continuous(range = c(0.01, 0.5)) +
  theme_custom +
  theme(plot.background = element_rect(fill = "#050831", color = NA),
        panel.background =element_rect(fill = "#050831", color = NA)) +
  labs(title = "",
       subtitle = "",
       caption = "") +
  coord_sf(expand = FALSE) 

ggsave("p_allcity.pdf", p, width = 8, height = 6, dpi = 300)


######################### city cluster #########################

jjj <- c('北京','天津','保定','唐山','廊坊','石家庄','秦皇岛','张家口','承德','沧州','衡水','邢台','邯郸','定州','辛集')
zsj <- c("广州","佛山","肇庆","深圳","东莞","惠州","珠海","中山","江门","阳江","韶关","清远","云浮","汕尾","河源")
csj <- c('上海','南京','无锡','常州','苏州','南通','盐城','扬州','镇江','泰州',
         '杭州','宁波','嘉兴','湖州','绍兴','金华','舟山','台州',
         '合肥','芜湖', '马鞍山','铜陵','安庆','滁州','池州','宣城')
cy <- c('成都','自贡','泸州','德阳','绵阳','遂宁','内江','乐山','南充','眉山','宜宾','广安','达州','雅安','资阳','重庆')
bbw <- c('南宁','北海','钦州','防城港','玉林','崇左','湛江','茂名','阳江',
         '海口','儋州','东方','澄迈县','临高县','昌江县')
zy <- c('郑州','开封','洛阳','平顶山','新乡','焦作','许昌','漯河','济源','鹤壁','商丘','周口',
        '晋城','亳州')
xj <- c('乌鲁木齐','石河子','昌吉回族自治州','石河子','五家渠','喀什')
# or see province: 四川省,c("江苏省","浙江省","上海市"),c("北京市","天津市","河北省")
#flow_high = city_list$city_name[city_list$province_name=="广东省"] 

getcitymap <- function(x, label = c(TRUE, FALSE)){
  pcity <- ggplot() + 
  geom_sf(data = routes_sf %>% filter(city_to%in%x), 
          aes(alpha = value_weighted, size = (value_weighted)/93.2025, colour = (value_weighted)/93.2025),
          linetype = "solid") +
  scale_colour_gradient2(low = "#08519C", mid = "#DEEBF7", high = "white") +
  scale_size_continuous(guide = FALSE, range = c(0.01, 0.5)) + # scale for widths
  scale_alpha_continuous(range = c(0.01, 0.5)) +
  theme_custom +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background =element_rect(fill = "transparent", color = NA)) +
  coord_sf(expand = FALSE) 
  if(label==TRUE){
    pcity  <- pcity +
      geom_text_repel(data = city_list %>% filter(city_name%in%x),
                      aes(x = long, y = lat, label = city_name),
                      size= 2, segment.size = 0.05, segment.color = "#FDD0A2",
                      color="white", family = "simyou", fontface = "bold") +
      theme(plot.background = element_rect(fill = "#050831", color = NA),
            panel.background =element_rect(fill = "#050831", color = NA))
  }
  return(pcity)
}

getcitymap(bbw, label = FALSE) %>%
  ggsave("p_bbw.pdf", ., width=8, height=6, bg = "transparent")
getcitymap(bbw, label = TRUE) %>%
  ggsave("p_bbw_label.pdf", ., width=8, height=6, bg = "transparent")

######################### city with large flows #########################

# label
geom_text_repel(data = city_list %>% filter(city_name%in%flow_high),
                aes(x = long, y = lat, label = city_name),
                size= 2, direction = "y", nudge_x = 1, segment.size = 0.1, segment.color = "#FDD0A2",
                color="white", family = "simyou", fontface = "bold") 

showtext_auto()
# polygons
p2 <- p + 
  geom_text_repel(data = city_list %>% mutate(code = str_sub(city_list$city_code, 3, 6)) %>%
                  filter(code%in%c("0100", "0000")),
                aes(x = long, y = lat, label = city_name),
                size= 2, direction = "y", nudge_x = 1, segment.size = 0.3, segment.color = "grey60",
                color="white", family = "simyou", fontface = "bold")


city$name <- as.character(city$name) %>% gsub("市|地区", "", .)

display.brewer.pal(9, "Oranges"); brewer.pal(9, "Oranges")

p3 <- p + 
  geom_sf(data = city %>% filter(name%in%degree_df$city[1:50]), 
          colour = "#FEE6CE", fill = NA, size = 0.05, alpha = 1,
          linetype = "dotted") +
  theme_custom +
  coord_sf(expand = FALSE) 

ggsave("p2_1.pdf", p, width = 8, height = 6, dpi = 300)

# background color
# https://www.color-hex.com/color-palette/87912
# https://colorpalettes.net/color-palette-3860/
# https://colorpalettes.net/color-palette-3860/ #050831 #d5e6f7 #133072
