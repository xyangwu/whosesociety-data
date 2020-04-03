library(stringr)
library(ggplot2)
library(showtext)
library(ggmap)

############################# load in data ###############################
verdict_rape2 <- readxl::read_excel("export/verdict_rape.xlsx", sheet = 1)
verdict_rape2 <- openxlsx::read.xlsx("export/verdict_rape.xlsx", sheet = 1)
sh.b1 <- readRDS('C:/Users/hello/Documents/R_learning/segregation/data/sh_b4.rds')

sh.b4 <- get_stamenmap(bbox = c(120.8523+0.1, 30.68147, 121.9740+0.1, 31.86613), zoom = 11, maptype = "watercolor")
sh.b1$data
bb <- attr(sh.b1$data, "bb") # shanghai map boundary

############################# tidy data ###############################

verdict_rape2$spot_time 
verdict_rape2$spot_time2 <- verdict_rape2$spot_time %>% 
  as.numeric(as.character()) %>% 
  as.Date(., origin = "1899-12-30") %>% as.character()
  #janitor::excel_numeric_to_date(., date_system = "modern")
na <- verdict_rape2$spot_time2 %>% 
  is.na(.) %>% 
  which() 
verdict_rape2$spot_time2[na] <- verdict_rape2$spot_time[na]

data <- verdict_rape2 %>% 
  select(judge_num, accused_hukou, accused_birth, accused_ethnic, spot, spot_time2)

data <- data %>% mutate(year_judg =  str_sub(judge_num, 2, 5), 
                        year_spot = str_sub(spot_time2, 1, 4)) # %>% group_by(year) %>% summarise(rows = nrow(.))

range(data$year[data$year > 2009], na.rm = T)
table(data$year_judg)
which(data$year=="2007") %>% verdict_rape$content[.]
############################# get coordinate #############################
pacman::p_load(baidumap)

options(baidumap.key = 'your APIkey')
for (i in which(is.na(verdict_rape$lat))) {
  
  city = "上海市"
  place = verdict_rape$place[i]%>%gsub("上海市|上海","",.)%>%paste0(str_sub(city,4),.)
  
  trynext <- try({
    data[i, c('long', 'lat')] <- getPlace(place,str_sub(city,1,2),pages = 1)[1, c('lon', 'lat')]
  }) 
  if('try-error' %in% class(trynext)) next
  
}

which(verdict_rape$long > 121.9991)
which(verdict_rape$long < 120.8565)
which(verdict_rape$lat < 30.6704)
which(verdict_rape$lat > 31.8672)
verdict_rape[192,]

############### theme ################


font_add('hanyi', 'HanYiChangSongJian-1.ttf') #add font family
font_add('courier', 'cour.ttf', 'courbd.ttf')
font_families();showtext_auto()

theme_base <- theme(panel.grid.minor = element_blank(), # theme #
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         legend.title = element_text(size = rel(1), family = "hanyi",face = 'bold',lineheight = 1.2),
                         legend.position = c(0, 0.9),
                         legend.justification = c(0, 1),
                         legend.background = element_rect(fill= alpha('white', 0)),
                         legend.key.size = unit(0.3, "cm"),
                         #legend.text = element_text(size = 10, hjust = 1.3, face = 'bold'),
                         #legend.spacing.y = unit(c(1, 0, 0, 0), 'lines'),
                         plot.title = element_text(size=16,  family="hanyi",face="bold"),
                         plot.subtitle=element_text(size=12, family="hanyi",face="bold"),
                         plot.margin = unit(c(1, 0, 0, 0), 'lines'))


############### plot ################
sh_crime_map <- sh.b1 +
  #geom_polygon(data = sh_street_df, 
   #            aes(x = long, y = lat, group = group, fill = pop_densit),
    #           alpha = 0.5) +
  
  #geom_path(data = sh_street_df, aes(x = long, y = lat, group = group), 
            #colour='grey40', alpha=0.5, lwd= 0.4) +
  
  #scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'Spectral')[c(1:6, 10)]))(212), name="人口密度\n（人/平方公里）") + # add interesting scale
  
  #geom_path(data = sh_district_df, aes(x = long, y = lat, group = group),
            #colour = 'grey40', alpha = 0.8, lwd = 1.1) +
  
  geom_point(data = judgement_rape_info, aes(x = long, y = lat), shape = 1,
            colour = "lightcoral", fill = "lightcoral", alpha = 0.7) +
  #stat_bin_2d(data = judgement_rape_info, aes(x = long, y = lat),
   #           colour = "lightcoral", fill = "lightcoral", bins = 50, alpha = 0.4)

  labs(title = "上海性犯罪地点分布",
       subtitle = "构成：2378",
       caption = "{ggmap}")

ggsave('export/sh_crime_map2.pdf', sh_crime_map, width = 10, height = 8)
