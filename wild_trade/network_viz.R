
library(ggplot2);library(RColorBrewer)

edge_city_16 <- readRDS('data/edge_city_16.rds')
############################### relation between variables ###################################
city_data <- openxlsx::read.xlsx('C:/Users/WXY/Desktop/地级市.xlsx', sheet = 1)
city_data$city <- str_squish(city_data$city)
city_df <- data.frame(city=V(g_city_16)$name,betweenness=betweenness(g_city_16),degree=degree(g_city_16),stringsAsFactors = F)
city_df <- dplyr::left_join(city_df,city_data[,c(1,3:6)],by="city")
names(city_df) <- c("city","betweenness","degree","world_flow","world_rev","country_flow","country_rev")

options(scipen = 999)
cor_city <- cor.test(city_df$betweenness, city_df$country_flow) # betweenness, tourist flow

betwen_flow_p <- 
  ggplot(city_df[city_df$city!="重庆市"&!is.na(city_df$country_flow),],
         aes(betweenness, country_flow,fill=degree))+
  geom_point(aes(size = betweenness))+
  theme_classic()+
  #scale_x_continuous(breaks=c(0.6,0.7,0.8,0.9),limits=c(0.6,0.7,0.8,0.9)) +
  geom_smooth(method = "auto", se = F, color = "darkmagenta") +
  geom_text(aes(label=city,size=betweenness),)+
  expand_limits(y=c(0,75000000)) +
  geom_text(aes(x=5000, y=75000000,label='estimated correlation= 0.3141'))

ggsave(paste0('export/betwen_flow_p2.pdf'), betwen_flow_p, width = 7, height = 7)


#---------------------------- join gdp and tourist data ---------------------------------- 

mydata <- xlsx::read.xlsx("")

############################### plot network ############################### 

library(igraph)

g_city_16 <- graph.data.frame(edge_city_16)
g_city_16 <- as.undirected(g_city_16,mode = 'collapse',edge.attr.comb = "mean")
g_city_16 <- simplify(g_city_16)
plot(g_city_16,vertex.size=log(betweenness(g_city_16)),vertex.label=NA)

E(g_city_16)[get.edge.attribute(g_city_16,name = "vertex names")=='宁德市']

# ---------------------------------- Changjiang Delta ------------------------------------------ 

csj <- paste0(c('上海','南京','无锡','常州','苏州','南通','盐城','扬州','镇江','泰州',
                '杭州','宁波','嘉兴','湖州','绍兴','金华','舟山','台州','合肥','芜湖',
                '马鞍山','铜陵','安庆','滁州','池州','宣城'),'市')

station_16$citygroup[station_16$city%in%csj] <- '长三角'
csj_mat <- as.matrix(g_city_16[V(g_city_16)$name%in%csj])
g_csj <- delete.vertices(g_city_16,!V(g_city_16)$name%in%csj)

plot(g_csj)

# ---------------------------------- 1. network ------------------------------------------ 

library(network);library(sna)
library(GGally)
# root URL
r = "https://raw.githubusercontent.com/briatte/ggnet/master/"

# read nodes
v = read.csv(paste0(r, "inst/extdata/nodes.tsv"), sep = "/t")
names(v)

# read edges
e = read.csv(paste0(r, "inst/extdata/network.tsv"), sep = "/t")
names(e)

# random graph
net = network(station_2016_edge, directed = TRUE, bipartite = F)
network <- intergraph::asNetwork(station_2016_edge)
network.vertex.names(net) = station_2016_node$
  x = data.frame(name = network.vertex.names(net))
x = merge(x, station_2016_node, by = "name", sort = FALSE)$Groupe
net %v% "party" = as.character(x)

y = colorRampPalette(rev(brewer.pal(11, 'Spectral')))(29)
names(y) = unique(station_2016_node$province)


# ---------------------------- 2. overlaying network on map ---------------------------- 
pacman::p_load(geosphere)
# Add a point on the map for each station:
gadm36_CHN_1 <- rgdal::readOGR('C:/Users/hello/Downloads/R_learning/map/data/gadm36_CHN_shp/gadm36_CHN_1.shp', use_iconv = T, encoding = 'utf-8', stringsAsFactors = F)
plot(gadm36_CHN_1)
points(x=city_2016_node$long, y=city_2016_node$lat, pch=19, col="orange")
col.1 <- adjustcolor("orange red", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)
for(i in which(!is.na(edge_city_16[,1:2]))) {
  node1 <- city_2016_node[city_2016_node$city == edge_city_16[i,]$city.x,]
  node2 <- city_2016_node[city_2016_node$city == edge_city_16[i,]$city.y,]
  
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat), 
                         c(node2[1,]$long, node2[1,]$lat), 
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*edge_city_16[i,]$weight / max(edge_city_16$weight))
  
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

# https://kateto.net/network-visualization

# -------------------------------- 3. networkd3 ------------------------------------------

library(networkD3)
# Create fake data
src <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)
# Plot simpleNetwork
simpleNetwork <- simpleNetwork(edge_city_16[!is.na(edge_city_16$city.x)&!is.na(edge_city_16$city.y),])
# Load data
data(MisLinks)
data(MisNodes)
# Plot forceNetwork
network_2016 <- forceNetwork(Links = edge_city_16, Nodes = station_16[!duplicated(station_16$city),c("city","province")], 
                             Source = "city.x", Target = "city.y", Value = "weight", NodeID = "city",
                             Group = "province", opacity = 0.7,
                             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"))
simpleNetwork %>% saveNetwork(file = 'simple.html')


F2 <- colorRampPalette(c("#FFFF00", "#FF0000"), bias = 1130, space = "rgb", interpolate = "linear")
edges_col <- sapply(edge_city_16$province, function(x) colCodes[which(sort(unique(edge_city_16$province)) == x)])
colCodes <- F2(length(unique(edge_city_16$province)))
forceNetwork(Links = edge_city_16, Nodes = city_location, Source = "city.x", Target = "city.y",
             Value = "weight", NodeID = "city", Nodesize = "betweeness", Group = "degree",
             height = 500, width = 1000, fontSize = 20, 
             linkDistance = networkD3::JS("function(d) { return 10*d.value; }"), 
             linkWidth = networkD3::JS("function(d) { return d.value/5; }"),
             opacity = 0.85, zoom = TRUE, opacityNoHover = 0.1, linkColour = edges_col)

# https://www.r-bloggers.com/network-visualization-part-6-d3-and-r-networkd3/
# https://github.com/christophergandrud/networkD3

# Use igraph to draw the graph and find membership
wc <- cluster_walktrap(g_city_16)
members <- membership(wc)

# Convert to object suitable for networkD3
g_d3 <- igraph_to_networkD3(g_city_16,group = clusters(g2)$membership)

# Create force directed network plot
forceNetwork(Links = g_d3$links, Nodes = g_d3$nodes, 
             Source = 'source', Target = 'target', NodeID = 'name',fontSize = 14,
             Group = 'group')%>%
  saveNetwork(file = 'g_d3.html')

forceNetwork(Links = city_16_d3$links, Nodes =  city_16_d3$nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.7,
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"))
simpleNetwork(networkData) %>% saveNetwork(file = 'csj.html')

# --------------------------------  map --------------------------------
# https://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/library(maps)
library(geosphere)
library(maps)

airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE)
flights <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/flights.csv", header=TRUE, as.is=TRUE)

# Unique carriers
carriers <- unique(flights$airline)

# Color
pal <- colorRampPalette(c("#333333", "white", "#1292db"))
colors <- pal(100)

# Limiting boundaries
cn <- map("world",regions = "china")
xlim <- cn$range[c(1,2)]
ylim <- cn$range[c(3,4)]

par(mar=c(0,0,0,0),mgp=c(0,0,0,0))
pdf("railnet.pdf", width=11, height=7)

map("world", col="#191919", fill=TRUE, bg="#000000", lwd=0.05, xlim=xlim, ylim=ylim)
#fsub <- flights[flights$airline == carriers[i],]
#fsub <- fsub[order(fsub$cnt),]
j <- which(!is.na(edge_19_weight$city.x)&!is.na(edge_19_weight$city.y))[1]
maxwgt <- max(edge_19_weight$weight)

for (j in nNA){
  city1 <- city_location[city_location$city==edge_19_weight$city.x[j],5:6]
  city2 <- city_location[city_location$city==edge_19_weight$city.y[j],5:6]
  
  inter <- gcIntermediate(c(city1[1,]$long.2, city1[1,]$lat.2), c(city2[1,]$long.2, city2[1,]$lat.2), n=100, addStartEnd=TRUE)
  
  colindex <- round( (edge_19_weight[j,]$weight / maxwgt) * length(colors) )
  
  lines(inter, col=colors[colindex], lwd=0.6)
} 

dev.off()

