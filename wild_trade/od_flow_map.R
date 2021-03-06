library(plyr)
library(ggplot2)
library(maptools)

download.file(url= "http://marlin.casa.ucl.ac.uk/~james/wu03ew_v1.csv.zip", 
              destfile="data/wu03ew_v1.csv.zip")
unzip("data/wu03ew_v1.csv.zip")

download.file(url="http://marlin.casa.ucl.ac.uk/~james/msoa_popweightedcentroids.csv",
              destfile="msoa_popweightedcentroids.csv")

input<-read.table("wu03ew_v1.csv", sep=",", header=T)

input<- input[,1:3]
names(input)<- c("origin", "destination","total")

centroids<- read.csv("msoa_popweightedcentroids.csv")

or.xy<- merge(input, centroids, by.x="origin", by.y="Code")
names(or.xy)<- c("origin", "destination", "trips", "o_name", "oX", "oY")
dest.xy<- merge(or.xy, centroids, by.x="destination", by.y="Code")
names(dest.xy)<- c("origin", "destination", "trips", "o_name", "oX", "oY","d_name", "dX", "dY")

Now for plotting with ggplot2.This first step removes the axes in the resulting plot.

xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

# Let’s build the plot. First we specify the dataframe we need, with a filter excluding flows of <10

ggplot(dest.xy[which(dest.xy$trips>10),], aes(oX, oY))+
  
#  The next line tells ggplot that we wish to plot line segments. The “alpha=" is line transparency and used below

geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, alpha=trips), col="white")+
  
  Here is the magic bit that sets line transparency – essential to make the plot readable

scale_alpha_continuous(range = c(0.03, 0.3))+
  
  Set black background, remove axes and fix aspect ratio

theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()

# Reference
# https://www.r-bloggers.com/mapping-historic-tracks-in-ggplot2/


