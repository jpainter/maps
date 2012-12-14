setwd("C:/Users/bzp3/Desktop/R/Maps")

library(gpclib)
library(maptools)     # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables

IND_adm0.shp <- readShapePoly('C:/Users/bzp3/Desktop/R/Maps/India/IND_Adm/IND_adm0', proj4string=CRS("+proj=longlat +datum=NAD27"))
IND_adm1.shp <- readShapePoly('C:/Users/bzp3/Desktop/R/Maps/India/IND_Adm/IND_adm1', proj4string=CRS("+proj=longlat +datum=NAD27"))

plot(IND_adm0.shp)
plot(IND_adm1.shp)

library(ggplot2)
library(mapproj)

# Transform shapefiles for ggplot 
#inspect file to determine 'region' variable
head( as.data.frame(IND_adm1.shp) )

# make state labels for geom_text layer
ind.states<- data.frame(coordinates(IND_adm1.shp))
names(ind.states)<-c("long", "lat")
ind.states$state <- as.data.frame(IND_adm1.shp)$NAME_1

# run  gpclibPermit() function to set to TRUE (don't know why this is done, but fortify fails without)
gpclibPermit()
# extract mapping data with fortify function
IND_adm1<-fortify(IND_adm1.shp, region="NAME_1") 

#sort
IND_adm1<-IND_adm1[order(IND_adm1$id), ] 
IND.pop<- IND.poverty[order(IND.poverty$ADM2NAME), ]

#combine db data
ind.pop <- merge(IND.pop.db,IND_adm1, by.x="ADM1NAME", by.y="id")

#This step removes the axes labels etc when called in the plot.
xquiet<- scale_x_continuous("", breaks=NA)
yquiet<-scale_y_continuous("", breaks=NA)
quiet<-list(xquiet, yquiet)

#data elements
state <- geom_polygon(data=IND_adm1, aes(long, lat, group=group),colour="black", fill="#FFF7BC", lwd=.1 )
state.text <- geom_text( data=ind.states, aes(x=long, y=lat, label=state), colour="black")

#create plot
india<- ggplot(IND_adm1) + 
  opts(panel.background=theme_rect(fill="#43A2CA")) +    
  state + 
  state.text +
  scale_colour_gradient(low="#F1A340", high="#998EC3", space="rgb") 
#       quiet 
india