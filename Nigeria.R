setwd("C:/Users/bzp3/Desktop/R/Maps")

library(gpclib)
library(maptools)     # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables

# data from http://sedac.ciesin.columbia.edu/povmap/datasets/ds_nat_all.jsp

NGA_adm0.shp <- readShapePoly('C:/Users/bzp3/Desktop/R/Maps/Nigeria/NGA_adm0', proj4string=CRS("+proj=longlat +datum=NAD27"))
NGA_adm1.shp <- readShapePoly('C:/Users/bzp3/Desktop/R/Maps/Nigeria/NGA_adm1', proj4string=CRS("+proj=longlat +datum=NAD27"))
NGA_adm2.shp <- readShapePoly('C:/Users/bzp3/Desktop/R/Maps/Nigeria/NGA_adm2', proj4string=CRS("+proj=longlat +datum=NAD27"))
NGA_water.shp <- readShapeSpatial('C:/Users/bzp3/Desktop/R/Maps/Nigeria/NGA_water_lines_dcw', proj4string=CRS("+proj=longlat +datum=NAD27"))
NGA_roads.shp <- readShapeSpatial('C:/Users/bzp3/Desktop/R/Maps/Nigeria/NGA_roads', proj4string=CRS("+proj=longlat +datum=NAD27"))

NGA.poverty.shp <- readShapePoly('C:/Users/bzp3/Desktop/R/Maps/National_Poverty_Data/NGA_CaseStudy_shapefile/NigeriaCaseStudy',proj4string=CRS("+proj=longlat +datum=NAD27"))
NGA.poverty.db <- read.csv('C:/Users/bzp3/Desktop/R/Maps/National_Poverty_Data/NGA_CaseStudy_Data_Table/NigeriaCaseStudy.csv')

plot(NGA_adm0.shp)
plot(NGA_adm1.shp)
plot(NGA_adm2.shp)
plot(NGA_water.shp)
plot(NGA_roads.shp)
plot(NGA.poverty.shp)

library(ggplot2)
library(mapproj)

# Transform shapefiles for ggplot 
  #inspect file to determine 'region' variable
  head( as.data.frame(NGA_adm1.shp) )
  head( as.data.frame(NGA_adm2.shp) )
  head( as.data.frame(NGA.poverty.shp))
  head( as.data.frame(NGA_water.shp))
  head( as.data.frame(NGA_roads.shp))

  # make state labels for geom_text layer
  nga.states<- data.frame(coordinates(NGA_adm1.shp))
  names(nga.states)<-c("long", "lat")
  nga.states$state <- as.data.frame(NGA_adm1.shp)$NAME_1

  # run  gpclibPermit() function to set to TRUE (don't know why this is done, but fortify fails without)
  gpclibPermit()
  # extract mapping data with fortify function
  NGA_adm1<-fortify(NGA_adm1.shp, region="NAME_1") 
  NGA_adm2<-fortify(NGA_adm2.shp, region="NAME_2")
#   nga.shape <- fortify(NGA.poverty.shp, region="ADM2NAME")
  NGA.water <- fortify(NGA_water.shp, region="F_CODE_DES")
  NGA.roads <- fortify(NGA_roads.shp, region="F_CODE_DES")

  #sort
  NGA_adm1<-NGA_adm1[order(NGA_adm1$id), ] 
  NGA_adm2<-NGA_adm2[order(NGA_adm2$id), ]
#   nga.shape<-nga.shape[order(nga.shape$id), ]
  NGA.poverty<- NGA.poverty[order(NGA.poverty$ADM2NAME), ]
  NGA.water<- NGA.water[order(NGA.water$id), ]
  NGA.roads<- NGA.roads[order(NGA.roads$id), ]

  #combine db data
  nga.poverty <- merge(NGA.poverty.db,NGA_adm2, by.x="ADM2NAME", by.y="id")


#This step removes the axes labels etc when called in the plot.
  xquiet<- scale_x_continuous("", breaks=NA)
  yquiet<-scale_y_continuous("", breaks=NA)
  quiet<-list(xquiet, yquiet)

#data elements
  state <- geom_polygon(data=NGA_adm1, aes(long, lat, group=group),colour="black", fill="#FFF7BC", lwd=.1 )
  state.text <- geom_text( data=nga.states, aes(x=long, y=lat, label=state), colour="black")
  district <- geom_path(data=NGA_adm2, aes(long, lat, group=group),colour="grey", fill=NA )
  water <- geom_path( data=NGA.water, aes(x=long, y=lat, group = group), colour="darkblue", lwd=.1)
  roads <- geom_path( data=NGA.roads, aes(x=long, y=lat, group = group), colour="green", lwd=.2, alpha=0.5)

#data layers
  vita <- geom_polygon( data=nga.poverty, aes(x=long, y=lat, group = group, fill=VITA), colour="grey", lwd=0.2)
  iod<-c( geom_polygon(data=nga.poverty, aes(x=long, y=lat, group = group, fill=IODINE), colour="grey"), lwd=0.2 )

#create plot
nga<- ggplot(nga.poverty) + 
        opts(panel.background=theme_rect(fill="#43A2CA")) +    
#       water +
#       roads +
      state + 
#       district +
      vita +
#       state.text +
      scale_colour_gradient(low="#F1A340", high="#998EC3", space="rgb") 
#       quiet 
nga