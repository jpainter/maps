# world map

# install.packages("maps")
# install.packages("ggplot2")

library(ggplot2)
library(maps)
library(mapproj)
library(maptools)

#standard world plot
  # when using non-stantdard rotation of projection, some odd lines crop up.
  # wrap = True removes some of the odd lines; fill= FALSE (default)
world_map <- map("world", orient = c(90, 0, -90), wrap=TRUE, ylim=c(-60,71))

# Convert to SP
  # get names to use as IDs for polygon objects/ names missing if use wrap
  world_map <- map("world", orient = c(90, 0, -90), ylim=c(-60,71))
  IDs <- sapply(strsplit(world_map$names, ":"), function(x) x[1])
  world_map.sp <- map2SpatialPolygons(world_map, IDs=IDs )

#load map data into ggplot polygon format
world <- map_data("world")
world<-world[order(world$order),]
cities<- map_data("world.cities")

#plot world with ggplot
p <- ggplot()
p <- p + geom_polygon( data=world, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
p

# add cities
cities<-data(world.cities)
p <- p + geom_text( data=cities, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=label), colour="gold2", size=4 )
p
