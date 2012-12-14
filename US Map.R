# US MAP

install.packages("maps")
install.packages("ggplot2")

library(ggplot2)
library(maps)
#load us map data
all_states <- map_data("state")
#plot all states with ggplot
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
p

# selected states
states <- subset(all_states, region %in% c( "illinois", "indiana", "iowa", "kentucky", "michigan", "minnesota","missouri", "north dakota", "ohio", "south dakota", "wisconsin" ) )

# add data
mydata <- read.csv("midwest universities.csv", header=TRUE, row.names=1, sep=",")

p <- p + geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="white" )
p <- p + geom_point( data=mydata, aes(x=long, y=lat, size = enrollment), color="coral1") + scale_size(name="Total enrollment")
p <- p + geom_text( data=mydata, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=label), colour="gold2", size=4 )
p