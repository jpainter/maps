# simple map demo from http://xweb.geos.ed.ac.uk/~hcp/r_notes/node3.html

library(maps)
library(mapproj)

map()
map(proj="mercator",xlim=c(-178,178),ylim=c(-70,70))
map(proj="aitoff",xlim=c(-178,178))
map(proj="mollweide",xlim=c(-175,175), ylim=c(-60,80), fill=TRUE)
map(proj="azequalarea",xlim=c(-178,178))
map.grid()
map(proj="orthographic",orientation=c(55,0,0))
map.grid()

#     To get rid of the white space around the map, you may want to do
       par(mai=c(0,0,0,0))
      # before making the plots: this sets all the margins to 0 inches. To add data to your map, 

# you need to convert its latitude and longitude to the projected map co-ordinates:
      longs<-c(0, -3.1, 2.4, -3, -77)
      lats<-c(51.5, 55.9, 48.8, 17, 38.8)
      labs<-c("London","Edinburgh","Paris","Timbuktu","Washington")
      points(mapproject(longs,lats),col="red",pch=19)
      text(mapproject(longs,lats),labs,col="red",adj=-0.1)

# Note that mapproject converts your lat and long to the correct x and y co-ordinates for 
# the last map you drew. Also, don't forget that the on-line documentation works for add-on 
# packages like those used for mapping, so you can find out more by typing
    
help(map)