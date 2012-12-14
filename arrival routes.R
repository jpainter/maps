#great circle lines

library(maps)
library(geosphere)
library(RODBC)

# arrivals
  edn.geocode.db<-odbcConnectAccess2007("//cdc/project/ccid_ncpdcid_dgmq/IRMH/_Epi Team/PanelDB/EDN Geocode.accdb")
  arrivals <- sqlQuery(edn.geocode.db, "select Latitude, Longitude, State from Table1 where Longitude is not Null")
  arrivals.test <- arrivals[1:10,]
  states <- data.frame(table(arrivals$State))
  maxcnt <- max(states$Freq)

# Color
  pal <- colorRampPalette(c("#191919", "white", "blue"))
  colors <- pal(50)

# Map
  xlim <- c(-180, 180)
  ylim <- c(-60, 90)
  orient <- c(90, 0, -90)
  map<- map("world", proj = "mercator", col="black", bg="light grey", lwd=0.05, xlim=xlim, ylim=ylim, orient = orient, wrap=TRUE)
#             projection = "mollweide", parameters= c(0,0))

# draw each line
    # test line
      air1 <- c(100, 13)
      air2 <- arrivals[22,]
      route <- gcIntermediate( c(air1[1], air1[2]) , c(air2$Longitude, air2$Latitude), n=100, addStartEnd=TRUE)
      # adjust route to stay below 70th latitude so that it draws better
      lowerRoute <- route
      i <- 1:(length(route)/2)
      midi <- length(route)/4
      lowerRoute[,2] <- (1- abs(midi - i)/midi)
      map("world", bg="grey", fill=TRUE, col="white")
      lines(lowerRoute, col="black", lwd=2)

     for (j in 1:100) {
        air1 <- c(100, 13)
        air2 <- arrivals[j,]
      
        route <- gcIntermediate( c(air1[1], air1[2]) , c(air2$Longitude, air2$Latitude), n=100, addStartEnd=TRUE)
        
#         colindex <- round( ( subset(states, Var1==arrivals[j,]$State)$Freq  / maxcnt) * length(colors) )
#         lines(route, col=colors[colindex], lwd=0.2)
        
        lines(route, col="black", lwd=0.2)
        }