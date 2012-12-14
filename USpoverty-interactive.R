# R code for interactive map of poverty rates by state.
# Click a state to recolor the map,
# showing which states' poverty rates are 
# significantly higher or lower than the selected state's rate.
# Click the stop sign to exit the loop.
#
# Jerzy Wieczorek, June 14, 2012
# @civilstat
# http://civilstat.com


# Create map that contains AK, HI, DC, and an exit button
AddExitButton <- TRUE
source("poverty/Add.AK.HI.DC.r")
nrareas <- length(USAmap$names)

# Load 2009 SAIPE data from U.S. Census Bureau:
#   http://www.census.gov/did/www/saipe/
# 1st column is state abbrevs sorted by povrate
# Dataset has all 50 states and DC
povdata <- read.csv("poverty/statepovdata.csv",stringsAsFactors=F)
names(povdata) <- c("State.Abb","State.FIPS","Povrate","CI.lo","CI.hi")

# Load libraries
library(RColorBrewer)
library(maps)
library(mapproj)
library(plotrix)
data(state, package = "datasets")

# Since we have data for DC,
# add it to default list of state names;
# also add the "exit" button to list of polygons;
# that'll make the mapping easier
state.name.aug <- c(state.name,"District of Columbia","Exit")
state.abb.aug <- c(state.abb,"DC","Exit")
state.to.map <- match.map(USAmap, state.name.aug)

# Set the colors
col.bg <- "grey90"
col.nodiff <- brewer.pal(3,"Greys")[2]
col.higher <- brewer.pal(9,"PuOr")[8]
col.lower <- brewer.pal( 9,"PRGn")[8]
col.selected <- brewer.pal(4,"PuOr")[1]
col.border <- "black"
col.text <- "black"
col.boxfill <- brewer.pal(4,"PRGn")[4]
col.exit <- "red"

# Allow us to plot title text etc
# outside the main plotting area
par(xpd=TRUE)

# Start with map relative to nat'l avg as baseline
US.Povrate <- 14.3
US.CI.lo <- 14.3
US.CI.hi <- 14.4

# Define the map-drawing as a function,
# so can just re-run that one line in a function call
# instead of the whole function
maploop <- function(){
  # Code similar to loop below; see comments there
  mycols <- rep(col.nodiff,nrareas)
  lower <- which(povdata[,"CI.hi"] < US.CI.lo)
  higher <- which(povdata[,"CI.lo"] > US.CI.hi)
  mycols[which(state.to.map %in% which(state.abb.aug %in% povdata[lower,1]))]=col.lower
  mycols[which(state.to.map %in% which(state.abb.aug %in% povdata[higher,1]))]=col.higher
  mycols[nrareas] <- col.exit
  mymap <- map(USAmap,fill=T,col=mycols, bg=col.bg,resolution=0, projection="polyconic")
  title( main=paste("US national average: ",US.Povrate,
                    "% in poverty (all ages)",sep=""), col.main=col.text)
  title(sub="Source: U.S. Census Bureau,\n",line=3,col.sub=col.text,font.sub=2)
  title(sub="Small Area Income & Poverty Estimates, 2009",line=3,col.sub=col.text,font.sub=2)
  color.legend(-.3,.35,.3,.4,legend=c(paste("lower:\n",length(lower)," states",sep=""),
                                      "not\ndifferent",paste("higher:\n",length(higher)," states",sep="")),align="rb",
               rect.col=c(rep(col.lower,length(lower)),rep(col.nodiff,51-length(lower)-length(higher)),rep(col.higher,length(higher))),col=col.text)
  
  #####
  # Re-color to show lower or higher povrate estimates,
  # now accounting for significance of the difference!
  # 
  # Entering a "while" loop;
  # click the red STOP-sign area on map to stop
  whichstate.map <- 0
  # We made the Exit button the last area,
  # so if identify() returns nrareas, it is time to exit.
  exit.area <- nrareas
  while(whichstate.map!=exit.area){
    # Identify the selected state
    whichstate.map <- identify(mymap,index=T,plot=F)
    if(whichstate.map!=nrareas){
      whichstate.table <- which(povdata[,1]==state.abb.aug[state.to.map[whichstate.map]])
      mycols<-rep(col.nodiff,nrareas)
      # Check if low point est's CI.hi is lower than selection's CI.lo
      lower <- which(povdata[,"CI.hi"] < povdata[whichstate.table,"CI.lo"])
      # Similarly for high points
      higher <- which(povdata[,"CI.lo"] > povdata[whichstate.table,"CI.hi"])
      # Set colors
      mycols[which(state.to.map %in% which(state.abb.aug %in% povdata[lower,1]))]=col.lower
      mycols[which(state.to.map %in% which(state.abb.aug %in% povdata[higher,1]))]=col.higher
      # Recolor all polygons for the selected state, not just the clicked polygon
      # (i.e. both halves of Michigan)
      mycols[which(state.to.map==state.to.map[whichstate.map])] <- col.selected
      mycols[nrareas] <- col.exit
      # Draw the map
      map(USAmap,col=mycols,fill=T,bg=col.bg,resolution=0, projection="polyconic")
      # Create the titles/subtitles/legend
      title( main=paste(state.name.aug[state.to.map[whichstate.map]],": ",povdata[whichstate.table,3],
                        "% in poverty (all ages)",sep=""), col.main=col.text)
      title(sub="Source: U.S. Census Bureau,\n",line=3,col.sub=col.text,font.sub=2)
      title(sub="Small Area Income & Poverty Estimates, 2009",line=3,col.sub=col.text,font.sub=2)
      color.legend(-.3,.35,.3,.4,legend=c(paste("lower:\n",length(lower)," states",sep=""),
                                          "not\ndifferent",paste("higher:\n",length(higher)," states",sep="")),align="rb",
                   rect.col=c(rep(col.lower,length(lower)),rep(col.nodiff,51-length(lower)-length(higher)),rep(col.higher,length(higher))),col=col.text)
    }
  }
}
#####

# Run the function defined above
maploop()
