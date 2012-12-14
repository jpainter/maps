# R code for simple map of poverty rates by state.
#
# Jerzy Wieczorek, June 13, 2012
# @civilstat
# http://civilstat.com

# Read poverty dataset
povdata <- read.csv("poverty/statepovdata.csv")
names(povdata) <- c("State.Abb","State.FIPS","Povrate","CI.lo","CI.hi")

# Load libraries and state dataset
library(maps)
library(mapproj)
library(RColorBrewer)
library(plotrix)
data(state, package = "datasets")

# Run code to add extra states to map
#source("Add.AK.DC.HI.r")
AddExitButton <- FALSE
source("poverty/Add.AK.HI.DC.r")

# Since we have data for DC,
# add it to default list of state names,
# since that'll make the mapping easier
state.name.aug <- c(state.name,"District of Columbia")
state.abb.aug <- c(state.abb,"DC")

# Figure out which areas on map correspond to which states
state.to.map <- match.map(USAmap, state.name.aug)

# Use nicer colors with RColorBrewer package
nrcols <- 5
mycolpal <- brewer.pal(nrcols,"Blues")
#col.bg <- brewer.pal(3,"Greys")[1]
col.bg <- "grey90"
col.text <- "black"


# Assign a color class to each state
# Need to account for match.map!
options(stringsAsFactors = FALSE)
plotcolstemp <- data.frame(abb=povdata[,1],
                           col=mycolpal[cut(povdata[,3],nrcols,labels=F)])
stateabbs <- data.frame(abb=state.abb.aug[state.to.map])
#plotcols <- merge(stateabbs,plotcolstemp,sort=F)[,2]
# Also need to make sure order of states stays the same.
stateabbs$myorder <- seq_len(nrow(stateabbs))
plotcols <- merge(stateabbs,plotcolstemp,sort=F)
plotcols <- plotcols[order(plotcols$myorder),3]



# Set xpd=TRUE to allow writing/plotting outside margins
par(xpd=TRUE)

# Plot the map itself
map(USAmap,col=plotcols,fill=T,bg=col.bg, resolution=0, proj="polyconic")

# Annotate with title and source info
title(main="US states: % in poverty (all ages)", col.main=col.text)
title(sub="Source: U.S. Census Bureau,",line=2.5,col.sub=col.text,font.sub=2)
title(sub="Small Area Income & Poverty Estimates, 2009",line=3.5,col.sub=col.text,font.sub=2)

# Nicer legend using plotrix package
color.legend(-.2,.35,.2,.4,legend=c("8.6%","21.8%"),align="rb",rect.col=mycolpal,col=col.text)
