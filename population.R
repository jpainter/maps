# Population density map from R blogger
# http://procomun.wordpress.com/2012/02/20/maps_with_r_2/

setwd('C:/Users/bzp3/Desktop/R/Maps')

library(maps)
library(maptools)
library(sp)
library(lattice)
library(latticeExtra)
library(colorspace)
library(raster)   #also need to have package 'rgdal' installed
library(rasterVis)
library(colorspace)
# Now, I define the geographical extent to be analyzed (approximately India and China).

ext <- extent(65, 100, 5, 40)
# The first raster file is the population density in our planet, 
# available at this NEO-NASA webpage (choose the Geo-TIFF floating option, ~25Mb). 
# After reading the data with raster I subset the geographical extent and replace the 99999 with NA.

pop <- raster('NASA/875430rgb-167772161.0.TIFF')
pop <- crop(pop, ext)
pop[pop==99999] <- NA
pTotal <- levelplot(pop, zscaleLog=10, par.settings=BTCTheme)
pTotal

landClass <- raster('NASA/241243rgb-167772161.0.TIFF')
landClass <- crop(landClass, ext)
# The codes of the classification are described here. 
# In summary, the sea is labeled with 0, forests with 1 to 5, 
# shrublands, grasslands and wetlands with 6 to 11, agriculture and urban lands with 12 to 14, 
# and snow and barren with 15 and 16. 
# This four groups (sea is replaced NA) will be the levels of the factor. 
# (I am not sure if these sets of different land covers is sensible: comments from experts are welcome!)

landClass[landClass %in% c(0, 254)] <- NA
landClass <- cut(landClass, c(0, 5, 11, 14, 16))
classes <- c('FOR', 'LAND', 'URB', 'SNOW')
nClasses <- length(classes)

# Display
rng <- c(minValue(landClass), maxValue(landClass))
## define the breaks of the color key
my.at <- seq(rng[1]-1, rng[2])
## the labels will be placed vertically centered
my.labs.at <- seq(rng[1], rng[2])-0.5

levelplot(landClass, at=my.at, margin=FALSE, col.regions=terrain_hcl(4),
          colorkey=list(labels=list(
            labels=classes, ## classes names as labels
            at=my.labs.at)))

# This histogram shows the distribution of the population density in each land class.
s <- stack(pop, landClass)
layerNames(s) <- c('pop', 'landClass')
histogram(~log10(pop)|landClass, data=s,
          scales=list(relation='free'),
          strip=strip.custom(strip.levels=TRUE))

# create a list of trellis objects with four elements (one for each level of the factor). 
# Each of these objects is the representation of the population density in a particular land class. 
# I use the same scale for all of them to allow for comparisons 
# (the at argument of levelplot receives the correspondent at values from the global map)

at <- pTotal$legend$bottom$args$key$at

pList <- lapply(1:nClasses, function(i){
  landSub <- landClass
  landSub[!(landClass==i)] <- NA
  popSub <- mask(pop, landSub)
  step <- 360/nClasses
  pal <- rev(sequential_hcl(16, h = (30 + step*(i-1))%%360))
  pClass <- levelplot(popSub, zscaleLog=10, at=at,
                      col.regions=pal, margin=FALSE)
})

# The “+.trellis” method of the latticeExtra package with Reduce superposes the elements of this list 
# and produce a trellis object. I will use a set of sequential palettes from the colorspace package 
# with a different hue for each group.

p <- Reduce('+', pList)

# However, the legend of this trellis object is not valid.
# 
# First, a title for the legend of each element pList will be useful. 
# Unfortunately, the levelplot function (the engine under the spplot method) 
# does not allow for a title with its colorkey argument. 
# The frameGrob and packGrob of the grid package will do the work.

addTitle <- function(legend, title){
  titleGrob <- textGrob(title, gp=gpar(fontsize=8), hjust=1, vjust=1)
  legendGrob <- eval(as.call(c(as.symbol(legend$fun), legend$args)))
  ly <- grid.layout(ncol=1, nrow=2, widths=unit(0.9, 'grobwidth', data=legendGrob))
  fg <- frameGrob(ly, name=paste('legendTitle', title, sep='_'))
  pg <- packGrob(fg, titleGrob, row=2)
  pg <- packGrob(pg, legendGrob, row=1)
}

for (i in seq_along(classes)){
  lg <- pList[[i]]$legend$right
  lg$args$key$labels$cex=ifelse(i==nClasses, 0.8, 0) ##only the last legend needs labels
  pList[[i]]$legend$right <- list(fun='addTitle',
                                  args=list(legend=lg, title=classes[i]))
}

# Now, every component of pList includes a legend with a title below. The last step is to 
# modify the legend of the p trellis object in order to merge the legends from every component of pList.

## list of legends
legendList <- lapply(pList, function(x){
  lg <- x$legend$right
  clKey <- eval(as.call(c(as.symbol(lg$fun), lg$args)))
  clKey
})

##function to pack the list of legends in a unique legend
##adapted from latticeExtra::: mergedTrellisLegendGrob
packLegend <- function(legendList){
  N <- length(legendList)
  ly <- grid.layout(nrow = 1,  ncol = N)
  g <- frameGrob(layout = ly, name = "mergedLegend")
  for (i in 1:N) g <- packGrob(g, legendList[[i]], col = i)
  g
}

## The legend of p will include all the legends
p$legend$right <- list(fun = 'packLegend',  args = list(legendList = legendList))

# final plot
provinces <- readShapePoly('C:/Users/bzp3/Desktop/R/Maps/India/IND_adm/IND_adm1', proj4string=CRS("+proj=longlat +datum=NAD27"))
p + layer(sp.polygons(provinces,  lwd = 0.5))