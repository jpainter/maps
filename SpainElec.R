library(maps)
library(maptools)
library(sp)
library(lattice)
library(latticeExtra)
library(colorspace)
# Let’s start with the data, which is available here (thanks to Emilio Torres, who “massaged” the original dataset, available from here).
# 
# Each region of the map will represent the percentage of votes obtained by the predominant political option. Besides, only four groups will be considered: the two main parties (“PP” and “PSOE”), the abstention results (“ABS”), and the rest of parties (“OTH”).

load('votos2011.rda')
votesData <- votos2011[, 12:1023] ##I don't need all the columns

votesData$ABS <- with(votos2011, Total.censo.electoral - Votos.válidos) ##abstention

Max <- apply(votesData, 1, max)
whichMax <- apply(votesData,  1, function(x)names(votesData)[which.max(x)])

## OTH for everything but PP, PSOE and ABS
whichMax[!(whichMax %in% c('PP',  'PSOE', 'ABS'))] <- 'OTH'

## Finally, I calculate the percentage of votes with the electoral census
pcMax <- Max/votos2011$Total.censo.electoral * 100
# The Spanish administrative boundaries are available as shapefiles at the INE webpage (~70Mb):
  
  espMap <- readShapePoly(fn="mapas_completo_municipal/esp_muni_0109")
Encoding(levels(espMap$NOMBRE)) <- "latin1"

##There are some repeated polygons which can be dissolved with:
gpclibPermit() ##needed for unionSpatialPolygons to work
espPols <- unionSpatialPolygons(espMap, espMap$PROVMUN) ##disolve repeated polygons
The last step before drawing the map is to link the data with the polygons:
  
  idx <- match(levels(espMap$PROVMUN), votos2011$PROVMUN)

##Places without information
idxNA <- which(is.na(idx))

##Information to be added to the SpatialPolygons object
dat2add <- data.frame(prov = votos2011$PROV,
                      poblacion = votos2011$Nombre.de.Municipio,
                      Max = Max,  pcMax = pcMax,  who = whichMax)[idx, ]

row.names(dat2add) <- levels(espMap$PROVMUN)
espMapVotes <- SpatialPolygonsDataFrame(espPols, dat2add)

## Drop those places without information
espMapVotes <- espMapVotes[-idxNA, ]

## ...and drop the Canary islands to get a smaller map. Sorry!
canarias <- espMapVotes$prov %in% c(35,  38)
espMapVotes <- espMapVotes[!canarias, ]
So let’s draw the map. I will produce a list of plots, one for each group. The “+.trellis” method of the latticeExtra package with Reduce superposes the elements of this list and produce a trellis object. I will use a set of sequential palettes from the colorspace package with a different hue for each group.

classes <- levels(factor(whichMax))
nClasses <- length(classes)

pList <- lapply(1:nClasses, function(i){
  mapClass <- espMapVotes[espMapVotes$who==classes[i],]
  step <- 360/nClasses ## distance between hues
  pal <- rev(sequential_hcl(16, h = (30 + step*(i-1))%%360)) ## hues equally spaced
  pClass <- spplot(mapClass['pcMax'], col.regions=pal, lwd=0.1,
                   at = c(0, 20, 40, 60, 80, 100))
})

p <- Reduce('+', pList)
# However, the legend of this trellis object is not valid.
# 
# First, a title for the legend of each element pList will be useful. Unfortunately, the levelplot function (the engine under the spplot method) does not allow for a title with its colorkey argument. The frameGrob and packGrob of the grid package will do the work.

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
# Now, every component of pList includes a legend with a title below. The last step is to modify the legend of the p trellis object in order to merge the legends from every component of pList.

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
# Here is the result with the provinces boundaries superposed (click on the image to get a SVG file)

provinces <- readShapePoly(fn="mapas_completo_municipal/spain_provinces_ag_2")
p + layer(sp.polygons(provinces,  lwd = 0.5))