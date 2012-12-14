# SVG Animation example
# http://procomun.wordpress.com/2011/11/12/animation-with-gridsvg/

# examle of pdf with multiple plots
trellis.device(pdf, file='iris.pdf')
p <- xyplot(Sepal.Length~Petal.Length|Species, data=iris, layout=c(1, 1))
print(p)
dev.off()

# This code creates svg file for animated sequence of slides

library(gridSVG)
library(XML)

grid.grabExpr <- function(expr, warn=2, wrap=FALSE, ... ) {
  # Start an "offline" PDF device for this function
  # .Call("R_GD_nullDevice", PACKAGE = "grDevices")
  pdf(file=NULL)
  on.exit(dev.off())
  # Run the graphics code in expr
  # Rely on lazy evaluation for correct "timing"
  eval(expr)
  # Grab the DL on the new device
  grid:::grabDL(warn, wrap, ...)
}

animateTrellis <- function(object, file='animatedSVG.svg',
                           duration=.1, step=2, show=TRUE){
  nLayers <- dim(object)
  stopifnot(nLayers>1)
  for (i in seq_len(nLayers)){
    p <- object[i]
    label <- p$condlevels[[1]][i]
    ##Create intermediate SVG files
    g <- grid.grabExpr(print(p, prefix=label),
                       name=label)
    if (i==1){ ## First frame
      ga <- animateGrob(g, group=TRUE,
                        visibility="hidden",
                        duration=duration, begin=step)
    } else if (i==nLayers){ ##Last Frame
      gg <- garnishGrob(g, visibility='hidden')
      ga <- animateGrob(gg, group=TRUE,
                        visibility="visible",
                        duration=duration, begin=step*(i-1))
    } else { ##any frame
      gg <- garnishGrob(g, visibility='hidden')
      gaV <- animateGrob(gg, group=TRUE,
                         visibility="visible",
                         duration=duration, begin=step*(i-1))
      ga <- animateGrob(gaV, group=TRUE,
                        visibility="hidden",
                        duration=duration, begin=step*i)
    }
    grid.newpage()
    grid.draw(ga)
    fich <- tempfile(fileext='.svg')
    gridToSVG(fich)
    
    ## Combine all
    if (i==1) {
      svgTop <- xmlParse(fich)
      nodeTop <- getNodeSet(svgTop,
                            "//svg:g[@id='gridSVG']",
                            c(svg="http://www.w3.org/2000/svg"))[[1]]
    } else {
      svgChildren <- xmlParse(fich)
      node <- getNodeSet(svgChildren,
                         "//svg:g[@id='gridSVG']/*",
                         c(svg="http://www.w3.org/2000/svg"))
      addChildren(nodeTop, node)
    }
    unlink(fich)
  }
  saveXML(svgTop, file=file)
  dev.off()
  if (show) browseURL(file)
  invisible(svgTop)
}

animateTrellis(p, file='C:/Users/bzp3/Desktop/R/Maps/iris.svg')


