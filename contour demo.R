# Countour demo from http://xweb.geos.ed.ac.uk/~hcp/r_notes/node3.html

## Set up vectors of x and y  values
x <- seq(-2,2,by=0.05)
y <- seq(-2,2,by=0.1)

## Define a function of x and y
myfn <- function(x,y){exp(-(x^2 +y^2)) + 0.6*exp( -((x+1.8)^2 + y^2)) }

## use outer()  to apply that function to all our x and y values 
z <- outer(x,y,myfn)

#plot
contour(x,y,z)
filled.contour(x,y,z)
filled.contour(x,y,z, lev=seq(0,1,by=0.05) )
filled.contour(x,y,z, filled.contour(x,y,z), color.palette=terrain.colors)

#data
image(x,y,z)
contour(x,y,z, add=TRUE)