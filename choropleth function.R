# Function creates svg choropleth map from a base svg map
# returns list with 1) filename of map, 2) cutpoints and 3) colors


choropleth <- function (data, # must supply a field with FIPS
                        variable, # rate or count variable
                        numcolors = 5, # number of color levels
                        map = "CensusAtlasStateMapTweaked.svg" # base svg map
                          ) {
  library(XML)
  mapXML = xmlTreeParse(map)
  
  # Set the style tag to fill in,
  # leaving just fill color to be added at end
  styleprefix =
    "fill-opacity:1;fill-rule:nonzero;stroke:none;fill:"
  
  # Choose color palette; but reverse the order
  library(RColorBrewer)
  mycolors = brewer.pal(numcolors,"BrBG")[numcolors:1]
  
  # Divide povrates into groups using the Jenks approach
  library(classInt)
  cutpoints = classIntervals(data[,variable], numcolors, "jenks")$brks
  data$color = mycolors[cut(data[, variable], cutpoints,
                               labels=FALSE, include.lowest=TRUE)]
  
  # Iterate through the XML and redefine styles based on id
  npaths = length(mapXML$doc$children$svg$children$g$children)
  for(i in 1:npaths){
    id = mapXML$doc$children$svg$children$g$children[i]$path$attributes["id"]
    if(id != "State_Lines"){
      whichrow = which(data$FIPS==as.numeric(id))
      mapXML$doc$children$svg$children$g$children[i]$path$attributes["style"] =
        paste0(styleprefix, data$color[whichrow])
    }
  }
  
  # Save results
  filename = paste(variable,".svg", sep='')
  saveXML(mapXML$doc$children$svg, filename)
  return( list(filename, cutpoints, mycolors) )
}

# test
# choropleth(data = povdata, variable= "Povrate", numcolors=10)