# houston crime data example from 
# https://github.com/hadley/ggplot2/wiki/Crime-in-Downtown-Houston,-Texas-:-Combining-ggplot2-and-Google-Maps

library(ggplot2)
library(ReadImages)
library(RgoogleMaps)
library(MASS)
theme_set(theme_bw())


################################################################################
#################### preload functions for later use        ####################
################################################################################

ggimage <- function(image){
  require(ggplot2)
  
  if(length(dim(image)) == 2){
    message('creating black and white image...')
    image <- melt(image)
    names(image) <- c('row','column','fill')
    plot <- qplot(column, -row, data = image, geom = 'tile', fill = fill) +
      scale_fill_gradient(low = 'black', high = 'white')
  }
  
  if(length(dim(image)) == 3){
    message('creating color image...')
    image <- apply(image, 1:2, function(v) rgb(v[1], v[2], v[3]))
    image <- melt(image)
    names(image) <- c('row', 'column', 'fill')
    plot <- qplot(column, -row, data = image, geom = 'tile', fill = fill) +
      scale_fill_identity()  	
  }
  
  #return(plot) # remove first pound for the image in the case study
  plot +
    opts(      
      axis.line = theme_blank(), axis.ticks = theme_blank(),
      axis.text.x = theme_blank(), axis.text.y = theme_blank(), 
      axis.title.x = theme_blank(), axis.title.y = theme_blank(),
      axis.ticks.length = unit(0, "lines"), 
      axis.ticks.margin = unit(0, "lines"),
      legend.position = "none", 
      panel.background = theme_blank(), 
      panel.border = theme_blank(), 
      panel.grid.major = theme_blank(), 
      panel.grid.minor = theme_blank(), 
      panel.margin = unit(0, "lines"), 
      plot.background = theme_blank(), 
      plot.title = theme_blank(), 
      plot.margin = unit(c(-1, -1, -1.5, -1.5), "lines")
      )
}


ggooglemap <- function(location = 'houston', 
                       center = c(lat = 29.7632836, lon = -95.3632715), API, 
                       type = c('color','bw')[1], rgbcoefs = c(0, 1, 0), zoom = 10, 
                       maptype = 'terrain',
                       destfile = 'TemporaryMap.jpg', n_pix = 640)
{
  require(ggplot2)	
  require(RgoogleMaps)
  require(ReadImages)	
  
  if(!missing(location)){
    url_string <- paste('http://maps.google.com/maps/geo?q=', location, sep = '')
    site   <- readLines(url(url_string))	
    site   <- site[which(regexpr('coordinates', site) > 0)]
    if(is.na(site)) stop('location geocoding error.')
    site   <- strsplit(site, ': [', fixed=TRUE)[[1]][2]
    site   <- strsplit(site, ',')[[1]][1:2]
    latlon <- as.numeric(site)	
    center <- c(lat = latlon[2], lon = latlon[1])
    closeAllConnections()
  }
 
# ENTER YOUR API HERE
#   no longer neede--see GetMap documentation
#   if(missing(API)) API <- 'ABQIAAAAb8a78qCzHMmAO9kLtXEoIhSecF-ZV83mgi428MfuqUXlmp5t3xRfmRiHBiVIXj5vq-XpEg-SF4JGhg' 

  
  # get map
  GetMap(center = center[c('lat','lon')], 
         size = c(n_pix, n_pix), zoom = zoom, format = 'jpg', 
         maptype = maptype, destfile = destfile)
  
  # load map  
  map <- read.jpeg(destfile)
  
  # deal with color
  if(type == 'color'){
    map <- apply(map, 1:2, function(v) rgb(v[1], v[2], v[3]))     
  } else if(type == 'bw') {
    nrow <- nrow(map)
    ncol <- ncol(map)  	
    map <- grey(rgb2grey(map, coefs = rgbcoefs))
    map <- matrix(map, nrow = nrow, ncol = ncol)
  } else {
    stop('type must be either 50aab105c5a133889b1aeb4f8dc967bc28c77661#39;color\' or 50aab105c5a133889b1aeb4f8dc967bc28c77661#39;bw\'', call. = FALSE)
  }
  
  # reshape map for plotting
  m_map <- melt(map)
  names(m_map) <- c('x','y','fill')
  m_map <- within(m_map,{
    x <- x - n_pix/2 - 1
    y <- y - n_pix/2 - 1
  })     
  
  mapInfo <- list(lat = center['lat'], lon = center['lon'], zoom = zoom, map)
  XY_cent <- LatLon2XY.centered(mapInfo, center['lat'], center['lon'])
  #XY2LatLon(HouMapInfo, XY_cent$newX, XY_cent$newY)  
  
  # geocode pixel references
  s <- (-n_pix/2) : (n_pix/2 - 1)  
  lat_wrapper <- function(x) XY2LatLon(mapInfo, -n_pix/2, x)[1]
  lats <- apply(data.frame(s), 1, lat_wrapper)  
  lon_wrapper <- function(y) XY2LatLon(mapInfo, y, -n_pix/2)[2]
  lons <- apply(data.frame(s), 1, lon_wrapper)
  
  # merge colors to latlons and return
  df_xy   <- expand.grid(x = s, y = s)
  df_ll   <- expand.grid(lat = rev(lats), lon = lons)
  df_xyll <- data.frame(df_xy, df_ll)
  df <- suppressMessages(join(df_xyll, m_map, type = 'right'))
  df <- df[,c('lon','lat','fill')]
  df
} # end google map function




theme_nothing <- function (base_size = 12){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_blank(), axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.title.x = theme_blank(), axis.title.y = theme_blank(), 
    axis.ticks.length = unit(0, "lines"), axis.ticks.margin = unit(0, "lines"), 
    legend.position = "none", 
    panel.background = theme_rect(fill = 'white'), 
    panel.border = theme_blank(), 
    panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), 
    panel.margin = unit(0, "lines"), 
    plot.background = theme_rect(colour = 'white'), 
    plot.title = theme_text(size = base_size * 1.2), 
    plot.margin = unit(c(-1, -1, -1.5, -1.5), "lines")
    ), class = "options")
}




vplayout <- function(x, y)  viewport(layout.pos.row = x, layout.pos.col = y) 

################################################################################
#################### hadley's picture                       ####################
################################################################################

hadley <- read.jpeg('hadley.jpg')

# for the axes, remove the pound sign in the ggimage function above
ggimage(hadley) + coord_equal()


################################################################################
#################### making a map                           ####################
################################################################################

CityHall_latlon <- c(lon = -95.369318, lat = 29.760210) 

# Unse n_pix = 640 (default) to limit size
DowntownMap <-ggooglemap(center = CityHall_latlon, zoom = 6, n_pix = 22)

# Takes forever...convert fill to character from factor to run a slice
# as a factor, requires time to load all data even when truncating dataset (jp)

# houston<- qplot(lon, lat, data = DowntownMap, geom = 'tile', fill = fill) +
#   scale_fill_identity() +
#   scale_x_continuous('Longitude') + 
#   scale_y_continuous('Latitude') +  
#   coord_equal() +
#   opts(title = 'Terrain Map of Downtown Houston')

ggplot(data = DowntownMap, aes(x=lon, y=lat)) +
      geom_tile(aes(fill=fill)) +
      scale_fill_identity() +
      scale_x_continuous('Longitude') + 
      scale_y_continuous('Latitude') +  
      coord_equal() +
      opts(title = 'Terrain Map of Downtown Houston')


################################################################################
#################### houston crime                          ####################
################################################################################

load('HoustonCrime.Rdata')  # variable name : crime_df

# restrict to violent crimes
violent_crimes <- subset(crime_df, 
                         offense != 'Auto Theft' & offense != 'Theft' & offense != 'Burglary'
                         )

# restrict to year 2010 (january - august)
violent_crimes <- subset(violent_crimes, 
                         time >= ISOdatetime(2010, 1, 1, 0, 0, 0)
                         )

# grab downtown google map
CityHall_latlon <- c(lon = -95.369318, lat = 29.760210) 
DowntownMap <-ggooglemap(center = CityHall_latlon, zoom = 14)
lat_range <- range(DowntownMap$lat)
lon_range <- range(DowntownMap$lon)

# contour plot
ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = fill), data = DowntownMap) +
  scale_fill_identity() +
  geom_density2d(aes(x = lon, y = lat, colour = ..level..), 
                 bins = I(10), fill = NA, alpha = I(1/2), size = I(.75), data = violent_crimes) +
                   scale_colour_gradient2('Violent\nCrime\nDensity', 
                                          low = 'darkblue', mid = 'orange', high = 'red', midpoint = 900) + 
                                            scale_x_continuous('Longitude', limits = lon_range) + 
                                            scale_y_continuous('Latitude', limits = lat_range) +
                                            opts(title = 'Violent Crime Contour Map of Downtown Houston, 2010') +
                                            coord_equal()   

# point plot
violent_crimes <- subset(violent_crimes,
                         lon_range[1] <= lon & lon <= lon_range[2] &
                           lat_range[1] <= lat & lat <= lat_range[2] 
                         )  # cuts out the warning for missing values in geom_point

ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = fill), data = DowntownMap) +
  scale_fill_identity() +
  geom_jitter(aes(x = lon, y = lat, colour = offense, size = offense), 
              fill = NA, alpha = I(3/4), data = violent_crimes,
              position = position_jitter(width = .001, height = .001)) +
                scale_x_continuous('Longitude', limits = lon_range) + 
                scale_y_continuous('Latitude', limits = lat_range) +
                scale_colour_discrete('') +
                scale_size_manual('', values = c(
                  'Robbery' = 2, 'Aggravated Assault' = 2.5, 'Rape' = 3, 'Murder' = 4
                  )) +
                    opts(title = 'Violent Crime Map of Downtown Houston, 2010') +
                    coord_equal()   



# weather map
load('HoustonCrime.Rdata')  # variable name : crime_df

violent_crimes <- subset(crime_df, 
                         offense != 'Auto Theft' & offense != 'Theft' & offense != 'Burglary'
                         )

violent_crimes <- subset(violent_crimes, 
                         time >= ISOdatetime(2010, 1, 1, 0, 0, 0)
                         ) 

CityHall_latlon <- c(lon = -95.369318, lat = 29.760210) 
DowntownMap <-ggooglemap(center = CityHall_latlon, zoom = 14, maptype = 'hybrid')
lat_range <- range(DowntownMap$lat)
lon_range <- range(DowntownMap$lon)

vclatlon <- violent_crimes[,c('lon','lat')]
vclatlon <- na.omit(violent_crimes[,c('lon','lat')])
vclatlon <- subset(vclatlon,
                   lon_range[1] <= lon & lon <= lon_range[2] &
                     lat_range[1] <= lat & lat <= lat_range[2] 
                   )
den <- kde2d(vclatlon$lon, vclatlon$lat, n = 320, 
             lims = c(lon_range, lat_range))

kde_df <- expand.grid(
  lon = seq.int(lon_range[1], lon_range[2], length.out = 320),
  lat = seq.int(lat_range[1], lat_range[2], length.out = 320)
  )
kde_df$density <- melt(den$z)$value

summary(kde_df$density)
den_fill_scale <- scale_colour_gradient2(low = 'white', mid = 'darkgreen', 
                                         high = 'red', midpoint = 225)
den_fill_scale$train(kde_df$density, T)
kde_df$density_s <- den_fill_scale$map(kde_df$density)
kde_df$density_zeroone <- pmin(kde_df$density / max(kde_df$density), .9)


big_plot <- ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = fill), data = DowntownMap) +
  geom_tile(aes(x = lon, y = lat, fill = density_s, alpha = density_zeroone), 
            data = kde_df) +
              scale_x_continuous('Longitude', limits = lon_range) + 
              scale_y_continuous('Latitude', limits = lat_range) +
              scale_alpha(to = c(0, .9)) +
              scale_fill_identity() +
              opts(
                legend.position = 'none',
                title = 'Violent Crime Weather Map of Downtown Houston, 2010'
                ) +
                  coord_equal()

little_plot <- ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = density_s, alpha = density_zeroone), 
            data = kde_df) +
              scale_alpha(to = c(0, .9)) +
              scale_fill_identity() +
              coord_equal() +
              theme_nothing()


grid.newpage()
pushViewport(viewport(layout = grid.layout(4,4))) 
print(big_plot, vp = vplayout(1:4, 1:4))
print(little_plot, vp = vplayout(4, 4))


grid.newpage()
pushViewport(viewport(layout = grid.layout(1000,1000))) 
print(big_plot, vp = vplayout(1:1000, 1:1000))
print(little_plot, vp = vplayout(697:897, 740:940))