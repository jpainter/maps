## map of points
# Based of shipping route map at 
# http://spatialanalysis.co.uk/2012/03/mapped-british-shipping-1750-1800/

require(Hmisc) 
require(RODBC)
require(ggplot2)

## download zipped data from http://www.ucm.es/info/cliwoc/
  path <- getwd()
  URL <- "http://www.knmi.nl/"
  PATH <- "cliwoc/download/"
  FILE <- "CLIWOC15_2000.zip"
  download.file(paste(URL,PATH,FILE,sep=""),              
                paste(path,"CLIWOC15_2000.zip",sep=""))
  dir <- unzip(paste(path,"CLIWOC15_2000.zip",sep=""))
  file <- substr(dir,3,nchar(dir))

## read access data
  dat.chanel <- odbcConnectAccess(file)
  dat <- sqlFetch(dat.chanel, "CLIWOC15")

## Plot
  tmp <- dat[,c("Lon3","Lat3")]
  ggplot(tmp,aes(Lon3,Lat3))+
  geom_point(alpha=0.01,size=1) +
  coord_map() +
  ylim(-90,90)

