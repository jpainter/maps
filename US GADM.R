# US map
library(ggplot2)

# Load RDA from gadm.org and rename gadm object as states
states.url<- url("http://gadm.org/data/rda/USA_adm1.RData")
  load(states.url)
  states<- gadm
rm(gadm)

plot(states)
