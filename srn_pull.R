## Read in shape file


#read in 
library(sf)
library(tidyverse)
library(leaflet)


## read in data
srn <- st_read("./Data/network.shp")


View(srn)


manchester_srn<- srn %>% filter(AUTHO_NAME == "Manchester")
birmingham_srn<- srn %>% filter(AUTHO_NAME == "Birmingham")


plot(manchester_srn)
plot(birmingham_srn)



