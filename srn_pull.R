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

write_sf(birmingham_srn, "./birmingham_srn")

plot(manchester_srn)
plot(birmingham_srn)

#transform data
birmingham_srn<- st_transform(birmingham_srn, crs = "+init=epsg:4326")


##build leaflet map ----

srn_pop <- paste0("Road Number: ",
                  birmingham_srn$ROA_NUMBER,
                  "<br>",
                  "Location: ",
                  birmingham_srn$LOCATION)

leaflet(birmingham_srn) %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addPolygons(stroke = 0.4, color = "black", fillOpacity = 0.1,
              popup = ~srn_pop) 


