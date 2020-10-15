library(rgdal)
library(sp)
library(rgdal)
library(ggplot2)
library(geosphere)
library(spdep)
library(ggplot2)
library(ggmap)
library(mapproj)
library(tidygeocoder)
library(revgeo)
library(reticulate)
library(rgeos)

coord2nuts <- function(Lat, Lon, nuts=map) {
  # Lat = 52.0844
  # Lon = 20.42421
  pp <- data.frame(lon=Lon, lat=Lat)
  coordinates(pp) <- ~lon+lat
  proj4string(pp)<-CRS("+init=epsg:4326")
  map@proj4string  <- pp@proj4string 
  n0 <- over(pp,map)
  return(n0$NUTS_ID)
}


loc <- data.frame(lon=klienci$lon_mean,lat=klienci$lat_mean)
eu_nuts <- readOGR(".", "NUTS_RG_01M_2013")
map <- subset(eu_nuts, STAT_LEVL_ == 3)
proj4string(map)
coordinates(loc)<-~lon+lat
proj4string(loc)<-CRS("+init=epsg:4326")
data <- spTransform(loc, proj4string(map))

#select Poland
map@data$NUTS_ID_char <- as.character(map@data$NUTS_ID)
map@data$country <- substr(map@data$NUTS_ID_char, 1, 2) 
map <- map[map@data$country == "PL", ]
plot(map)
points(data, pch = 10, col = "darkgoldenrod")




#select Poland
map@data$NUTS_ID_char <- as.character(map@data$NUTS_ID)
map@data$country <- substr(map@data$NUTS_ID_char, 1, 2) 
map <- map[map@data$country == "PL", ]
plot(map)
points(data, pch = 10, col = "darkgoldenrod")


writeRDS(loc)

library(rgdal)
library(spdep)
library(maptools)
library(RColorBrewer)
library(classInt)
library(spatialEco)
library(plotKML)

colors <- brewer.pal(6, "BuGn")
brks <- classIntervals(spatial_data$Mean, n = 6, style = "quantile")
brks <- brks$brks
windows(12,7)
plot(spatial_data, col = colors[findInterval(spatial_data$Mean, brks, all.inside = TRUE)], axes = F)
title(paste ("Fertility indicators [%]"),sub = "source: Eurostat")
legend(x = "right", legend = leglabs(round(brks, 3), under = "below", over = "over"), fill = colors, bty = "n", x.intersp = 1, y.intersp = 1)



