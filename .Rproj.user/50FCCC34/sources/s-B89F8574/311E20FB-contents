mapa <- spTransform(map, "+proj=longlat")

mapa_2 <- mapa %>%
  fortify(region = "NUTS_ID") %>%
  left_join(freq, by = c("id" = "NUTS_ID")) 

spTransform(mapa_2, "+proj=longlat")
mapa_2
colnames(freq) <- c("NUTS_ID","freq")

mapa <- map %>%
  left_join(freq$Freq, by = "NUTS_ID") 


loc <- data.frame(geo_mean$lon_mean,geo_mean$lat_mean)
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

