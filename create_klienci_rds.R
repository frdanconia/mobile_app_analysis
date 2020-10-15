#	Firma chce wdrożyć odpowiednią komunikację marketingową dla klientów, u których można stwierdzić szanse
# kupna produktu premium. Stwórz model klasyfikacyjny, którego celem jest przewidywanie którzy klienci mogą wykupić usługę premium. Jaka jest skuteczność takiego modelu? Jakie zmienne mają największy wpływ na decyzję o kupnie?
library(dplyr)
library(vroom)
library(geosphere)
library(lubridate)
library(sp)
library(rgdal)

klienci <- vroom("data/klienci.csv")
session_geo <- vroom("data/session_geo.csv")
session_info <- vroom("data/session_info.csv")
klienci[, 1] <- c()
session <- merge(session_info, session_geo, by = "id_sesji")

eu_nuts <- readOGR(".", "NUTS_RG_01M_2013")
map <- subset(eu_nuts, STAT_LEVL_ == 3)
lon_lat <- data.frame(lon = session$lon, lat = session$lat)

coord2nuts <- function(lon_lat) {
  coordinates(lon_lat) <- ~ lon + lat
  proj4string(lon_lat) <- CRS("+init=epsg:4326")
  map@proj4string <- lon_lat@proj4string
  rt <- over(lon_lat, map)
  return(rt)
}
session$nuts3 <- coord2nuts(lon_lat)$NUTS_ID

salary <-
  data.frame(klient_id = klienci$klient_id,
             wynagrodzenie = klienci$wynagrodzenie)

for (i in 1:length(salary$klient_id)) {
  salary$nsessions[i] <-
    sum(session_info$klient_id == salary$klient_id[i])
}

for (i in 1:length(salary$klient_id)) {
  salary$session_length_mean[i] <-
    mean(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]])
  salary$session_length_median[i] <-
    median(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]])
}


for (i in 1:length(salary$klient_id)) {
  salary$session_length_mean[i] <-
    mean(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]])
  salary$session_length_median[i] <-
    median(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]])
  salary$session_length_variance[i] <-
    var(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]])
  salary$session_length_90percentile[i] <-
    quantile(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]], 0.9)[[1]]
  salary$session_length_10percentile[i] <-
    quantile(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]], 0.1)[[1]]
}


for (i in 1:length(salary$klient_id)) {
  dates <-
    session_info$date[session_info$klient_id == salary$klient_id[i]]
  dates <- dates[dates != "0"]
  dates <- dates[!is.na(dates)]
  dates <- as.Date(dates)
  salary$klient_ndays[i] <- as.numeric(today() - min(dates))
}

for (i in 1:length(salary$klient_id)) {
  dates <-
    session_info$date[session_info$klient_id == salary$klient_id[i]]
  dates <- dates[dates != "0"]
  dates <- dates[!is.na(dates)]
  dates <- as.Date(dates)
  salary$avg_diff_days[i] <- as.numeric(mean(diff(sort(dates))))
}

salary$klient_ndays <-
  salary$klient_ndays / min(salary$klient_ndays)
salary$nsessions_per_ndays <-
  salary$nsessions / salary$klient_ndays
salary$activity_frequency <-
  1 / (salary$avg_diff_days / min(salary$avg_diff_days))

salary$nsessions_per_avgdifftime <-
  salary$nsessions * salary$activity_frequency


salary$activity_overall <-
  log(salary$nsessions_per_avgdifftime * salary$nsessions_per_ndays)

poland_nuts3_population <- vroom("poland_nuts3_population.csv")
poland_nuts3_population <-
  poland_nuts3_population[poland_nuts3_population$TIME == 2013, ]

for (i in 1:length(klienci$klient_id)) {
  dates <-
    session_info$date[session_info$klient_id == klienci$klient_id[i]]
  dates <- dates[dates != "0"]
  dates <- dates[!is.na(dates)]
  dates <- as.Date(dates)
  klienci$first_day[i] <- min(dates)
}

klienci_ordered <- klienci[order(klienci$first_day), ]
klienci_ordered$nclients <-
  seq(1, length(klienci_ordered$klient_id), by = 1)
#klienci_ordered$day <- as.Date(klienci_ordered$first_day)

for (i in 1:length(klienci$klient_id)) {
  max_session <- session[session$klient_id == klienci$klient_id[i],][which.min(unique(session[session$klient_id == klienci$klient_id[i],])$lon + unique(session[session$klient_id == klienci$klient_id[i],])$lat), ]
  min_session <- session[session$klient_id == klienci$klient_id[i],][which.max(unique(session[session$klient_id == klienci$klient_id[i],])$lon + unique(session[session$klient_id == klienci$klient_id[i],])$lat), ]
  
  klienci$max_min_dist[i] <- distm(c(min_session$lon,min_session$lat), c(max_session$lon,max_session$lat))
}

klienci_ordered <- data.frame(klient_id=klienci_ordered$klient_id,nclient = klienci_ordered$nclients)

df <- klienci %>% left_join(klienci_ordered, by = "klient_id")
salary$wynagrodzenie <- c()
df2 <- df %>% left_join(salary, by = "klient_id")

for (i in 1:length(klienci$klient_id)) {
  klienci$avg_lon[i] <- mean(session[session$klient_id == klienci$klient_id[i],]$lon)
  klienci$avg_lat[i] <- mean(session[session$klient_id == klienci$klient_id[i],]$lat)
}

avg_coord <- data.frame(lon=klienci$avg_lon,lat=klienci$avg_lat)

df2$avg_loc <- coord2nuts(avg_coord)$NUTS

saveRDS(df2,"klienci.RDS")
