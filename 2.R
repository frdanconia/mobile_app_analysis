#W trakcie analizy powstała potrzeba posiadania informacji
#jaki jest procent pokrycia powierzchni mieszkalnej w promieniu stu metrów od klienta.
#Na podstawie odpowiednich źródeł danych przygotuj zestawienie (również w formie wizualizacji na mapie)
#jaki procent stumetrowego bufora wokół poszczególnego klienta to powierzchnia mieszkalna.
#devtools::install_github("hrbrmstr/overpass")

library(overpass)
library(osmdata)
library(sp)
library(vroom)
session_geo <- vroom("data/session_geo.csv")

check_oo <- function(lat, lon, meter) {
  lat1 <- as.numeric(lat - (180 / pi) * (meter / 6378137))
  lon1 <-
    as.numeric(lon - (180 / pi) * (meter / 6378137) / cos(lat1))
  lat2 <- as.numeric(lat + (180 / pi) * (meter / 6378137))
  lon2 <-
    as.numeric(lon + (180 / pi) * (meter / 6378137) / cos(lat2))
  bb <- c(lat1, lon1, lat2, lon2)
  
  bbstring <- paste(bb, collapse = ",")
  
  vv <- "residential"
  q1 <-
    paste0('  way["highway"="', vv, '"](', bbstring, ');', collapse = '\n')
  qq <-
    paste0('[out:xml][timeout:25]; \n( \n',
           q1,
           '); \nout body; \n>; \nout skel qt;')
  
  oo <- overpass_query(qq)
  
  return(oo)
}

check_residental_coverage <- function(lat, lon, meter) {
  lat1 <- as.numeric(lat - (180 / pi) * (meter / 6378137))
  lon1 <-
    as.numeric(lon - (180 / pi) * (meter / 6378137) / cos(lat1))
  lat2 <- as.numeric(lat + (180 / pi) * (meter / 6378137))
  lon2 <-
    as.numeric(lon + (180 / pi) * (meter / 6378137) / cos(lat2))
  bb <- c(lat1, lon1, lat2, lon2)
  
  bbstring <- paste(bb, collapse = ",")
  
  vv <- "residential"
  q1 <-
    paste0('  way["highway"="', vv, '"](', bbstring, ');', collapse = '\n')
  qq <-
    paste0('[out:xml][timeout:25]; \n( \n',
           q1,
           '); \nout body; \n>; \nout skel qt;')
  
  oo <- overpass_query(qq)
  
  if (is.null(oo)) {
    rt <- 0
  } else {
    rt <- (length(oo@data$highway) / meter) * 10
  }
  
  return(rt)
}


check_residental_coverage(session_geo$lat[10], session_geo$lon[10], 1000)

oo <- check_oo(session$lat[3], session$lon[3], 5000)
oo@data$tunnel[is.na(oo@data$tunnel)] <- "no"
plot(oo,
     col = factor(oo@data$highway),
     lty = (oo@data$tunnel == "yes") + 1)
legend(
  x = "right",
  legend = unique(oo@data$highway),
  col = unique(factor(oo@data$highway)),
  lty = 1,
  cex = 0.7
)


