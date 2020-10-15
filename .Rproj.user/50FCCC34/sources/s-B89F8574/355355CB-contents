library(vroom)
library(lubridate)
library(tidyverse)
library(lmtest)
library(rgdal)
library(sp)
library(spdep)
library(rgeos)
library(foreach)
library(dplyr)
library(tidyr)
library(mice)

klienci <- vroom("data/klienci.csv")
session_geo <- vroom("data/session_geo.csv")
session_info <- vroom("data/session_info.csv")
klienci[, 1] <- c()

#1.	Ilu firma ma obecnie klientów i jaki procent z nich korzysta z usługi premium?
length(unique(klienci$klient_id))
length(unique(klienci$klient_id[klienci$czy_w_bazie_klientow == 1]))
#1. Zakladajac, że baza danych zawiera wszystkich klientow firmy, firma ta ma obecnie 4766 klientów którzy znajduja sie przynajmniej w 2 bazach danych firmy oraz 6000 klientów obecnych tylko w analizowanej bazie

klienci_w_bazie <- klienci[klienci$czy_w_bazie_klientow == 1,]
sum(klienci_w_bazie$czy_kupil) / nrow(klienci_w_bazie)
#1. Sposrod 4766 klientów którzy znajduja sie przynajmniej w 2 bazach danych firmy  18% klientow kupilo uslugi premium

sum(klienci[klienci$czy_w_bazie_klientow == 0,]$czy_kupil) / nrow(klienci[klienci$czy_w_bazie_klientow == 0,])
#1. Sposrod 1234 klientów którzy znajduja sie tylko w tej jednej bazie firmy 16.85% klientow kupilo uslugi premium

sum(klienci$czy_kupil) / nrow(klienci)
#1. Sposrod 6000 klientów którzy znajduja sie przynajmniej w 2 bazach danych firmy  17.8% klientow kupilo uslugi premium

#2.	Jak ze względu na dane demograficzne, geograficzne i wykorzystywania aplikacji można podzielić użytkowników firmy?

session <- merge(session_info,session_geo,by="id_sesji")

eu_nuts <- readOGR(".", "NUTS_RG_01M_2013")
map <- subset(eu_nuts, STAT_LEVL_ == 3)
lon_lat <- data.frame(lon=session$lon,lat=session$lat)

coord2nuts <- function(lon_lat) {
  coordinates(lon_lat) <- ~lon+lat
  proj4string(lon_lat) <- CRS("+init=epsg:4326")
  map@proj4string <- lon_lat@proj4string 
  rt <- over(lon_lat,map)
  return(rt)
}

session$nuts3 <- coord2nuts(lon_lat)$NUTS_ID
freq <- data.frame(table(session$nuts3))
freq <- freq[grepl("PL",freq$Var1),]

map@data$NUTS_ID_char <- as.character(map@data$NUTS_ID)
map@data$country <- substr(map@data$NUTS_ID_char, 1, 2) 
map <- map[map@data$country == "PL", ]

spatial_data <- merge(y = freq, x = map, by.y = "NUTS_ID", by.x = "NUTS_ID", sort = FALSE, all.x = F)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(spatial_data$freq, 6)
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(spatial_data$freq, brks,all.inside = TRUE)], axes = F, main = "Ilosc bezwzgledna przypadkow uzycia aplikacji")

#2. Uzytkownikow firmy mozna podzielic na poszczegolne podregiony NUTS3 w ktorych najczesciej uzywaja aplikacji
#W wartosciach bezwezglednym aplikacja jest uzywana rzadko w podregionie szczecinsko-pyrzyckim, z kolei jest uzywana czesto w regionie zielonogórskim. Nie mowi to jeszcze o popularnosci aplikacji poniewaz nie bierze pod uwage populacji danych regionow.
# Widoczny jest takze podzial na Polske pólnocna i poludniowa, na poludniu wiecej osob korzysta z aplikacji niz na polnocy

barplot(table(klienci$plec),names.arg=c('Mezczyna','Kobieta'))
#Na plcie, wsrod klientow jest wiecej mezczyzn niz kobiet

barplot(table(klienci$czy_samochod),names.arg=c('Brak samochodu','Posiada samochod'))
#Na fakt posiadania samochodow, wsrod klientow jest wiecej osob posiadajacych samochod

barplot(table(klienci$czy_mieszkanie),names.arg=c('Brak mieszkania','Posiada mieszkania'))
#Na fakt posiadania mieszkania, minimalnie wiecej osob posiada mieszkanie
mean(klienci$wiek[!is.na(klienci$wiek)])
median(klienci$wiek[!is.na(klienci$wiek)])
#Jest to zaskakujace biorac pod uwage sredni wiek/mediane wieku 50.9/51 lat klientow
#W Polsce ponad 84% mieszkań wlasnosciowych i nie mamy do czynienia z klientami bardzo mlodymi 
#zatem zaskakujaco duzo sposrod naszych klientow nie posiada mieszkania

quantile(klienci$wiek, na.rm = TRUE)
#Ze wzgledu na wiek
#25% klientow jest w wieku w wieku od 18 do 25 lat
#25% klientow jest w wieku w wieku od 47 do 51 lat
#25% klientow jest w wieku w wieku od 51 do 55 lat
#25% klientow jest w wieku w wieku od 55 do 73 lat
#Aż 50% klientów jest w wieku od 47 do 55 lat
#Widac wiec ze bardzo wielu naszych klientow jest w poznych wieku srednim
#Srednia wynoi 51 lat i jest bardzo bliska mediany, nie ma wyraznej asymetrii rozkładu wieku klientow
densityplot(klienci$wiek)


#3.	Czy model płatny trafia zgodnie z założeniami do wykształconej grupy odbiorców?
table(klienci$wyksztalcenie[klienci$czy_kupil == 1])
mean(klienci$wiek[!is.na(klienci$wiek)])
median(klienci$wiek[!is.na(klienci$wiek)])
#3. To czy model biznesowy trafia do osob wyksztalconych zalezy od struktury wyksztalcenia w populacji potencjalnych klinetow
#Nie ma ani jednej obserwacji z wyksztalceniem zasadniczym zawodowym, wiec trzeba byloby sie zastanowic czy jest ono kodowane jako podstawowe czy srednie razem z wyksztalceniem srednim zawodowym
#Zakladajac ze grupa potencjalnych klientow odzwierciedla populacje Polski
#i biorac jednak pod uwage ze sredni wiek klientow wynosi 50,87 lat a mediana 51 lat to fakt, że w populacji klientow, ktorzy wykupili uslugi
#premium znajduje sie o ponad 40% wiecej osob z wyksztalceniem wyzszym niz podstawowym nalezy uznac za sukces gdyz w populacji Polski osoby
#w tym wieku kilkukrotnie czesciej maja wyksztalcenie zasadnicze zawodowe
#Jezeli jednak model biznesowy uwzglednial kierowanie sie przede wszystkim do osob z wyksztalceniem wyzszym to
#należaloby sie zastanowic czy satysfakcjonuje nas fakt, że mamy dokladnie tyle samo klientow z wyksztalceniem srednim i wyzszym


#4.	Czy zarobki użytkowników mają wpływ na częstotliwość korzystania z aplikacji i czas spędzany w apce?
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
    quantile(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]],0.9)[[1]]
  salary$session_length_10percentile[i] <-
    quantile(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]],0.1)[[1]]
}
 

for (i in 1:length(salary$klient_id)) {
  dates <- session_info$date[session_info$klient_id == salary$klient_id[i]]
  dates <- dates[dates != "0"]
  dates <- dates[!is.na(dates)]
  dates <- as.Date(dates)
  salary$klient_ndays[i] <- as.numeric(today() - min(dates))
}

for (i in 1:length(salary$klient_id)) {
  dates <- session_info$date[session_info$klient_id == salary$klient_id[i]]
  dates <- dates[dates != "0"]
  dates <- dates[!is.na(dates)]
  dates <- as.Date(dates)
  salary$avg_diff_days[i] <- as.numeric(mean(diff(sort(dates))))
}

salary$klient_ndays <- salary$klient_ndays / min(salary$klient_ndays)
salary$nsessions_per_ndays <-  salary$nsessions/salary$klient_ndays
#4.im czesciej tym srednia jest nizsza a spodziewamy sie ze im czesciej tym lepszy klient dla nas
salary$activity_frequency <- 1/(salary$avg_diff_days/min(salary$avg_diff_days))
#4.jezeli sredni czas miedzy iloscia sesji jest krotki to jedna sesja klienta jest "warta" wiecej
salary$nsessions_per_avgdifftime <- salary$nsessions * salary$activity_frequency
salary$activity_overall <- log(salary$nsessions_per_avgdifftime * salary$nsessions_per_ndays)


ggplot(salary, aes(x = nsessions, y = wynagrodzenie)) + geom_point() + geom_smooth(method =
                                                                                     "loess", se = F)
#4. Ciezko stwierdzic na ile parametr nsessions odzwierciedla czestosc korzystania z aplikacji bo jest on mocno zwiazany z tym od jak dawna dany klient jest naszym klientem
#Widać jednak, że klienci z wieksza ilosci sesji sa też klientami nieco lepiej zarabiajacymi
#Sepcjalnie nie usuwalem brakow danych w wynagrodzeniu po to zeby potem latwo zmergowac z glownym df i przeprowadzić imputacje brakow danych

ggplot(salary, aes(x = nsessions_per_ndays, y = wynagrodzenie)) + geom_point() + geom_smooth(method =
                                                                                     "loess", se = F)
#4. zmienna nsessions_per_ndays poza iloscia akcji jakich dokonal klient bierze pod uwage uplyw czasu od pierwszej akcji jakiej dokonal klient czyli to jak dlugo jest naszym klientem
#widać, że klienci z wyzszym wynagrodzeniem sa bardziej aktywni w naszej aplikacji jednak nalezy zwrocic uwage na to, ze wariancja jest wysoka 

temp <- salary %>% filter(activity_frequency < 0.3)
ggplot(temp, aes(x = activity_frequency, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
#4. osoby czesto korzystajace z aplikacji (majace bardzo niska srednio roznice w dniach miedzy jednym a nastepnym uzyciem aplikacji) maja wyzsze wynagrodzenie

ggplot(salary, aes(x = nsessions_per_avgdifftime, y = wynagrodzenie)) + geom_point() + geom_smooth(method =
                                                                                           "loess", se = F)
ggplot(salary, aes(x = activity_overall, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)

model <- lm(activity_overall~wynagrodzenie,data=salary)
summary(model)
plot(model, which=1)
plot(model, which=2)
#4. Obserwacje odstajace na pierwszych kwantylach 
plot(model, which=4)
#4. jest kilka outlierow, zmiany wartoci wspóczynników regresji przy pominiciu tych obserwacji nie przekraczaja 0.1 
bptest(model)
#4. Na kazdym realistycznym poziomie istotnosci odrzucamy H0 o stabilnosci wariancji skladnika losowego
#Charakter tej zaleznosci w lepszym stopniu oddaje regresja ważona lokalnie (loess) widoczna na wykresach niz regresja liniowa.

#4.Podsumowywujac - tak, istnieje zaleznosc miedzy zarobkami klientow a czestotliowoscia uzywania aplikacji i jest ona widoczna na wykresach 
#(tj. osoby ponadprzecietnie czesto korzystajace z aplikacji sa zazwyczaj osobami ponadprzecietnei zarabiajacymi)
salary
#choc wariancja jest dosyc wysoka, jest duże grono osób ponadprzecietnie zarabiajacych i nieróżniacych sie od osob biedniejszych pod katem czestotliwosci uzywania aplikacji.


ggplot(salary, aes(x = session_length_mean, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
ggplot(salary, aes(x = session_length_median, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
ggplot(salary, aes(x = session_length_variance, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
temp <- salary %>% filter(session_length_variance < 600) 
ggplot(temp, aes(x = session_length_variance, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
ggplot(salary, aes(x = session_length_90percentile, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
ggplot(salary, aes(x = session_length_10percentile, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
#4. Zarobki klientów nie maja wplywu na czas sprzedzany w appce, srednia, mediana, wariancja i 10 i 90 percentyl spedzanego czasu na to nie wskazuja


#5.	W jakich regionach kraju aplikacja nie jest jeszcze zbyt popularna?


#6.	Jak przyrastała nam liczba użytkowników w czasie? Czy było jakieś wydarzenie, które miało wpływ na liczbę użytkowników?


#7.	Jakie rodzaje segmentów użytkowników aplikacji posiadamy? Które segmenty częściej sięgają po usługę premium?


#8.	Czy klienci korzystają z aplikacji w jednym miejscu, czy może w większej liczbie miejsc? Jaki jest średni rozrzut odległości w wykorzystaniu aplikacji?


