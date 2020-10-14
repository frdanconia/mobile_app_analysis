library(vroom)
library(lubridate)
library(tidyverse)

klienci <- vroom("data/klienci.csv")
session_geo <- vroom("data/session_geo.csv")
session_info <- vroom("data/session_info.csv")
klienci[, 1] <- c()

length(unique(klienci$klient_id))
length(unique(klienci$klient_id[klienci$czy_w_bazie_klientow == 1]))
#Zakladajac, że baza danych zawiera wszystkich klientow firmy, firma ta ma obecnie 4766 klientów którzy znajduja sie przynajmniej w 2 bazach danych firmy oraz 6000 klientów obecnych tylko w analizowanej bazie

klienci_w_bazie <- klienci[klienci$czy_w_bazie_klientow == 1,]
sum(klienci_w_bazie$czy_kupil) / nrow(klienci_w_bazie)
#Sposrod 4766 klientów którzy znajduja sie przynajmniej w 2 bazach danych firmy  18% klientow kupilo uslugi premium

sum(klienci[klienci$czy_w_bazie_klientow == 0,]$czy_kupil) / nrow(klienci[klienci$czy_w_bazie_klientow == 0,])
#Sposrod 1234 klientów którzy znajduja sie tylko w tej jednej bazie firmy 16.85% klientow kupilo uslugi premium

sum(klienci$czy_kupil) / nrow(klienci)
#Sposrod 6000 klientów którzy znajduja sie przynajmniej w 2 bazach danych firmy  17.8% klientow kupilo uslugi premium

table(klienci$wyksztalcenie[klienci$czy_kupil == 1])
mean(klienci$wiek[!is.na(klienci$wiek)])
median(klienci$wiek[!is.na(klienci$wiek)])
#To czy model biznesowy trafia do osob wyksztalconych zalezy od struktury wyksztalcenia w populacji potencjalnych klinetow
#Nie ma ani jednej obserwacji z wyksztalceniem zasadniczym zawodowym, wiec trzeba byloby sie zastanowic czy jest ono kodowane jako podstawowe czy srednie razem z wyksztalceniem srednim zawodowym
#Zakladajac ze grupa potencjalnych klientow odzwierciedla populacje Polski
#i biorac jednak pod uwage ze sredni wiek klientow wynosi 50,87 lat a mediana 51 lat to fakt, że w populacji klientow, ktorzy wykupili uslugi
#premium znajduje sie o ponad 40% wiecej osob z wyksztalceniem wyzszym niz podstawowym nalezy uznac za sukces gdyz w populacji Polski osoby
#w tym wieku kilkukrotnie czesciej maja wyksztalcenie zasadnicze zawodowe
#Jezeli jednak model biznesowy uwzglednial kierowanie sie przede wszystkim do osob z wyksztalceniem wyzszym to
#należaloby sie zastanowic czy satysfakcjonuje nas fakt, że mamy dokladnie tyle samo klientow z wyksztalceniem srednim i wyzszym


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
#im czesciej tym srednia jest nizsza a spodziewamy sie ze im czesciej tym lepszy klient dla nas
salary$activity_frequency <- 1/(salary$avg_diff_days/min(salary$avg_diff_days))
#jezeli sredni czas miedzy iloscia sesji jest krotki to jedna sesja klienta jest "warta" wiecej
salary$nsessions_per_avgdifftime <- salary$nsessions * salary$activity_frequency
salary$activity_overall <- log(salary$nsessions_per_avgdifftime * salary$nsessions_per_ndays)


ggplot(salary, aes(x = nsessions, y = wynagrodzenie)) + geom_point() + geom_smooth(method =
                                                                                     "loess", se = F)
#Ciezko stwierdzic na ile parametr nsessions odzwierciedla czestosc korzystania z aplikacji bo jest on mocno zwiazany z tym od jak dawna dany klient jest naszym klientem
#Widać jednak, że klienci z wieksza ilosci sesji sa też klientami nieco lepiej zarabiajacymi
#Sepcjalnie nie usuwalem brakow danych w wynagrodzeniu po to zeby potem latwo zmergowac z glownym df i przeprowadzić imputacje brakow danych

ggplot(salary, aes(x = nsessions_per_ndays, y = wynagrodzenie)) + geom_point() + geom_smooth(method =
                                                                                     "loess", se = F)
#zmienna nsessions_per_ndays poza iloscia akcji jakich dokonal klient bierze pod uwage uplyw czasu od pierwszej akcji jakiej dokonal klient czyli to jak dlugo jest naszym klientem
#widać, że klienci z wyzszym wynagrodzeniem sa bardziej aktywni w naszej aplikacji jednak nalezy zwrocic uwage na to, ze wariancja jest wysoka 

temp <- salary %>% filter(activity_frequency < 0.3)
ggplot(temp, aes(x = activity_frequency, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
#osoby czesto korzystajace z aplikacji (majace bardzo niska srednio roznice w dniach miedzy jednym a nastepnym uzyciem aplikacji) maja wyzsze wynagrodzenie

ggplot(salary, aes(x = nsessions_per_avgdifftime, y = wynagrodzenie)) + geom_point() + geom_smooth(method =
                                                                                           "loess", se = F)
ggplot(salary, aes(x = activity_overall, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)


model <- lm(activity_overall~wynagrodzenie,data=salary)
summary(model)
plot(model, which=1)
plot(model, which=2)
plot(model, which=3)

plot(model, which=4)
#jest kilka outlierow

plot(model, which=5)
#Zarobki klientow maja istotny wplyw na czestotliwosc korzystania z aplikacji


ggplot(salary, aes(x = session_length_mean, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
ggplot(salary, aes(x = session_length_median, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
ggplot(salary, aes(x = session_length_variance, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
temp <- salary %>% filter(session_length_variance < 600) 
ggplot(temp, aes(x = session_length_variance, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
ggplot(salary, aes(x = session_length_90percentile, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
ggplot(salary, aes(x = session_length_10percentile, y = wynagrodzenie)) + geom_point() + geom_smooth(method = "loess", se = F)
#Zarobki klientów nie maja wplyw na czas sprzedzany w appce, srednia, mediana, wariancja i 10 i 90 percentyl spedzanego czasu na to nie wskazuja




#Zarobki klientow maja statystycznie istotny wplyw na czestotliwosc korzystania z aplikacji

