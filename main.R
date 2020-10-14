library(vroom)

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

plot(salary$nsessions, salary$wynagrodzenie)
library(tidyverse)
ggplot(salary, aes(x = nsessions, y = wynagrodzenie)) + geom_point() + geom_smooth(method =
                                                                                     "loess", se = F)
#Ciezko stwierdzic na ile parametr nsessions odzwierciedla czestosc korzystania z aplikacji bo jest on mocno zwiazany z tym od jak dawna dany klient jest naszym klientem
#Widać jednak, że klienci z wieksza ilosci sesji sa też klientami nieco lepiej zarabiajacymi

for (i in 1:length(salary$klient_id)) {
  salary$session_length_mean[i] <-
    mean(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]])
  salary$session_length_median[i] <-
    median(session_info$dlugosc_sesji_min[session_info$klient_id == salary$klient_id[i]])
}
 
library(lubridate)

for (i in 1:length(salary$klient_id)) {
  dates <- session_info$date[session_info$klient_id == salary$klient_id[i]]
  dates <- dates[dates != "0"]
  dates <- dates[!is.na(dates)]
  dates <- as.Date(dates)
  salary$klient_ndays[i] <- as.numeric(today() - min(dates))
}

salary$klient_ndays <- salary$klient_ndays - (min(salary$klient_ndays) - 1)

