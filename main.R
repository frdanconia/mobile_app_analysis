library(vroom)

klienci <- vroom("data/klienci.csv")
klienci[,1] <- c()

length(unique(klienci$klient_id))
length(unique(klienci$klient_id[klienci$czy_w_bazie_klientow == 1]))
#Zakladajac, że baza danych zawiera wszystkich klientow firmy, firma ta ma obecnie 4766 klientów którzy znajduja sie przynajmniej w 2 bazach danych firmy oraz 6000 klientów obecnych tylko w analizowanej bazie

klienci_w_bazie <- klienci[klienci$czy_w_bazie_klientow == 1,]
sum(klienci_w_bazie$czy_kupil)/nrow(klienci_w_bazie)
#Sposrod 4766 klientów którzy znajduja sie przynajmniej w 2 bazach danych firmy  18% klientow kupilo uslugi premium

sum(klienci[klienci$czy_w_bazie_klientow == 0,]$czy_kupil)/nrow(klienci[klienci$czy_w_bazie_klientow == 0,])
#Sposrod 1234 klientów którzy znajduja sie tylko w tej jednej bazie firmy 16.85% klientow kupilo uslugi premium

sum(klienci$czy_kupil)/nrow(klienci)
#Sposrod 6000 klientów którzy znajduja sie przynajmniej w 2 bazach danych firmy  17.8% klientow kupilo uslugi premium

table(klienci$wyksztalcenie[klienci$czy_kupil == 1])
mean(klienci$wiek[!is.na(klienci$wiek)])
median(klienci$wiek[!is.na(klienci$wiek)])
#To czy model biznesowy trafia do osob wyksztalconych zalezy od struktury wyksztalcenia w populacji
#Nie ma ani jednej obserwacji z wyksztalceniem zasadniczym zawodowym, wiec trzeba byloby sie zastanowic czy jest ono kodowane jako podstawowe czy srednie razem z wyksztalceniem srednim zawodowym
#Biorac jednak pod uwage ze sredni wiek klientow wynosi 50,87 lat a mediana 51 lat to fakt, że w populacji klientow, ktorzy wykupili uslugi 
#premium znajduje sie o ponad 40% wiecej osob z wyksztalceniem wyzszym niz podstawowym nalezy uznac za sukces gdyz w populacji Polski osoby 
#w tym wieku kilkukrotnie czesciej maja wyksztalcenie zasadnicze zawodowe 
#Jezeli jednak model biznesowy uwzglednial kierowanie sie przede wszystkim do osob z wyksztalceniem wyzszym to  
#należaloby sie zastanowic czy satysfakcjonuje nas fakt, że mamy dokladnie tyle samo klientow z wyksztalceniem srednim i wyzszym 


klienci$wynagrodzenie

klienci$
