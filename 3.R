#	Firma chce wdrożyć odpowiednią komunikację marketingową dla klientów, u których można stwierdzić szanse
# kupna produktu premium. Stwórz model klasyfikacyjny, którego celem jest przewidywanie którzy klienci mogą wykupić usługę premium. Jaka jest skuteczność takiego modelu? Jakie zmienne mają największy wpływ na decyzję o kupnie?

library(vroom)
library(lubridate)
library(xgboost)
library(mice)
library(xgboost)
library(matrixStats)
library(Matrix)
library(tidyverse)


#source("VarSummary.R")
#VarSummary(data)
data <- readRDS("klienci.RDS")


md.pattern(data)

imputed_data <- mice(data, m=5, maxit = 50, method = 'pmm', seed = 500)
densityplot(imputed_data)

dt <- complete(imputed_data,3)


source("PlotVariableSparsity.R")

PlotVariableSparsity(onehotmatrix)

#Rzadkosc 0/1 jest akceptalna

source("PlotVariableCardinality.R")
PlotVariableCardinality(dt,"czy_kupil")

#niektore kategorie maja bardzo niewiele obserwacji, mozna by pomyslec nad redukcja ilosci kategorii poprzez inne kodowanie zmiennych, z drugiej strony i tak mamy zaledwie 6000 obserwacji

table(dt$avg_loc)

#konwersja NUTS3 do NUTS2

for(i in 1:length(dt$avg_loc)){
  if(!is.na(dt$avg_loc[i])){ 
    if(nchar(dt$avg_loc[i]) == 5){
      dt$avg_loc[i] <- substr(dt$avg_loc[i],1,nchar(dt$avg_loc[i])-1)
    }
  }
}


table(dt$avg_loc)

#Przypisanie brakow i zagranicy to oddzielnej

for(i in 1:length(dt$avg_loc)){
  if(is.na(dt$avg_loc[i])){ 
    dt$avg_loc[i] <- "OTHER"
  }
  
  if(!grepl("PL",dt$avg_loc[i])){
    dt$avg_loc[i] <- "OTHER"
  }

}


table(dt$avg_loc)


onehotmatrix <- model.matrix(object = czy_kupil ~ ., data = dt)

temp_df <- data.frame(onehotmatrix)
temp_df <- temp_df %>% left_join(dt)



col.scale <- colMaxs(abs(onehotmatrix))
x.sc <- sweep(onehotmatrix, 2, col.scale, "/")

dtrain <- xgb.DMatrix(Matrix(x.sc, sparse = TRUE),
                      label = as.integer(temp_df$czy_kupil))

xgb.params <- list(
  "booster" = "gbtree",
  "eta" = 0.05,
  "max_depth" = 4,
  "subsample" = 0.632,
  "colsample_bytree" = 0.4,
  "colsample_bylevel" = 0.6,
  "min_child_weight" = 1,
  "gamma" = 0,
  "lambda" = 0,
  "alpha" = 0,
  "objective" = "binary:logistic",
  "eval_metric" = "auc",
  "silent" = 1,
  "nthread" = 4,
  "num_parallel_tree" = 5
)

set.seed(2020)
cv.out <-
  xgb.cv(
    params = xgb.params,
    data = dtrain,
    nrounds = 1.5e3,
    metrics = list('error'),
    nfold = 5,
    prediction = FALSE,
    verbose = TRUE,
    showsd = FALSE,
    print.every.n = 10,
    early.stop.round = 10,
    maximize = TRUE
  ) 

cv.out

#0.723 auc na zbiorze testowym przy odchyleniu standardowym 0.032 przy 5 krotnej cross-validacji, przyzwoity wynik jak na tak maly zbior danych
#bez przekodowania zmiennej avg_loc model uzyskiwal maksymalne auc na zbiorze testowym na  poziomie 0.7

xgb.model <- xgb.train(data = dtrain,
                       params = xgb.params,
                       nrounds = 500)


var.imp <- xgb.importance(colnames(x.sc), model = xgb.model) %>%
  mutate(Feature = gsub('[0-9]+', '', Feature)) %>%
  group_by(Feature) %>%
  summarise(Importance = quantile(Gain, 0.9)) %>%
  ungroup() %>%
  arrange(desc(Importance)) %>%
  mutate(Importance = round(100 * Importance / sum(Importance), 2))

var.imp

#Na decyzje o kupnie najwiekszy wplyw ma 
#a) wiek (wysoka istotnosc parametru bylo widac juz na etapie ekspolarcji zbioru danych)
#b) wynagrodzenie
#c) plec
#d) statystyki dlugosc uzywania aplikacji


#przypuszczalnie czestotliowsc uzywania aplikacji ma wiekszy wplyw niż wynikaloby to z importance matrix z uwagi na to, ze jest ona reprezentowana przez rozne, silnie wspolniniowe zmienne
#xgboost jak wszystkie modele drzewiaste nie jest wrazliwy wspolliniowosc zmiennych pod katem jakosci tworzenia modelu, wspolliniowosc ma jednak wplyw na ksztalt importance matrix



