library(forecast)

categorization <- function(selected_data) {
  qantiles <-
    quantile(selected_data, c(.2, .4, .6, .8), na.rm = TRUE)
  
  vector <- c()
  
  
  for (i in 1:length(selected_data)) {
    if (!is.na(selected_data[i])) {
      if (selected_data[i] <= qantiles[[1]]) {
        vector[i] <- 2
      }
      else if (selected_data[i] > qantiles[[1]] &&
               selected_data[i] <= qantiles[[2]]) {
        vector[i] <- 3
      }
      else if (selected_data[i] > qantiles[[2]] &&
               selected_data[i] <= qantiles[[3]]) {
        vector[i] <- 4
      }
      else if (selected_data[i] > qantiles[[3]] &&
               selected_data[i] <= qantiles[[4]]) {
        vector[i] <- 5
      }
      else if (selected_data[i] > qantiles[[4]]) {
        vector[i] <- 6
      }
      
    } else {
      vector[i] <- 1
    }
  }
  
  return(vector)
}



library(mice)
library(forcats)
old_data <- read.csv("data.csv")
dataset <- read.csv("data_predict.csv")
imputed_data <- mice(dataset, m=5, maxit = 50, method = 'pmm', seed = 500)
dataset <- complete(imputed_data,3)
joblost_categoories <- fct_lump(dataset$joblost, 2)
dataset$joblost <- joblost_categoories

lambda1 <- BoxCox.lambda(old_data$statemb)
lambda2 <- BoxCox.lambda(old_data$stateur)

dataset$statemb <- BoxCox(dataset$statemb, lambda1)
dataset$stateur <- BoxCox(dataset$stateur, lambda2)

dataset$age <- as.factor(categorization(dataset$age))
dataset$statemb <- as.factor(categorization(dataset$statemb))
dataset$stateur <- as.factor(categorization(dataset$stateur))

hd <- cbind(
  data.frame(model.matrix(~ age - 1, dataset)),
  data.frame(model.matrix(~ stateur - 1, dataset)),
  data.frame(model.matrix(~ statemb - 1, dataset)),
  data.frame(model.matrix(~ joblost - 1, dataset)),
  data.frame(model.matrix(~ school12 - 1, dataset)),
  data.frame(model.matrix(~ married - 1, dataset)),
  data.frame(model.matrix(~ dkids - 1, dataset)),
  data.frame(model.matrix(~ dykids - 1, dataset)),
  data.frame(model.matrix(~ head - 1, dataset))
)

