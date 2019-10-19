library(haven)
library(tidyverse)
library(reshape2)
library(xgboost)
average_scores2_math <- function(df){
  country_means_math <- select(df,CNT,PV1MATH:PV10MATH) %>% group_by(CNT) %>% 
    summarise(mean1 = mean(PV1MATH),
              mean2 = mean(PV2MATH), mean3 = mean(PV3MATH),mean4 = mean(PV4MATH),mean5 = mean(PV5MATH),mean6 = mean(PV6MATH),
              mean7 = mean(PV7MATH), mean8 = mean(PV8MATH), mean9 = mean(PV9MATH), mean10 = mean(PV10MATH))
  country_means_math <-rowMeans(select(country_means_math,-c(CNT)))
  nazwy <- unique(df$CNT)
  x <- data.frame(CNT=nazwy,mean_math=country_means_math)
  df <- left_join(x,df,by="CNT")
}
best_in_math <- function(x, df){
  
  #Obliczenie sredniej z plausible values (z matematyki) dla kazdego ucznia
  #srednia pozostanie w kolumnie 'average_pv_math'
  result <- df %>% mutate(average_pv_math = rowMeans(select(., PV1MATH:PV10MATH)))
  result <- result[order(result$average_pv_math, decreasing = TRUE),]
  return(result[1:floor(nrow(result)*(x/100)),])
}


data<- haven::read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")
df <- select(data, CNT,CNTSTUID,PV1MATH:PV10SCIE)
result_math <- best_in_math(20, df)
result_math <- as.data.frame(result_math$CNTSTUID)
colnames(result_math) <- c("CNTSTUID")
dane_math <- left_join(result_math,data,by="CNTSTUID")

dane_math1 <- average_scores2_math(dane_math)
x <- as.matrix(select(dane_math1, -c(mean_math)) %>% select_if(is.numeric))
y <- dane_math1$mean_math

params = list('num_leaves'= 128,
  'objective'= 'reg:squarederror',
  'max_depth'=13,
    'eta'= 0.03,
  "boosting_type"="gbdt",
  "subsample"= 0.9,
  "bagging_seed"= 11,
  'alpha'= 0.3,
  'lambda'= 0.3,
  'colsample_bytree'= 0.9)

model <- xgboost(data=x,label=y,params = params,nthread=-1,nrounds=10)
importance_matrix <- xgb.importance(model=model)
write.csv(importance_matrixFeature,"Cechy_xgboost.csv")
