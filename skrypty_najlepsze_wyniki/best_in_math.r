library(dplyr)

#Skrypt umozliwia uzyskanie podgrupy osob, ktore uzyskaly x% najlepszych wynikow
#z czesci testu: MATEMATYKA

best_in_math <- function(x, df){

#Obliczenie sredniej z plausible values (z matematyki) dla kazdego ucznia
#srednia pozostanie w kolumnie 'average_pv_math'
result <- df %>% mutate(average_pv_math = rowMeans(select(., PV1MATH:PV10MATH)))
result <- result[order(result$average_pv_math, decreasing = TRUE),]
return(result[1:floor(nrow(result)*(x/100)),])
}

#Podaj x (wartosc liczbowa np. 5), otrzymasz x% najlepszych wynikow
#Wprowadz ramke danych: df, z ktorej chcesz wybrac podgrupe

x <- 
df <- 
result <- best_in_math(x, df)
