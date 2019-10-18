library(dplyr)

#Skrypt umozliwia uzyskanie podgrupy osob, ktore uzyskaly x% najlepszych wynikow
#z czesci testu: CZYTANIE

best_in_reading <- function(x, df){
  
  #Obliczenie sredniej z plausible values (z dzialu czytanie) dla kazdego ucznia
  #srednia pozostanie w kolumnie 'average_pv_science'
  result <- df %>% mutate(average_pv_reading = rowMeans(select(., PV1READ:PV10READ)))
  result <- result[order(result$average_pv_reading, decreasing = TRUE),]
  return(result[1:floor(nrow(result)*(x/100)),])
}

#Podaj x (wartosc liczbowa np. 5), otrzymasz x% najlepszych wynikow
#Wprowadz ramke danych: df, z ktorej chcesz wybrac podgrupe

x <- 
df <-
result <- best_in_reading(x, df)
