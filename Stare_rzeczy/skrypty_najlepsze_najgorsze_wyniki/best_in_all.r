library(dplyr)

#Skrypt umozliwia uzyskanie podgrupy osob, ktore uzyskaly x% najlepszych wynikow
#bierzemy pod uwage srednia z 3 kategorii: matematyka, czytanie oraz nauka

best_in_all <- function(x, df){
  #Podaj x (wartosc liczbowa np. 5), otrzymasz x% najlepszych wynikow
  #Wprowadz ramke danych: df, z ktorej chcesz wybrac podgrupe
  
  #Obliczenie sredniej z plausible values dla kazdego ucznia
  #srednia pozostanie w kolumnie 'average_pv_math'
  result <- df %>% mutate(average_pv_all = rowMeans(select(., PV1MATH:PV10SCIE)))
  result <- result[order(result$average_pv_all, decreasing = TRUE),]
  return(result[1:floor(nrow(result)*(x/100)),])
}

