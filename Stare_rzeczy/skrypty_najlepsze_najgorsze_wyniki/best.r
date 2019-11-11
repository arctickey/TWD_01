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

library(dplyr)

#Skrypt umozliwia uzyskanie podgrupy osob, ktore uzyskaly x% najlepszych wynikow
#z czesci testu: NAUKA

best_in_science <- function(x, df){
  
  #Obliczenie sredniej z plausible values (z dzialu nauka) dla kazdego ucznia
  #srednia pozostanie w kolumnie 'average_pv_science'
  result <- df %>% mutate(average_pv_science = rowMeans(select(., PV1SCIE:PV10SCIE)))
  result <- result[order(result$average_pv_science, decreasing = TRUE),]
  return(result[1:floor(nrow(result)*(x/100)),])
}

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
