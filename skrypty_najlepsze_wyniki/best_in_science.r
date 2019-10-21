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
