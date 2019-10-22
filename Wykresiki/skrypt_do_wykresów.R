### wyktres bast_in_all po krajach 

data_plot  <- result[,c(1,length(colnames(result)))]

# średnia dla danego kraju 
data_plot1<- data_plot %>% group_by(CNT)%>% summarise(mean_all_by_CNT=mean(average_pv_all))

# Wykres dla wszystkich krajów 

plot <- ggplot(data_plot1,aes(x=reorder(CNT,-mean_all_by_CNT),y=mean_all_by_CNT,group=1,label=CNT))+geom_line()

### SYF NIE POLECAM 
ggsave("Wykresiki/all_average_by_CNT.png",width = 18, height = 9)
### analogiczne wykresy dla pozostałych best_inn
plot_best <- function(x,df){
 #tworzy 4 wykresy dla możliwych best in  x to procent najlepszych
label <- c("all_average_by_CNT","math_average_by_CNT","reading_average_by_CNT","science_average_by_CNT")
label <- paste0(label,x)
label <- paste0("Wykresiki/",label[1:4],".png")
pointer <- 1
for (i in c(best_in_all,best_in_math,best_in_reading,best_in_science)) {
  
  result <- i(x, df)
  data_plot  <- result[,c(1,length(colnames(result)))]
  data_plot1<- aggregate(data_plot,by=list(data_plot$CNT),FUN = mean)
  data_plot1 <- data_plot1[,c(1,3)]
  colnames(data_plot1) <- c("CNT","mean_by_CNT") 
  plot1 <- ggplot(data_plot1,aes(x=reorder(CNT,-mean_by_CNT),y=mean_by_CNT,group=1,label=CNT))+geom_line()
  ggsave(label[pointer],width = 18, height = 9)
  pointer <-  pointer+1
}
}
plot_best(50,df)

