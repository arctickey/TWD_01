### wyktres bast_in_all po krajach 

dane <- haven::read_sas("../cy6_ms_cmb_stu_qqq.sas7bdat")
#dane <- select(dane, CNT,CNTSTUID,PV1MATH:PV10SCIE)
dane_1 <- best_in_reading(10,dane)## ZMIEN W CELU ZMIANY DANYCH z pissa
dane_1 <- dane_1[,c(1,33)]
dane_1<- dane_1 %>% group_by(CNT)%>% summarise(mean_all_by_CNT=mean(average_pv_reading))## I TU 
colnames(dane_1)[2] <- "i10"
for( i in 2:5){
dane_2 <- best_in_reading(i*10,dane)## i TU
dane_2<- dane_2 %>% group_by(CNT)%>% summarise(mean_all_by_CNT=mean(average_pv_reading))## I TU
dane_1 <- merge(dane_1,dane_2,by="CNT")
colnames(dane_1)[1+i] <- paste0("i",toString(i*10))
}

library(reshape2)

dane_test <- melt(dane_1)
# Wykres dla wszystkich krajów 

plot <- ggplot(dane_test,aes(x=reorder(CNT,-value),y=value,group=1))+geom_line(aes(color=variable,group=variable),size=4)+ 
  ggtitle("Średnie wyniki w kraju z czytania")+labs(y = "Średni wynik w kraju z czytania", x="Kod kraju")+theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20),
  legend.text = element_text(size=20),
  legend.title = element_text( size = 20),
  title = element_text(size=20)) + scale_color_discrete(name = "Procent najlepszych", labels = c("10%", "20%", "30%","40%","50%"))


plot

ggsave("Wykresiki/best_read.png",width = 18, height = 9)
