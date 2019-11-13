kolumny <- c("CNT","MISCED","FISCED","HISCED")
dane1 <- dane[, kolumny]
dane1 <- podziel_grupy_PKB(dane1)
dane1 <- dane1[, -1]
wynik <- group_by(dane1, grupa_rozwoju,MISCED) %>% count()
wynik1 <- group_by(dane1, grupa_rozwoju,FISCED) %>% count()
wynik2 <- group_by(dane1, grupa_rozwoju,HISCED) %>% count()
wynik <- na.omit(wynik)
wynik1 <- na.omit(wynik1)
wynik2<- na.omit(wynik2)
wynik <- group_by(wynik,grupa_rozwoju) %>% mutate(suma = sum(n)) %>% mutate(procent=round(n/suma,digits=3))
wynik1 <- group_by(wynik1,grupa_rozwoju) %>% mutate(suma = sum(n)) %>% mutate(procent=round(n/suma,digits=3))
wynik2 <- group_by(wynik2,grupa_rozwoju) %>% mutate(suma = sum(n)) %>% mutate(procent=round(n/suma,digits=3))

wynik3 <- melt(wynik2,id.vars = c("HISCED","grupa_rozwoju"))
wynik3 <- reshape(wynik3,idvar = "HISCED",direction  ='wide',timevar = "grupa_rozwoju")
wynik3$variable.1 <- NULL
wynik3$variable.2 <- NULL
colnames(wynik3) <- c("HISCED","Grupa_1","Grupa_2")
wynik4 <- mutate(wynik3,Grupa_1=Grupa_1/(Grupa_1+Grupa_2),Grupa_2=Grupa_2/(Grupa_1+Grupa_2))
wynik5 <- reshape(wynik4,idvar = "HISCED",direction  ='long')

a1 <- ggplot(wynik3, aes(x=Grupa_1, xend=Grupa_2, y=HISCED, group=HISCED)) + 
    geom_dumbbell(
                size=5, 
                colour_x ="cornflowerblue",
                color="#a3c4dc",
                colour_xend="firebrick3")+theme_minimal()
  





ggplot(wynik4,aes(x=HISCED,y=p,fill=grupa_rozwoju))+geom_bar(stat='identity')+
  xlab("Wyksztalcenie matek w skali ISCED")+ylab("")+ggtitle("Procent lepszego wykstalcenia rodzica w grupie")+
  scale_x_continuous(breaks = round(seq(min(wynik2$HISCED), max(wynik2$HISCED), by = 1),1)) +
  theme(legend.title = element_blank())+theme_minimal()+coord_flip()


  
