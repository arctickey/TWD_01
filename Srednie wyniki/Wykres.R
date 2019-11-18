dane <- cbind(daneWr,daneSr)
dane[9,1] <- "Smartfon"
colnames(dane) <- c("Pytanie","Rozwinięte_posiada","Rozwinięte_nie_posiada","Nierozwinięte_posiada"
                    ,"Nierozwinięte_nie_posiada")
names <- c("Nie posiada","Posiada","Nierozwinięte","Rozwinięte")
names1 <- c("Słownik","Smartfon","Ciche miejsce do nauki","Łazienka","Biurko do nauki",
                   "Własny pokój","Połączenie z Internetem","Komputer",
                   "Auto","Dzieło sztuki","Instrument muzyczny","Tablet","Oprogramowanie edukacyjne")
dane <- dane[order(names1),]
df2 = tidyr::gather(dane,group,value,-Pytanie)
gg <- ggplot(data = dane) + theme_minimal()+
  geom_point(data = df2, aes(x = value, color = group,y=Pytanie), size = 5,inherit.aes = FALSE)+
  geom_dumbbell( aes(x=Rozwinięte_nie_posiada , xend=Rozwinięte_posiada, y=Pytanie, group=Pytanie),
                color="#66c2a5", size=2,colour_x = '#ffd92f',colour_xend = "#1f78b4")+ 
  geom_dumbbell( aes(x=Nierozwinięte_posiada , xend=Nierozwinięte_nie_posiada , y=Pytanie, group=Pytanie),
                                 color="#fc8d62", size=2,colour_x = "#1f78b4",colour_xend = "#ffd92f")+
  labs(x="Średni wynik", 
       y=NULL, 
       title="Wynik w zależności od grupy i posiadania różnych udogodnień") +
  theme(plot.title = element_text(hjust=0.5, face="bold",size=18),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        legend.text = element_text(size=10),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=18),
        legend.position = 'top',
        axis.text.x  = element_text(size=12))+
    scale_y_discrete(limits=rev(names1))+
    scale_color_manual(name = "",labels=names,values = c("#ffd92f","#1f78b4",'#fc8d62',"#66c2a5"))
plot(gg)


ggsave("./PLAKAT/Wyniki w zaleznosci od posiadania itemkow.svg",gg,height=10,width=10)
ggsave("./Wyniki w zaleznosci od posiadania itemkow.svg",gg,height=10,width=10)



