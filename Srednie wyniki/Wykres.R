dane <- cbind(daneWr,daneSr)
colnames(dane) <- c("Pytanie","Rozwinięte_posiada","Rozwinięte_nie_posiada","Nierozwinięte_posiada"
                    ,"Nierozwinięte_nie_posiada")
df2 = tidyr::gather(dane,group,value,-Pytanie)
gg <- ggplot(data = dane) + theme_minimal()+
  geom_point(data = df2, aes(x = value, color = group,y=Pytanie), size = 4)+
  geom_dumbbell( aes(x=Rozwinięte_nie_posiada , xend=Rozwinięte_posiada, y=Pytanie, group=Pytanie),
                color="#e3e2e1", size=2,colour_x = '#33a02c',colour_xend = "#fc8d62")+ 
  geom_dumbbell( aes(x=Nierozwinięte_posiada , xend=Nierozwinięte_nie_posiada , y=Pytanie, group=Pytanie),
                                 color="#e3e2e1", size=2,colour_x = "#1f78b4",colour_xend = "#ffd92f")+
  labs(x=NULL, 
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
        axis.text.x  = element_text(size=12))+
    scale_color_manual(name = "", values = c("#ffd92f","#1f78b4",'#33a02c',"#fc8d62"))
plot(gg)


ggsave("./PLAKAT/Wyniki w zaleznosci od posiadania itemkow.svg",gg,height=8,width=18)
ggsave("./Wyniki w zaleznosci od posiadania itemkow.svg",gg,height=8,width=18)


