
gg <- ggplot() + theme_minimal()+
  geom_dumbbell(data=daneWr, aes(x=Wynik_nie_ma_Wr , xend=Wynik_ma_Wr , y=Pytanie, group=Pytanie),
                color="#1f78b4", size=2,colour_x = '#33a02c',colour_xend = "#e31a1c")+ 
  geom_dumbbell(data=daneSr, aes(x=Wynik_nie_ma_Wr , xend=Wynik_ma_Wr , y=Pytanie, group=Pytanie),
                                 color="#ff7f00", size=2,colour_x = '#ffff33',colour_xend = "#a65628")+
  labs(x=NULL, 
       y=NULL, 
       title="Wynik w zależności od grupy i posiadania róznych itemków") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        panel.border=element_blank())
plot(gg)

ggsave("Wyniki w zaleznosci od posiadania itemkow.svg",gg,height=8,width=18)

