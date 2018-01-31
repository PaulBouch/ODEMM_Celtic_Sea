#################################################################################
#################################################################################
#####   Proportional Connectance Plots
#################################################################################

Sectors2 = melt(Sectors)

Sectors2 = Sectors2[Sectors2$variable == "Connect", ]

Sectors2$Sector = factor(Sectors2$Sector, levels = RankSum)


ggplot(Sectors2, aes(x= variable, y= value)) +
  geom_bar(stat = 'identity', colour = "red", fill = "red")+
  theme_bw()+
  facet_wrap(~Sector, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_text(angle = 0))+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())


pdf ("Connectance Plots by Sector.pdf" )
ggplot(Sectors2, aes(x= variable, y= value)) +
  geom_bar(stat = 'identity', colour = "red", fill = "red")+
  theme_bw()+
  facet_wrap(~Sector, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_blank())+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())
dev.off()

pdf ("Connectance Plots by Sector Scaled.pdf" )
ggplot(Sectors2, aes(x= variable, y= value)) +
  geom_bar(stat = 'identity', colour = "red", fill = "red")+
  theme_bw()+
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~Sector, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_blank())+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())
dev.off()