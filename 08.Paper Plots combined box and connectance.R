###############################################################################
#######  Box Plots and proprotional connectance plot. #########################
#######    Used for publication    ############################################
###############################################################################

require (ggpubr)
require (gridExtra)
require (RColorBrewer)
require (ggplot2)
require(plyr)
require (grid)

### 

################################################################################
#### Sectors

SecRank = ddply (data, "Sector", summarise,
                 AvgTR = mean(TotalRisk))

SecRank = SecRank[order(-SecRank$AvgTR),]

BoxPlotSector = merge(BPIS, SecRank)

Sectors = merge (Sectors, SecRank)

#################################################################################
#####  Remove the x axis text for the sector and pressure plots
#################################################################################

### Sectors
Seca = ggplot (Sectors, aes (x= reorder(Sector, AvgTR), y = Connect))+
  geom_bar(stat = "identity", 
           color = "black", 
           fill = colorRampPalette(brewer.pal(9,"Pastel1"))(16)) +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,0,0,6), "mm"))


Secb = ggplot (BoxPlotSector , aes(x= reorder(Sector, AvgTR), y = ImpactRisk))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(16))+
  stat_boxplot(geom ='errorbar', width =0.5)+ coord_flip()+
  theme_bw()+
  ylab("Impact Risk")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,0,0,0), "mm"))

Secc= ggplot (BoxPlotSector, aes(x= reorder(Sector, AvgTR), y = LN.IR))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(16))+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  coord_flip()+
  theme_bw()+
  ylab("Impact Rank")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,0,0,0), "mm"))

Secd = ggplot (BoxPlotSector, aes(x= reorder(Sector, AvgTR), y = Ryr))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(16))+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  coord_flip()+
  theme_bw()+
  ylab ("Recovery (Years)")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,5,0,0), "mm"))

BoxSector = grid.arrange(Seca, Secb , Secc, Secd, ncol = 4, widths = c(9, 4, 4, 4))

# pdf ("Box Plot Sectors no x label.pdf", width = 12, height = 8)
 grid.arrange(Seca, Secb , Secc, Secd, ncol = 4, widths = c(9, 4, 4, 4))
# dev.off()

###############################################################################
#### Pressures

PressRank = ddply (data, "Pressure", summarise,
                   AvgTR = mean(TotalRisk))

PressRank = PressRank[order(-PressRank$AvgTR),]


BoxPlotPressure = merge(BPIS, PressRank)

Pressures = merge (Pressures, PressRank)

#### Pressures

Pressa = ggplot (Pressures, aes (x= reorder(Pressure, AvgTR), y = Connect))+
  geom_bar(stat = "identity", 
           color = "black", 
           fill = colorRampPalette(brewer.pal(9,"Pastel1"))(23)) +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,0,5,5), "mm"))


Pressb = ggplot (BoxPlotPressure , aes(x= reorder(Pressure, AvgTR), y = ImpactRisk))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(23))+
  stat_boxplot(geom ='errorbar', width =0.5)+ coord_flip()+
  theme_bw()+
  ylab("Impact Risk")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,0,5,0), "mm"))

Pressc= ggplot (BoxPlotPressure, aes(x= reorder(Pressure, AvgTR), y = LN.IR))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(23))+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  coord_flip()+
  theme_bw()+
  ylab("Impact Rank")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,0,5,0), "mm"))

Pressd = ggplot (BoxPlotPressure, aes(x= reorder(Pressure, AvgTR), y = Ryr))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(23))+
  coord_flip()+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  theme_bw()+
  ylab ("Recovery (Years)")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,5,5,0), "mm"))

BoxPressure = grid.arrange(Pressa, Pressb , Pressc, Pressd, ncol = 4, widths = c(9, 4, 4, 4))

# pdf ("Box Plot Pressures no x label.pdf", width = 12, height = 8)
 grid.arrange(Pressa, Pressb , Pressc, Pressd, ncol = 4, widths = c(9, 4, 4, 4))
# dev.off()

###############################################################################
#### Eco Char

EcoRank = ddply (BPIS, "EcoChar", summarise,
                 AvgTR = mean(TotalRisk))

EcoRank = EcoRank[order(-EcoRank$AvgTR),]


BoxPlotEco = merge(BPIS, EcoRank)

Eco = merge (Eco, EcoRank)

#### Plots

Ecoa = ggplot (Eco, aes (x= reorder(EcoChar, AvgTR), y = Connect))+
  geom_bar(stat = "identity", 
           color = "black", 
           fill = colorRampPalette(brewer.pal(9,"Pastel1"))(28)) +
  ylab ("Connectance")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        plot.margin = unit(c(0,0,5,7), "mm"))

Ecob = ggplot (BoxPlotEco , aes(x= reorder(EcoChar, AvgTR), y = ImpactRisk))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(28))+
  stat_boxplot(geom ='errorbar', width =0.5)+ coord_flip()+
  theme_bw()+
  ylab("Impact Risk")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=12),
        plot.margin = unit(c(0,0,5,0), "mm"))

Ecoc= ggplot (BoxPlotEco, aes(x= reorder(EcoChar, AvgTR), y = LN.IR))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(28))+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  coord_flip()+
  theme_bw()+
  ylab("Impact Rank")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=12),
        plot.margin = unit(c(0,0,5,0), "mm"))

Ecod = ggplot (BoxPlotEco, aes(x= reorder(EcoChar, AvgTR), y = Ryr))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(28))+
  coord_flip()+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  theme_bw()+
  ylab ("Recovery (Years)")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=12),
        plot.margin = unit(c(0,5,5,0), "mm"))

BoxEco = grid.arrange(Ecoa, Ecob , Ecoc, Ecod, ncol = 4, widths = c(8, 4, 4, 4))

#pdf ("Box Plot Eco.pdf", width = 12, height = 8)
grid.arrange(Ecoa, Ecob , Ecoc, Ecod, ncol = 4, widths = c(9, 4, 4, 4))
#dev.off()

#########################################################
### Combine all together
#########################################################

pdf ("Box Plot Crazy Arrange EMF 29_06_18.pdf", width = 12, height = 18)
grid.arrange(BoxSector, BoxPressure, BoxEco, ncol = 1, heights =  c(6,7,8))
grid.text("a) Sectors", x = unit(0.02, "npc"), y = unit(0.989, "npc"), just = "left", gp=gpar(fontsize=16))
grid.text("b) Pressures", x = unit(0.02, "npc"), y = unit(0.71, "npc"), just = "left", gp=gpar(fontsize=16))
grid.text("c) Ecological Characteristic", x = unit(0.02, "npc"), y = unit(0.388, "npc"), just = "left", gp=gpar(fontsize=12))
#grid.text("Characteristic", x = unit(0.035, "npc"), y = unit(0.383, "npc"), just = "left", gp=gpar(fontsize=13))
dev.off()

