########################################################################################
#####        Experimentation
#####    Risk Identity Plots
########################################################################################

##### By Sector and sum of the risks

options(scipen = 999)
mycolours = rep(c("indianred2", "darkslateblue", "orange2", "turquoise1", "darkgreen"), 16)


SecPlot = ddply (data, "Sector", summarise,
                 Overlap = sum(Overlap.Score),
                 Frequency = sum(Frequency.Score),
                 Impact = sum(DoI.Score),
                 Resilience = sum(Resilience.Score),
                 Persistence = sum(Persistence.Score),
                 AverageTR = mean(TotalRisk),
                 SumTR = sum(TotalRisk))

SecPlot = SecPlot[order (SecPlot$SumTR, decreasing = T), ]

SecPlot$SumTR = round(SecPlot$SumTR, 4)
Rank = SecPlot$Sector
RankSum = SecPlot$Sector


SecPlot$SumTR = NULL
SecPlot$AverageTR = NULL

x = melt (SecPlot, id.vars = "Sector")


x2= x


x2$Sector = factor (x2$Sector, levels = Rank)


ggplot(x2, aes(y= value, x = variable)) +
  geom_bar(stat = 'identity', colour = mycolours, fill = mycolours)+
  theme_bw()+
  facet_wrap(~Sector, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_text(angle = 0))+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())

png ("Risk Plots by Sector (summed).png", width = 800, height = 800)
ggplot(x2, aes(y= value, x = variable)) +
  geom_bar(stat = 'identity', colour = mycolours, fill = mycolours)+
  theme_bw()+
  facet_wrap(~Sector, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_text(angle = 0))+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())
dev.off()


#########  Averaged risks

SecPlot = ddply (data, "Sector", summarise,
                 Overlap = mean(Overlap.Score),
                 Frequency = mean(Frequency.Score),
                 Impact = mean(DoI.Score),
                 Resilience = mean(Resilience.Score),
                 Persistence = mean(Persistence.Score),
                 AverageTR = mean(TotalRisk),
                 SumTR = sum(TotalRisk))

SecPlot = SecPlot[order (SecPlot$AverageTR, decreasing = T), ]


SecPlot$AverageTR = round(SecPlot$AverageTR, 4)
Rank = SecPlot$Sector

SecPlot$SumTR = NULL
SecPlot$AverageTR = NULL

x = melt (SecPlot, id.vars = "Sector")


x2= x


x2$Sector = factor (x2$Sector, levels = Rank)

ggplot(x2, aes(y= value, x = variable)) +
  geom_bar(stat = 'identity', colour = mycolours, fill = mycolours)+
  theme_bw()+
  facet_wrap(~Sector, ncol = 1, strip.position="right")+
  #  theme(strip.text.y = element_text(size= 6, angle = 0))+
  theme(strip.background = element_blank(), strip.text.y = element_blank())+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())

ggplot(x2, aes(y= value, x = variable)) +
  geom_bar(stat = 'identity', colour = mycolours, fill = mycolours)+
  theme_bw()+
  facet_wrap(~Sector, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_text(size= 6, angle = 0))+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())

png ("Risk Plots by Sector (averaged).png", width = 800, height = 800)
ggplot(x2, aes(y= value, x = variable)) +
  geom_bar(stat = 'identity', colour = mycolours, fill = mycolours)+
  theme_bw()+
  facet_wrap(~Sector, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_text(angle = 0))+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())
dev.off()

##########################################################################
### Do the same for Ecochar


##### Summed

mycolours = rep(c("indianred2", "darkslateblue", "orange2", "turquoise1", "darkgreen"), 28)

EcoPlot = ddply (data, "Ecological.Characteristic", summarise,
                 Overlap = sum(Overlap.Score),
                 Frequency = sum(Frequency.Score),
                 Impact = sum(DoI.Score),
                 Resilience = sum(Resilience.Score),
                 Persistence = sum(Persistence.Score),
                 AverageTR = mean(TotalRisk),
                 SumTR = sum(TotalRisk))

EcoPlot = EcoPlot[order (EcoPlot$SumTR, decreasing = T), ]


EcoPlot$SumTR = round(EcoPlot$SumTR, 4)
Rank = EcoPlot$Ecological.Characteristic

EcoPlot$SumTR = NULL
EcoPlot$AverageTR = NULL

x = melt (EcoPlot, id.vars = "Ecological.Characteristic")


x2= x


x2$Ecological.Characteristic = factor (x2$Ecological.Characteristic, levels = Rank)


ggplot(x2, aes(y= value, x = variable)) +
  geom_bar(stat = 'identity', colour = mycolours, fill = mycolours)+
  theme_bw()+
  facet_wrap(~Ecological.Characteristic, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_text(size= 6, angle = 0))+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())

png ("Risk Plots by Ecological Characteristic (summed).png", width = 800, height = 800)
ggplot(x2, aes(y= value, x = variable)) +
  geom_bar(stat = 'identity', colour = mycolours, fill = mycolours)+
  theme_bw()+
  facet_wrap(~Ecological.Characteristic, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_text(angle = 0))+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())
dev.off()

##### Averaged

EcoPlot = ddply (data, "Ecological.Characteristic", summarise,
                 Overlap = mean(Overlap.Score),
                 Frequency = mean(Frequency.Score),
                 Impact = mean(DoI.Score),
                 Resilience = mean(Resilience.Score),
                 Persistence = mean(Persistence.Score),
                 AverageTR = mean(TotalRisk),
                 SumTR = sum(TotalRisk))

EcoPlot = EcoPlot[order (EcoPlot$AverageTR, decreasing = T), ]


EcoPlot$AverageTR = round(EcoPlot$Average, 4)
Rank = EcoPlot$Ecological.Characteristic

EcoPlot$SumTR = NULL
EcoPlot$AverageTR = NULL

x = melt (EcoPlot, id.vars = "Ecological.Characteristic")


x2= x


x2$Ecological.Characteristic = factor (x2$Ecological.Characteristic, levels = Rank)


ggplot(x2, aes(y= value, x = variable)) +
  geom_bar(stat = 'identity', colour = mycolours, fill = mycolours)+
  theme_bw()+
  facet_wrap(~Ecological.Characteristic, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_text(size= 6, angle = 0))+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())

png ("Risk Plots by Ecological Characteristic (averaged).png", width = 800, height = 800)
ggplot(x2, aes(y= value, x = variable)) +
  geom_bar(stat = 'identity', colour = mycolours, fill = mycolours)+
  theme_bw()+
  facet_wrap(~Ecological.Characteristic, ncol = 1, strip.position="right")+
  theme(strip.text.y = element_text(angle = 0))+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())
dev.off()
