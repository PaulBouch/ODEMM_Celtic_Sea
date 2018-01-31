require (RColorBrewer)
require (plyr)
require (ggplot2)
require (gridExtra)
require (pvclust)
require (reshape2)
require (ggrepel)


# read in data
data = read.csv("Data\\Celtic SeaS Pressure Assessment PB 24_01_18 Consistent Resilience.csv")

# remove rows with No overlap
data = data[!data$Overlap == "NO", ]

# only the desriptor rows and their scores
data = data[ , c(1:8)]
# add columns with the values asscociated with the clasifactions
data$Overlap.Score = ifelse(data$Overlap == "W", 1,
                            ifelse(data$Overlap == "L", 0.37, 
                                   ifelse(data$Overlap == "S", 0.03, NA)))

data$Frequency.Score = ifelse(data$Frequency == "P", 1,
                              ifelse(data$Frequency == "C", 0.67, 
                                     ifelse(data$Frequency == "O", 0.33,
                                            ifelse(data$Frequency == "R", 0.08, NA ))))

data$DoI.Score = ifelse(data$DoI == "A" , 1,
                        ifelse(data$DoI == "H" , 1,
                               ifelse(data$DoI == "C", 0.13, 
                                      ifelse(data$DoI == "M", 0.13, 
                                             ifelse(data$DoI == "L", 0.01, NA)))))

data$Resilience.Score = ifelse(data$Resilience == "L", 0.55,
                               ifelse(data$Resilience == "M", 0.06, 
                                      ifelse(data$Resilience == "H", 0.01, NA)))

data$Persistence.Score = ifelse(data$Persistence == "C", 1,
                                ifelse(data$Persistence == "H", 0.55,
                                       ifelse(data$Persistence == "M", 0.06, 
                                              ifelse(data$Persistence == "L", 0.01, NA))))

# Check for NA's
unique(data$Overlap.Score)
unique(data$Frequency.Score)
unique(data$DoI.Score)
unique(data$Resilience.Score)
unique(data$Persistence.Score)

### Calculate Impact Risk, Recovery Lag and log IR
data$ImpactRisk = data$Overlap.Score*data$Frequency.Score*data$DoI.Score
data$RecoveryLag = data$Resilience.Score*data$Persistence.Score
data$LN.IR = log(data$ImpactRisk)

# calculate recovery years from resilience and persistence
# Can do by adding resilience to persistence and multipling by 100
data$Ryr =  (data$Resilience.Score + data$Persistence.Score) *100

## calculate total risk - IR*RL
data$TotalRisk = data$ImpactRisk * data$RecoveryLag

#### Facet plot all sectors

sector.summary = ddply (data, c("Sector", "Pressure"), summarise,
                        IR.mean = mean(ImpactRisk),
                        RL.mean = mean(RecoveryLag),
                        TR.mean = mean(TotalRisk),
                        IR.sum = sum(ImpactRisk),
                        RL.sum = sum(RecoveryLag),
                        TR.sum = sum(TotalRisk)) 

TR25 = sector.summary
TR25 = TR25[order (TR25$TR.mean, decreasing = TRUE)[1:25],]

png ("PSA Plots with averaged risk.png", width = 400, height = 400)
ggplot (sector.summary, aes(x= RL.mean, y =IR.mean))+
  geom_point()+
  geom_text(data=TR25, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()+
  facet_wrap(~ Sector)
dev.off()

TR25 = sector.summary
TR25 = TR25[order (TR25$TR.sum, decreasing = TRUE)[1:25],]

png ("PSA Plots with summed risk.png", width = 400, height = 400)
ggplot (sector.summary, aes(x= RL.sum, y =IR.sum))+
  geom_point()+
  geom_text(data=TR25, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()+
  facet_wrap(~ Sector)
dev.off()

#### Log transform and label the top 25 IR and RL

sector.summary$logIR = log(sector.summary$IR.mean)
sector.summary$logRL = log(sector.summary$RL.mean)

TR25.Sector = merge(sector.summary, TR25)

png ("PSA Plots with log averaged risk.png", width = 400, height = 400)
ggplot (sector.summary, aes(x= logRL, y = logIR))+
  geom_point()+
  geom_text(data = TR25.Sector, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()+
  facet_wrap(~ Sector)
dev.off()


sector.summary$logIR = log(sector.summary$IR.sum)
sector.summary$logRL = log(sector.summary$RL.sum)

TR25.Sector = merge(sector.summary, TR25)

png ("PSA Plots with log summed risk.png", width = 400, height = 400)
ggplot (sector.summary, aes(x= logRL, y = logIR))+
  geom_point()+
  geom_text(data = TR25.Sector, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()+
  facet_wrap(~ Sector)
dev.off()


#### Just fishing

Fishing = sector.summary[sector.summary$Sector == "Fishing", ]

png ("PSA Plots for fishing with log summed risk.png", width = 400, height = 400)
ggplot (Fishing, aes(x= logRL, y = logIR))+
  geom_point()+
  geom_text_repel(aes(x= logRL, y = logIR, label = Pressure))+
  #  geom_text(data = Fishing, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()
dev.off()


#######################################################################
#### To seperate the fishing out, but plot sector summed by eco char

sector.summary = ddply (data, c("Sector", "Ecological.Characteristic"), summarise,
                        IR.mean = mean(ImpactRisk),
                        RL.mean = mean(RecoveryLag),
                        TR.mean = mean(TotalRisk),
                        IR.sum = sum(ImpactRisk),
                        RL.sum = sum(RecoveryLag),
                        TR.sum = sum(TotalRisk)) 

TR25 = sector.summary
TR25 = TR25[order (TR25$TR.sum, decreasing = TRUE),]

sector.summary$logIR = log(sector.summary$IR.sum)
sector.summary$logRL = log(sector.summary$RL.sum)

TR25.Sector = merge(sector.summary, TR25)

Fishing = sector.summary[sector.summary$Sector == "Fishing", ]

png ("PSA Plots for fishing and ecological characteristic with log summed risk.png", width = 400, height = 400)
ggplot (Fishing, aes(x= logRL, y = logIR))+
  geom_point()+
  geom_text_repel(aes(x= logRL, y = logIR, label = Ecological.Characteristic))+
  #  geom_text(data = Fishing, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()
dev.off()











################################################################################

###########################################################
#### Seperate by eco char

eco.summary = ddply (data, c("Pressure", "Ecological.Characteristic"), summarise,
                     IR.sum = sum(ImpactRisk),
                     RL.sum = sum(RecoveryLag),
                     TR.sum = sum(TotalRisk)) 

TR25eco = eco.summary
TR25eco = TR25eco[order (TR25eco$TR.sum, decreasing = TRUE)[1:25],]

ggplot (eco.summary, aes(x= RL.sum, y =IR.sum))+
  geom_point()+
  geom_text(data = TR25eco, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()+
  facet_wrap(~ Ecological.Characteristic)

eco.summary$logIR = log(eco.summary$IR.sum)
eco.summary$logRL = log(eco.summary$RL.sum)

TR25.Eco = merge(eco.summary, TR25eco)

png ("PSA Plots of Eco Char and Pressure with log summed risk.png", width = 400, height = 400)
ggplot (eco.summary, aes(x= logRL, y = logIR))+
  geom_point()+
  geom_text(data = TR25.Eco, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()+
  facet_wrap(~ Ecological.Characteristic)
dev.off()



###################################################################################
### Don't want to do mean values of the total risk
###################################################################################

ggplot (data, aes(x= RL, y =IR))+
  geom_point()+
  #  geom_text(data=TR25, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()+
  facet_wrap(~ Sector)

data$logIR = log10(data$IR)
data$logRL = log10(data$RL)

TR100 = data
TR100 = TR100[order (TR100$TotalRisk, decreasing = TRUE)[1:100],]

ggplot (data, aes(x= logRL, y = logIR))+
  geom_point()+
  geom_text(data=TR100, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()+
  facet_wrap(~ Sector)

ggplot (data, aes(x= logRL, y = logIR))+
  geom_point()+
  geom_text(data=TR100, aes(label=Pressure),hjust=0, vjust=0)+
  theme_bw()+
  facet_wrap(~ Ecological.Characteristic)

ggplot (data, aes(x= logRL, y = logIR))+
  geom_point()+
  geom_text(data=TR100, aes(label=Sector),hjust=0, vjust=0)+
  theme_bw()+
  facet_wrap(~ Ecological.Characteristic)
