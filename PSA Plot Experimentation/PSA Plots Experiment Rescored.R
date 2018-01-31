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
data$Overlap.Score = ifelse(data$Overlap == "W", 3,
                            ifelse(data$Overlap == "L", 2, 
                                   ifelse(data$Overlap == "S", 1, NA)))

data$Frequency.Score = ifelse(data$Frequency == "P", 4,
                              ifelse(data$Frequency == "C", 3, 
                                     ifelse(data$Frequency == "O", 2,
                                            ifelse(data$Frequency == "R", 1, NA ))))

data$DoI.Score = ifelse(data$DoI == "A" , 3,
                        ifelse(data$DoI == "H" , 3,
                               ifelse(data$DoI == "C", 2, 
                                      ifelse(data$DoI == "M", 2, 
                                             ifelse(data$DoI == "L", 1, NA)))))

data$Resilience.Score = ifelse(data$Resilience == "L", 3,
                               ifelse(data$Resilience == "M", 2, 
                                      ifelse(data$Resilience == "H", 1, NA)))

data$Persistence.Score = ifelse(data$Persistence == "C", 4,
                                ifelse(data$Persistence == "H", 3,
                                       ifelse(data$Persistence == "M", 2, 
                                              ifelse(data$Persistence == "L", 1, NA))))


# Check for NA's
unique(data$Overlap.Score)
unique(data$Frequency.Score)
unique(data$DoI.Score)
unique(data$Resilience.Score)
unique(data$Persistence.Score)

### Calculate Impact Risk, Recovery Lag and log IR
data$ImpactRisk = data$Overlap.Score*data$Frequency.Score*data$DoI.Score
data$RecoveryLag = data$Resilience.Score*data$Persistence.Score

## calculate total risk - IR*RL
data$TotalRisk = data$ImpactRisk * data$RecoveryLag

#### Look at fishing

fish = data[data$Sector == "Fishing", ]

fish.pressure = ddply (fish, c("Sector", "Pressure"), summarize,
                       IR.mean = mean(ImpactRisk),
                       RL.mean = mean (RecoveryLag))

ggplot (fish.pressure, aes(x= RL.mean, y = IR.mean))+
  geom_point()+
  geom_text_repel(aes(x= RL.mean, y = IR.mean, label = Pressure))+
  theme_bw()

fish.eco = ddply (fish, c("Sector", "Ecological.Characteristic"), summarize,
                  IR.mean = mean(ImpactRisk),
                  RL.mean = mean (RecoveryLag))

ggplot (fish.eco, aes(x= RL.mean, y = IR.mean))+
  geom_point()+
  geom_text_repel(aes(x= RL.mean, y = IR.mean, label = Ecological.Characteristic))+
  theme_bw()

#### But these skew the scale a bit. Can we convert to a percentage of the
#### max possible risk and recovery scores

fish.pressure$IR.Perc = fish.pressure$IR.mean/36
fish.pressure$RL.Perc = fish.pressure$RL.mean/12

ggplot (fish.pressure, aes(x= RL.Perc, y = IR.Perc))+
  geom_point()+
  geom_text_repel(aes(x= RL.Perc, y = IR.Perc, label = Pressure))+
  theme_bw()

fish.eco$IR.Perc = fish.eco$IR.mean/36
fish.eco$RL.Perc = fish.eco$RL.mean/12


ggplot (fish.eco, aes(x= RL.Perc, y = IR.Perc))+
  geom_point()+
  geom_curve(x = 0, y=0.66, xend = 0.66, yend = 0, curvature = -0.3)+
  geom_curve(x = 0, y=0.33, xend = 0.33, yend = 0, curvature = -0.3)+
  geom_text_repel(aes(x= RL.Perc, y = IR.Perc, label = Ecological.Characteristic))+
  coord_cartesian(ylim = c(0, 0.7), xlim = c(0, 0.7)) +
  theme_bw()

