require (RColorBrewer)
require (plyr)
require (ggplot2)
require (gridExtra)
require (pvclust)
require (reshape2)

setwd("H:\\ODEMM\\Celtic 24_01_18")

# read in data
data = read.csv("Celtic SeaS Pressure Assessment PB 24_01_18 Consistent Resilience.csv")

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
                        ifelse(data$DoI == "C", 0.13,
                               ifelse(data$DoI == "L", 0.01, NA)))

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

data$TotalRisk = data$ImpactRisk * data$RecoveryLag

BPIS = data
names(BPIS)[names(BPIS) == 'Ecological.Characteristic'] <- 'EcoChar'
