########################################################################################################
#########################################################################################################
#######################           Rankings Tables           ###############################################
#########################################################################################################
#########################################################################################################

### Want to rank the biggest risks (Total Risk, Impact Risk and Recovery Lag)

BPIS = BPIS[order(BPIS$TotalRisk, decreasing = T), ]


### Top 25 impact chains by the three measures
TotalRiskChains25 = BPIS[order(BPIS$TotalRisk, decreasing = T) [1:25], ]
TotalRiskChains25 = TotalRiskChains25[ , c(1:8, 14, 15, 18)]

ImpactRiskChains25 = BPIS[order(BPIS$ImpactRisk, decreasing = T) [1:25], ]
ImpactRiskChains25 = ImpactRiskChains25[ , c(1:8, 14, 15, 18)]

RecoveryLagChains25 = BPIS[order(BPIS$RecoveryLag, decreasing = T) [1:25], ]
RecoveryLagChains25 = RecoveryLagChains25[ , c(1:8, 14, 15, 18)]

### Top 50 impact chains by the three measures
TotalRiskChains50 = BPIS[order(BPIS$TotalRisk, decreasing = T) [1:50], ]
TotalRiskChains50 = TotalRiskChains50[ , c(1:8, 14, 15, 18)]

ImpactRiskChains50 = BPIS[order(BPIS$ImpactRisk, decreasing = T) [1:50], ]
ImpactRiskChains50 = ImpactRiskChains50[ , c(1:8, 14, 15, 18)]

RecoveryLagChains50 = BPIS[order(BPIS$RecoveryLag, decreasing = T) [1:50], ]
RecoveryLagChains50 = RecoveryLagChains50[ , c(1:8, 14, 15, 18)]

### Top 100 impact chains by the three measures
TotalRiskChains100 = BPIS[order(BPIS$TotalRisk, decreasing = T) [1:100], ]
TotalRiskChains100 = TotalRiskChains100[ , c(1:8, 14, 15, 18)]

ImpactRiskChains100 = BPIS[order(BPIS$ImpactRisk, decreasing = T) [1:100], ]
ImpactRiskChains100 = ImpactRiskChains100[ , c(1:8, 14, 15, 18)]

RecoveryLagChains100 = BPIS[order(BPIS$RecoveryLag, decreasing = T) [1:100], ]
RecoveryLagChains100 = RecoveryLagChains100[ , c(1:8, 14, 15, 18)]

####################################################################################
###### Need to aggregate the data to see better patterns
##########################################################
##### Let's focus on sector initially

Sec = ddply (data, "Sector", summarise,
             AverageTR = mean(TotalRisk),
             SumTR = sum(TotalRisk),
             AverageIR = mean(ImpactRisk),
             SumIR = sum(ImpactRisk))

Sec.Ave = Sec[order (Sec$AverageTR, decreasing = T), ]
Sec.Sum = Sec[order (Sec$SumTR, decreasing = T), ]

Sec.Ave[, 2:5] = round(Sec.Ave[, 2:5], 5)
Sec.Sum[, 2:5] = round(Sec.Sum[, 2:5], 5)
row.names(Sec.Ave) <- NULL


pdf ("Total Risk Ranks by Sector (Average).pdf")
grid.table(Sec.Ave, rows = NULL)
dev.off()

pdf ("Total Risk Ranks by Sector (Sum).pdf")
grid.table(Sec.Sum, rows = NULL)
dev.off()




Sec.Eco = ddply (data, c("Sector", "Ecological.Characteristic"), summarise,
                 AverageTR = mean(TotalRisk),
                 SumTR = sum(TotalRisk))

Sec.Eco.Ave = Sec.Eco[order (Sec.Eco$AverageTR, decreasing = T), ]
Sec.Eco.Sum = Sec.Eco[order (Sec.Eco$SumTR, decreasing = T), ]

######  Ok Let's look at pressure

Pres = ddply (data, "Pressure", summarise,
              AverageTR = mean(TotalRisk),
              SumTR = sum(TotalRisk), 
              AverageIR = mean(ImpactRisk),
              SumIR = sum(ImpactRisk))

Pres.Ave = Pres[order (Pres$AverageTR, decreasing = T), ]
Pres.Sum = Pres[order (Pres$SumTR, decreasing = T), ]

Pres.Ave[, 2:5] = round(Pres.Ave[, 2:5], 5)
Pres.Sum[, 2:5] = round(Pres.Sum[, 2:5], 5)

pdf ("Total Risk Ranks by Pressure (Average).pdf")
grid.table(Pres.Ave, rows = NULL)
dev.off()

pdf ("Total Risk Ranks by Pressure (Sum).pdf")
grid.table(Pres.Sum, rows = NULL)
dev.off()

######  Ok Let's look at ecological 

EcoCh = ddply (data, "Ecological.Characteristic", summarise,
               AverageTR = mean(TotalRisk),
               SumTR = sum(TotalRisk), 
               AverageIR = mean(ImpactRisk),
               SumIR = sum(ImpactRisk))

EcoCh.Ave = EcoCh[order (EcoCh$AverageTR, decreasing = T), ]
EcoCh.Sum = EcoCh[order (EcoCh$SumTR, decreasing = T), ]

EcoCh.Ave[, 2:5] = round(EcoCh.Ave[, 2:5], 5)
EcoCh.Sum[, 2:5] = round(EcoCh.Sum[, 2:5], 5)

pdf ("Total Risk Ranks by Ecological Characteristic (Average).pdf", height = 10, width = 6)
grid.table(EcoCh.Ave, rows = NULL)
dev.off()

pdf ("Total Risk Ranks by Ecological Characteristic (Sum).pdf", height = 10, width = 6)
grid.table(EcoCh.Sum, rows = NULL)
dev.off()

####### Sector and Eco

Sec.Eco = ddply (data, c("Sector", "Ecological.Characteristic"), summarise,
                 AverageTR = mean(TotalRisk),
                 SumTR = sum(TotalRisk))

Sec.Eco.Ave = Sec.Eco[order (Sec.Eco$AverageTR, decreasing = T)[1:25], ]
Sec.Eco.Sum = Sec.Eco[order (Sec.Eco$SumTR, decreasing = T)[1:25], ]

png ("Total Risk Ranks by Sector and Ecological Characteristic (Average).png", width =600, height = 800)
grid.table(Sec.Eco.Ave)
dev.off()

png ("Total Risk Ranks by Sector and Ecological Characteristic (Sum).png", width =600, height = 800)
grid.table(Sec.Eco.Sum)
dev.off()


###### Sector and pressure

Sec.Pres = ddply (data, c("Sector", "Pressure"), summarise,
                  AverageTR = mean(TotalRisk),
                  SumTR = sum(TotalRisk))

Sec.Pres.Ave = Sec.Pres[order (Sec.Pres$AverageTR, decreasing = T)[1:25], ]
Sec.Pres.Sum = Sec.Pres[order (Sec.Pres$SumTR, decreasing = T)[1:25], ]

png ("Total Risk Ranks by Sector and Pressure (Average).png", width =600, height = 800)
grid.table(Sec.Pres.Ave)
dev.off()

png ("Total Risk Ranks by Sector and Pressure (Sum).png", width =600, height = 800)
grid.table(Sec.Pres.Sum)
dev.off()

###### Eco and pressure

Eco.Pres = ddply (data, c("Pressure", "Ecological.Characteristic"), summarise,
                  AverageTR = mean(TotalRisk),
                  SumTR = sum(TotalRisk))

Eco.Pres.Ave = Eco.Pres[order (Eco.Pres$AverageTR, decreasing = T)[1:25], ]
Eco.Pres.Sum = Eco.Pres[order (Eco.Pres$SumTR, decreasing = T)[1:25], ]


png ("Total Risk Ranks by Pressure and Ecological Characteristic (Average).png", width =600, height = 800)
grid.table(Eco.Pres.Ave)
dev.off()

png ("Total Risk Ranks by Pressure and Ecological Characteristic (Sum).png", width =600, height = 800)
grid.table(Eco.Pres.Sum)
dev.off()