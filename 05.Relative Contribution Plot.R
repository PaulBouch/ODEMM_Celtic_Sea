#################################################################
#################################################################
#####     Relative Contribition Plots    ########################
#################################################################
#################################################################

## Plots of relative contribution of risk

require (reshape2)

#### Calcultate relative risk
BPIS$TotalRisk = BPIS$ImpactRisk * BPIS$RecoveryLag

TotIR = sum (BPIS$ImpactRisk)
TotRL = sum (BPIS$RecoveryLag)
TotTR = sum (BPIS$TotalRisk)

BPIS$relIR = (BPIS$ImpactRisk/TotIR)*100
BPIS$relRL = (BPIS$RecoveryLag/TotRL)*100
BPIS$relTR = (BPIS$TotalRisk/TotTR)*100

relIR = (BPIS$ImpactRisk/TotIR)*100
relRL = (BPIS$RecoveryLag/TotRL)*100
relTR = (BPIS$TotalRisk/TotTR)*100
relIR = sort (relIR, decreasing = T)
relRL = sort (relRL, decreasing = T)
relTR = sort (relTR, decreasing = T)

newd = as.data.frame (cbind(relIR, relRL, relTR))
newd$index = c(1:length(newd$relIR))

df = melt (newd, id.vars = "index")

df$variable = as.character(df$variable)
df$variable [df$variable == "relIR"] = "IR Relative Contribution"
df$variable [df$variable == "relRL"] = "RL Relative Contribution"
df$variable [df$variable == "relTR"] = "TR Relative Contribution"
df$variable = as.factor(df$variable)


#pdf("Relative Contribution Plot.pdf")
ggplot (df, aes(x= index, y=value, color = variable), log = "y")+
  geom_line (size=1) +
  geom_abline(intercept = 0, slope = 0)+ ##log scale
  scale_y_continuous(trans='log10', breaks=c(10,1,0.1,0.01,0.001, 0.0001, 0.00001, 0.000001, 0.0000001))+
  theme_bw()+
  theme (axis.title.x=element_blank(),
         axis.text.x =element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         legend.title = element_blank(),
         legend.position = c(0.7, 0.7))
#dev.off()

### No legend and background removed
ggplot (df, aes(x= index, y=value, color = variable), log = "y")+
  geom_line (size=1) +
  geom_abline(intercept = 0, slope = 0)+ ##log scale
  scale_y_continuous(trans='log10', breaks=c(10,1,0.1,0.01,0.001, 0.0001, 0.00001, 0.000001, 0.0000001))+
  theme_classic()+
  theme (axis.title.x=element_blank(),
         axis.text.x =element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         legend.position="none")

### Black and white
ggplot (df, aes(x= index, y=value, color = variable), log = "y")+
  geom_line (size=1) +
  scale_colour_manual(values=c("grey90", "grey12", "grey45"))+
  geom_abline(intercept = 0, slope = 0)+ ##log scale
  scale_y_continuous(trans='log10', breaks=c(10,1,0.1,0.01,0.001, 0.0001, 0.00001, 0.000001, 0.0000001))+
  theme_classic()+
  theme (axis.title.x=element_blank(),
         axis.text.x =element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         legend.position="none")

### Dashed
ggplot (df, aes(x= index, y=value), log = "y")+
  geom_line (size=1, aes(linetype=variable)) +
#  scale_colour_manual(values=c("grey45", "grey90", "grey2"))+
  geom_abline(intercept = 0, slope = 0)+ ##log scale
  scale_y_continuous(trans='log10', breaks=c(10,1,0.1,0.01,0.001, 0.0001, 0.00001, 0.000001, 0.0000001))+
  theme_classic()+
  theme (axis.title.x=element_blank(),
         axis.text.x =element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         legend.position="none")



