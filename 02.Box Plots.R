########################################################################
########################################################################
######     Box Plots             #######################################
########################################################################
########################################################################

### Creates box plots of the sectors, pressures and ecological characteristics
##' First use code for data input (01. Data Input and Initial Analysis.R)

## defines the order of the categories you are plotting - only needed for EcoChar
BPIS$EcoChar<-factor(BPIS$EcoChar,
                     levels=c( "Littoral Rock & Reef", "Littoral Sediment", "Shallow Rock & Reef", 
                               "Shallow Sediment", "Shallow Mud", "Shelf Rock & Reef", "Shelf Sediment",
                               "Shelf Pelagic", "Coastal Pelagic", "Oceanic Pelagic",
                               "Slope Rock & Reef", "Slope Sediment",
                               "Bathyal Rock & Reef", "Bathyal Sediment",
                               "Abyssal Rock & Reef", "Abyssal Sediment",
                               "Toothed Whales", "Baleen Whales", "Seals", "Seabirds", "Reptiles", "Cephalopods", 
                               "Demersal Fish", "Demersal Elasmo", "Pelagic Fish", "Pelagic Elasmo",
                               "Deep Sea Fish", "Deep Sea Elasmo"))

### Pressures

# pdf ("Box Plot Pressures.pdf", width = 12, height = 8)
par(mfrow = c(1, 3), oma = c(2, 16, 2 ,2), mar = c(5,0,1,0))

boxplot(BPIS$ImpactRisk ~ BPIS$Pressure, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(28), xlab="Impact Risk", cex.axis=1.4, cex.lab = 1.4,
        at = rev(seq(1, nlevels(BPIS$Pressure), 1)))

boxplot(BPIS$LN.IR ~ BPIS$Pressure, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(28), xlab="Impact Rank", cex.axis=1.4, cex.lab = 1.4,
        yaxt = "n",
        at = rev(seq(1, nlevels(BPIS$Pressure), 1)))

boxplot(BPIS$Ryr ~ BPIS$Pressure, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(28), xlab="Recovery (Years)", cex.axis=1.4, cex.lab = 1.4,
        yaxt = "n",
        at = rev(seq(1, nlevels(BPIS$Pressure), 1)))
# dev.off()


#### Eco Char

#pdf ("Box Plot Eco Char.pdf", width = 12, height = 8)

par(mfrow = c(1, 3), oma = c(2, 13, 2 ,2), mar = c(5,0,1,0))

boxplot(BPIS$ImpactRisk ~ BPIS$EcoChar, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(28), xlab="Impact Risk", cex.axis=1.4, cex.lab = 1.4,
        at = rev(seq(1, nlevels(BPIS$EcoChar), 1)))

boxplot(BPIS$LN.IR ~ BPIS$EcoChar, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(28), xlab="Impact Rank", cex.axis=1.4, cex.lab = 1.4,
        yaxt = "n",
        at = rev(seq(1, nlevels(BPIS$EcoChar), 1)))

boxplot(BPIS$Ryr ~ BPIS$EcoChar, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(28), xlab="Recovery (Years)", cex.axis=1.4, cex.lab = 1.4,
        yaxt = "n",
        at = rev(seq(1, nlevels(BPIS$EcoChar), 1)))
#dev.off()

#### Sectors

#pdf ("Outputs\\Box Plot Sectors.pdf", width = 12, height = 8)

par(mfrow = c(1, 3), oma = c(2, 15, 2 ,2), mar = c(5,0,1,0))

boxplot(BPIS$ImpactRisk ~ BPIS$Sector, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(28), xlab="Impact Risk", cex.axis=1.4, cex.lab = 1.4,
        at = rev(seq(1, nlevels(BPIS$Sector), 1)))

boxplot(BPIS$LN.IR ~ BPIS$Sector, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(28), xlab="Impact Rank", cex.axis=1.4, cex.lab = 1.4,
        yaxt = "n",
        at = rev(seq(1, nlevels(BPIS$Sector), 1)))

boxplot(BPIS$Ryr ~ BPIS$Sector, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(28), xlab="Recovery (Years)", cex.axis=1.4, cex.lab = 1.4,
        yaxt = "n",
        at = rev(seq(1, nlevels(BPIS$Sector), 1)))
#dev.off ()
