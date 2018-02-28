########################################################################
########################################################################
######     Dendrogram Plots             ################################
########################################################################
########################################################################

### Sectors by Pressure

SecPress = dcast (d, Sector ~ Pressure , value.var = "Count")

SecPress$Sector = NULL
SecPress = SecPress/SecPress 

SecPress = as.matrix(SecPress)
SecPress[is.nan(SecPress)] <- 0

SecPress.pv <- pvclust(SecPress, method.dist="euclidean", 
                       method.hclust="average", nboot=1000)

pdf ("Dendrogram Pressures by Sector.pdf")
plot(SecPress.pv, col.pv=c(2,3,4), main="", sub="", xlab="", ylab="", 
     float = 0.005, cex=0.8, cex.pv=0.6, adj=.7,axes=F)
axis(side=4, at=c(0,1,2,3), labels=c("0","1", "2", "3"),  
     mgp = c(0, 0.5, 0))
dev.off()


### Pressures by Sector

PressSec = dcast (d, Pressure ~ Sector, value.var = "Count")

PressSec$Pressure = NULL
PressSec = PressSec/PressSec 

PressSec = as.matrix(PressSec)
PressSec[is.nan(PressSec)] <- 0

PressSec.pv <- pvclust(PressSec, method.dist="euclidean", 
                       method.hclust="average", nboot=1000)

pdf ("Dendrogram Sectors by Pressure.pdf")
plot(PressSec.pv, col.pv=c(2,3,4), main="", sub="", xlab="", ylab="", 
     float = 0.005, cex=0.8, cex.pv=0.6, adj=.7,axes=F)
axis(side=4, at=c(0,1,2,3), labels=c("0","1", "2", "3"),  
     mgp = c(0, 0.5, 0))
dev.off()

### Eco Char by Sector

EcoSec = dcast (d, Sector ~ EcoChar, value.var = "Count")

EcoSec$Sector = NULL
EcoSec = EcoSec/EcoSec 

EcoSec = as.matrix(EcoSec)
EcoSec[is.nan(EcoSec)] <- 0

EcoSec.pv <- pvclust(EcoSec, method.dist="euclidean", 
                     method.hclust="average", nboot=1000)

pdf ("Dendrogram Eco Char by Sector.pdf")
plot(EcoSec.pv, col.pv=c(2,3,4), main="", sub="", xlab="", ylab="", 
     float = 0.005, cex=0.8, cex.pv=0.6, adj=.7,axes=F)
axis(side=4, at=c(0,1,2,3), labels=c("0","1", "2", "3"),  
     mgp = c(0, 0.5, 0))
dev.off()

### Eco Char by Pressure

EcoPress = dcast (d, Pressure  ~ EcoChar, value.var = "Count")

EcoPress$Pressure = NULL
EcoPress = EcoPress/EcoPress 

EcoPress = as.matrix(EcoPress)
EcoPress[is.nan(EcoPress)] <- 0

EcoPress.pv <- pvclust(EcoPress, method.dist="euclidean", 
                       method.hclust="average", nboot=1000)

pdf ("Dendrogram Eco Char by Pressure.pdf")
plot(EcoPress.pv, col.pv=c(2,3,4), main="", sub="", xlab="", ylab="", 
     float = 0.005, cex=0.8, cex.pv=0.6, adj=.7,axes=F)
axis(side=4, at=c(0,1,2,3), labels=c("0","1", "2", "3"),  
     mgp = c(0, 0.5, 0))
dev.off()
