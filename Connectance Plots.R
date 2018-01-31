########################################################################
########################################################################
######     Summary Plots             ###################################
########################################################################
########################################################################

BPIS$Sector = as.factor(BPIS$Sector)
BPIS$Pressure = as.factor(BPIS$Pressure)


d = ddply (BPIS, c("Sector", "Pressure", "EcoChar"), summarise,
           Count = length(ImpactRisk))

###########################################
#### Sectors
###########################################

SP = ddply (d, c("Sector", "Pressure"), summarise,
            Sum = sum(Count))

SE = ddply (d, c("Sector", "EcoChar"), summarise,
            Sum = sum(Count))

SecPress = ddply (SP, c("Sector"), summarise,
                  TotLinks = sum(Sum),
                  Pressures = length(Sum))

SecEco = ddply (SE, c("Sector"), summarise,
                Eco = length(Sum))

Sectors = merge (SecPress, SecEco)


Sectors$PercPress = (Sectors$Pressures / length(unique (BPIS$Pressure)))*100
Sectors$PercEco = (Sectors$Eco / length(unique (BPIS$EcoChar)))*100
Sectors$Connect = (Sectors$TotLinks/ (length (BPIS$ImpactRisk))) *100

Sectors = Sectors[order(Sectors$Connect, decreasing = TRUE), ]

write.csv (Sectors, "Data Summary Sectors.csv")

png ("Data Summary Sectors.png", width = 700, height = 700)
grid.table(Sectors)
dev.off()

## Plots

Sectors$Sector <- factor(Sectors$Sector, levels = Sectors$Sector[order(Sectors$Connect)])

ggplot (Sectors, aes (x = Sector, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()

pdf ("Connectance Sectors.pdf", width = 6, height = 8)
ggplot (Sectors, aes (x = Sector, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()
dev.off()

ggplot (Sectors, aes (x = Sector, y = PercPress))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Pressures Effected")+
  theme_bw() +
  coord_flip()

ggplot (Sectors, aes (x = Sector, y = PercEco))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Ecological Characteristics Effected")+
  theme_bw() +
  coord_flip()

#### Combine plots together
s1 = ggplot (Sectors, aes (x = Sector, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()

s2 = ggplot (Sectors, aes (x = Sector, y = PercPress))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Pressures Effected")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

s3 = ggplot (Sectors, aes (x = Sector, y = PercEco))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Ecological Characteristics Effected")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())

pdf ("Links Sectors.pdf", width = 12, height = 8)
grid.arrange(s1, s2, s3, ncol = 3, widths = c(10, 5,5))
dev.off()

###########################################
### Pressures
###########################################

PE = ddply (d, c("Pressure", "EcoChar"), summarise,
            Sum = sum(Count))

PressSec = ddply (SP, c("Pressure"), summarise,
                  TotLinks = sum(Sum),
                  Sectors = length(Sum))

PressEco = ddply (PE, c("Pressure"), summarise,
                  Eco = length(Sum))

Pressures = merge (PressSec, PressEco)


Pressures$PercSect = (Pressures$Sectors / length(unique (BPIS$Sector)))*100
Pressures$PercEco = (Pressures$Eco / length(unique (BPIS$EcoChar)))*100
Pressures$Connect = (Pressures$TotLinks/ (length (BPIS$ImpactRisk))) *100

Pressures = Pressures[order(Pressures$Connect, decreasing = TRUE), ]


write.csv (Pressures, "Data Summary Pressures.csv")

## Plots

Pressures$Pressure <- factor(Pressures$Pressure, levels = Pressures$Pressure[order(Pressures$Connect)])

ggplot (Pressures, aes (x = Pressure, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()

pdf ("Connectance Pressures.pdf", width = 6, height = 8)
ggplot (Pressures, aes (x = Pressure, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()
dev.off()

ggplot (Pressures, aes (x = Pressure, y = PercSect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Sectors Effected")+
  theme_bw() +
  coord_flip()

ggplot (Pressures, aes (x = Pressure, y = PercEco))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Ecological Characteristics Effected")+
  theme_bw() +
  coord_flip()

#### Combine plots together
p1 = ggplot (Pressures, aes (x = Pressure, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()

p2 = ggplot (Pressures, aes (x = Pressure, y = PercSect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Sectors")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p3 = ggplot (Pressures, aes (x = Pressure, y = PercEco))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Ecological Characteristics Effected")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())

pdf ("Links Pressures.pdf", width = 12, height = 8)
grid.arrange(p1, p2, p3, ncol = 3, widths = c(8, 5,5))
dev.off()

png ("Data Summary Pressures.png", width = 700, height = 700)
grid.table(Pressures)
dev.off()

###########################################
### Eco Char
###########################################

EcoSec = ddply (SE, c("EcoChar"), summarise,
                TotLinks = sum(Sum),
                Sectors = length(Sum))

EcoPress = ddply (PE, c("EcoChar"), summarise,
                  Pressure = length(Sum))

Eco = merge (EcoSec, EcoPress)


Eco$PercSect = (Eco$Sectors / length(unique (BPIS$Sector)))*100
Eco$PercPress = (Eco$Pressure / length(unique (BPIS$Pressure)))*100
Eco$Connect = (Eco$TotLinks/ (length (BPIS$ImpactRisk))) *100

Eco = Eco[order(Eco$Connect, decreasing = TRUE), ]

write.csv (Eco, "Data Summary Eco Char.csv")

png ("Data Summary Eco Char.png", width = 700, height = 700)
grid.table(Eco)
dev.off()

## Plots

Eco$EcoChar <- factor(Eco$EcoChar, levels = Eco$EcoChar[order(Eco$Connect)])

ggplot (Eco, aes (x = EcoChar, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  xlab ("Ecological Characteristic")+
  theme_bw() +
  coord_flip()

pdf ("Connectance Eco Char.pdf", width = 6, height = 8)
ggplot (Eco, aes (x = EcoChar, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  xlab ("Ecological Characteristic")+
  theme_bw() +
  coord_flip()
dev.off ()

ggplot (Eco, aes (x = EcoChar, y = PercSect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Sectors")+
  xlab ("Ecological Characteristic")+
  theme_bw() +
  coord_flip()

ggplot (Eco, aes (x = EcoChar, y = PercPress))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Pressures")+
  xlab ("Ecological Characteristic")+
  theme_bw() +
  coord_flip()

#### Combine plots together
e1 = ggplot (Eco, aes (x = EcoChar, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  xlab ("Ecological Characteristic")+
  theme_bw() +
  coord_flip()

e2 = ggplot (Eco, aes (x = EcoChar, y = PercSect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Sectors")+
  xlab ("Ecological Characteristic")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

e3 = ggplot (Eco, aes (x = EcoChar, y = PercPress))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Pressures")+
  xlab ("Ecological Characteristic")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())

pdf ("Links EcoChar.pdf", width = 12, height = 8)
grid.arrange(e1, e2, e3, ncol = 3, widths = c(8, 5,5))
dev.off()

