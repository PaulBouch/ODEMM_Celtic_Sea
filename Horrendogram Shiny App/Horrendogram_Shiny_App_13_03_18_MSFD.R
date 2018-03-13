library(shiny)
library(DiagrammeR)

packageVersion("base")
options(max.print=10000000)

raw = read.csv("Data//Celtic SeaS Pressure Assessment PB 28_02_18.csv")

raw = raw[!raw$Overlap == "NO", ]

raw$Sector = as.character(raw$Sector)
raw$Sector[raw$Sector == "Non-renewable (oil & gas)"] <- "Non-renewable"

######################################
###   MSFD Eco and Pressures
######################################
D1.Eco = c("Shallow Sediment", "Shallow Rock & Reef","Littoral Sediment","Littoral Rock & Reef","Demersal Elasmo",
           "Cephalopods","Demersal Fish","Pelagic Fish","Seabirds","Seals","Toothed Whales","Pelagic Elasmo","Coastal Pelagic",
           "Shallow Mud","Baleen Whales","Reptiles","Deep Sea Elasmo","Deep Sea Fish","Shelf Sediment","Shelf Rock & Reef",
           "Bathyal Sediment","Bathyal Rock & Reef","Slope Sediment","Slope Rock & Reef","Oceanic Pelagic","Shelf Pelagic",
           "Abyssal Sediment","Abyssal Rock & Reef")

D1.Pressures = c("Abrasion", "Current Changes", "Sealing","Siltation", "Incidental Loss", "Nitrogen & Phosphorus",
                 "Non-synthetic Compounds",  "Organic Matter","pH Changes", "Synthetic Compounds",
                 "Invasive Species", "Smothering","Species Extraction","Wave Exposure","Barriers",
                 "Emergence Regime", "Salinity Regime", "Bycatch", "Thermal Regime",  "EMF") 

raw$D1 [raw$Pressure %in%  D1.Pressures & raw$Ecological.Characteristic %in% D1.Eco] = "'D1. Biological Diversity'"
######################################
D2.Eco = c("Shallow Sediment", "Shallow Rock & Reef","Littoral Sediment","Littoral Rock & Reef","Demersal Elasmo",
           "Cephalopods","Demersal Fish","Pelagic Fish","Pelagic Elasmo","Coastal Pelagic",
           "Shallow Mud","Deep Sea Elasmo","Deep Sea Fish","Shelf Sediment","Shelf Rock & Reef",
           "Bathyal Sediment","Bathyal Rock & Reef","Slope Sediment","Slope Rock & Reef","Oceanic Pelagic","Shelf Pelagic",
           "Abyssal Sediment","Abyssal Rock & Reef")

D2.Pressures = c("Invasive Species")

raw$D2 [raw$Pressure %in%  D2.Pressures & raw$Ecological.Characteristic %in% D2.Eco] = "'D2. Non-indigenous Species'"
#####################################
D3.Eco = c("Shallow Sediment", "Shallow Rock & Reef","Littoral Sediment","Littoral Rock & Reef","Demersal Elasmo",
           "Cephalopods","Demersal Fish","Pelagic Fish","Seabirds","Seals","Toothed Whales","Pelagic Elasmo","Coastal Pelagic",
           "Shallow Mud","Baleen Whales","Reptiles","Deep Sea Elasmo","Deep Sea Fish","Shelf Sediment","Shelf Rock & Reef",
           "Bathyal Sediment","Bathyal Rock & Reef","Slope Sediment","Slope Rock & Reef","Oceanic Pelagic","Shelf Pelagic",
           "Abyssal Sediment","Abyssal Rock & Reef")

D3.Pressures = c("Incidental Loss", "Invasive Species", "Species Extraction","Bycatch") 

raw$D3 [raw$Pressure %in%  D3.Pressures & raw$Ecological.Characteristic %in% D3.Eco] = "'D3. Commercial Fishing'"
####################################
D4.Eco = c("Shallow Sediment", "Shallow Rock & Reef","Littoral Sediment","Littoral Rock & Reef","Demersal Elasmo",
           "Cephalopods","Demersal Fish","Pelagic Fish","Seabirds","Seals","Toothed Whales","Pelagic Elasmo","Coastal Pelagic",
           "Shallow Mud","Baleen Whales","Reptiles","Deep Sea Elasmo","Deep Sea Fish","Shelf Sediment","Shelf Rock & Reef",
           "Bathyal Sediment","Bathyal Rock & Reef","Slope Sediment","Slope Rock & Reef","Oceanic Pelagic","Shelf Pelagic",
           "Abyssal Sediment","Abyssal Rock & Reef")

D4.Pressures = c("Nitrogen & Phosphorus", "Organic Matter", "Invasive Species", "Species Extraction","Bycatch") 

raw$D4 [raw$Pressure %in%  D4.Pressures & raw$Ecological.Characteristic %in% D4.Eco] = "'D4. Food Webs'"
####################################
D5.Eco = c("Littoral Rock & Reef", "Littoral Sediment", "Shallow Rock & Reef", "Shallow Sediment", "Shallow Mud")

D5.Pressures = as.list(c("Nitrogen & Phosphorus", "Organic Matter")) 

raw$D5 [raw$Pressure %in%  D5.Pressures & raw$Ecological.Characteristic %in% D5.Eco] = "'D5. Eutrophication'"
####################################
D6.Eco = c("Shallow Sediment", "Shallow Rock & Reef","Littoral Sediment","Littoral Rock & Reef",
           "Shallow Mud","Shelf Sediment","Shelf Rock & Reef", "Bathyal Sediment","Bathyal Rock & Reef",
           "Slope Sediment","Slope Rock & Reef", "Abyssal Sediment","Abyssal Rock & Reef")

D6.Pressures = c("Incidental Loss", "Invasive Species", "Species Extraction","Bycatch", "Sealing","Siltation",
                 "Smothering", "Abrasion", "Non-living Resources") 

raw$D6 [raw$Pressure %in%  D6.Pressures & raw$Ecological.Characteristic %in% D6.Eco] = "'D6. Sea-floor Integrity'"
####################################
D7.Eco = c("Littoral Rock & Reef", "Littoral Sediment", "Shallow Rock & Reef", "Shallow Sediment", 
           "Shallow Mud", "Coastal Pelagic")

D7.Pressures = c("Emergence Regime", "Wave Exposure", "Current Changes")

raw$D7 [raw$Pressure %in%  D7.Pressures & raw$Ecological.Characteristic %in% D7.Eco] = "'D7. Hydrographical Conditions'"
####################################
D8.Eco = c("Shallow Sediment", "Shallow Rock & Reef","Littoral Sediment","Littoral Rock & Reef","Demersal Elasmo",
           "Cephalopods","Demersal Fish","Pelagic Fish","Seabirds","Seals","Toothed Whales","Pelagic Elasmo","Coastal Pelagic",
           "Shallow Mud","Baleen Whales","Deep Sea Elasmo","Deep Sea Fish","Shelf Sediment","Shelf Rock & Reef",
           "Bathyal Sediment","Bathyal Rock & Reef","Slope Sediment","Slope Rock & Reef","Oceanic Pelagic","Shelf Pelagic",
           "Abyssal Sediment","Abyssal Rock & Reef")

D8.Pressures = c("Non-synthetic Compounds", "Synthetic Compounds")

raw$D8 [raw$Pressure %in%  D8.Pressures & raw$Ecological.Characteristic %in% D8.Eco] = "'D8. Contaminants'"
####################################
D9.Eco = c("Littoral Rock & Reef", "Littoral Sediment", "Shallow Rock & Reef", "Shallow Sediment", 
           "Shallow Mud", "Shelf Sediment","Shelf Rock & Reef", "Pelagic Fish", "Demersal Fish",
           "Deep Sea Elasmo","Deep Sea Fish", "Pelagic Elasmo", "Demersal Elasmo", "Cephalopods")

D9.Pressures = c("Non-synthetic Compounds", "Synthetic Compounds", "Invasive Species")

raw$D9 [raw$Pressure %in%  D9.Pressures & raw$Ecological.Characteristic %in% D9.Eco] = "'D9. Contaminants in Seafood'"
####################################
D10.Eco = c("Shallow Sediment", "Shallow Rock & Reef","Littoral Sediment","Littoral Rock & Reef","Demersal Elasmo",
            "Cephalopods","Demersal Fish","Pelagic Fish","Seabirds","Seals","Toothed Whales","Pelagic Elasmo","Coastal Pelagic",
            "Shallow Mud","Baleen Whales","Reptiles","Deep Sea Elasmo","Deep Sea Fish","Shelf Sediment","Shelf Rock & Reef",
            "Bathyal Sediment","Bathyal Rock & Reef","Slope Sediment","Slope Rock & Reef","Oceanic Pelagic","Shelf Pelagic",
            "Abyssal Sediment","Abyssal Rock & Reef")

D10.Pressures = c("Litter")

raw$D10 [raw$Pressure %in%  D10.Pressures & raw$Ecological.Characteristic %in% D10.Eco] = "'D10. Marine Litter'"
####################################
D11.Eco = c("Shallow Sediment", "Shallow Rock & Reef","Littoral Sediment","Littoral Rock & Reef","Demersal Elasmo",
            "Cephalopods","Demersal Fish","Pelagic Fish","Seabirds","Seals","Toothed Whales","Pelagic Elasmo","Coastal Pelagic",
            "Shallow Mud","Baleen Whales","Reptiles","Deep Sea Elasmo","Deep Sea Fish","Shelf Sediment","Shelf Rock & Reef",
            "Bathyal Sediment","Bathyal Rock & Reef","Slope Sediment","Slope Rock & Reef","Oceanic Pelagic","Shelf Pelagic",
            "Abyssal Sediment","Abyssal Rock & Reef")

D11.Pressures = c("Noise")

raw$D11 [raw$Pressure %in%  D11.Pressures & raw$Ecological.Characteristic %in% D11.Eco] = "'D11. Underwater Noise'"

### Put all the descriptors into one column, so we can add them on to the Linkage chains
raw$Descriptors [raw$D1 == "'D1. Biological Diversity'"]  =  "'D1. Biological Diversity'"

raw$Descriptors = ifelse (is.na(raw$D2),  raw$Descriptors, 
                          ifelse(is.na(raw$Descriptors),"'D2. Non-indigenous Species'",paste(raw$Descriptors, "; 'D2. Non-indigenous Species'" ))) 

raw$Descriptors = ifelse (is.na(raw$D3),  raw$Descriptors,
                          ifelse(is.na(raw$Descriptors),"'D3. Commercial Fishing'",paste(raw$Descriptors, "; 'D3. Commercial Fishing'" )))

raw$Descriptors = ifelse (is.na(raw$D4),  raw$Descriptors,
                          ifelse(is.na(raw$Descriptors),"'D4. Food Webs'",paste(raw$Descriptors, "; 'D4. Food Webs'" )))

raw$Descriptors = ifelse (is.na(raw$D5),  raw$Descriptors,
                          ifelse(is.na(raw$Descriptors),"'D5.Eutrophication'",paste(raw$Descriptors, "; 'D5. Eutrophication'" )))

raw$Descriptors = ifelse (is.na(raw$D6),  raw$Descriptors,
                          ifelse(is.na(raw$Descriptors),"'D6. Sea-floor Integrity'", paste(raw$Descriptors, "; 'D6. Sea-floor Integrity'" )))

raw$Descriptors = ifelse (is.na(raw$D7),  raw$Descriptors,
                          ifelse(is.na(raw$Descriptors),"'D7. Hydrographical Conditions'",paste(raw$Descriptors, "; 'D7. Hydrographical Conditions'" )))

raw$Descriptors = ifelse (is.na(raw$D8),  raw$Descriptors,
                          ifelse(is.na(raw$Descriptors),"'D8. Contaminants'",paste(raw$Descriptors, "; 'D8. Contaminants'" )))

raw$Descriptors = ifelse (is.na(raw$D9),  raw$Descriptors,
                          ifelse(is.na(raw$Descriptors),"'D9. Contaminants in Seafood'",paste(raw$Descriptors, "; 'D9. Contaminants in Seafoods'" )))

raw$Descriptors = ifelse (is.na(raw$D10),  raw$Descriptors,
                          ifelse(is.na(raw$Descriptors),"'D10. Marine Litter'",paste(raw$Descriptors, "; 'D10. Marine Litter'" )))

raw$Descriptors = ifelse (is.na(raw$D11),  raw$Descriptors,
                          ifelse(is.na(raw$Descriptors),"'D11. Underwater Noise'",paste(raw$Descriptors, "; 'D11. Underwater Noise'" )))



raw$Descriptors = ifelse (is.na(raw$Descriptors), "", paste0(" -> {", raw$Descriptors, "}"))



raw$Links = paste("'", raw$Sector, "'", " -> ", "'", raw$Pressure, "'", " -> ", "'", raw$Ecological.Characteristic, "'" , raw$Descriptors )

data = raw[ , c(1:8,32)]

# add columns with the values asscociated with the clasifactions
# score each rating according to Knight et al 2015
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



### Calculate Impact Risk, Recovery Lag and Total Risk
data$ImpactRisk = data$Overlap.Score*data$Frequency.Score*data$DoI.Score
data$RecoveryLag = data$Resilience.Score*data$Persistence.Score

data$TotalRisk = data$ImpactRisk * data$RecoveryLag

### Column to look for unique chains
data$key = paste0(data$Sector, data$Pressure, data$Ecological.Characteristic)


########################################################################
####   Function to produce plot


graph_obj <- function(data, InSector, InPressure, InEco, method, percent){
  
  # For testing
  # InSector = "Fishing"
  # percent = "All"
  # method = "Total Risk"
  # InPressure = "All Pressures"
  # InEco = "All"
  
  ### Switch between different ways of selecting the top risks.
  ### The number selected is the column in the data set corresponding to that metric   
  MethodColumn = if (method == "Total Risk"){
    17
  }  else if (method == "Impact Risk"){
    15
  } else if (method == "Recovery Lag"){
    16
  }
  
  ### Switch between different percentage linkages
  ### What number do we need to divide by to get that percentage of links
  PercentFactor =   if (percent == "All"){
    1
  }  else if (percent == "10%"){
    10
  } else if (percent == "20%"){
    5
  } else if (percent == "50%"){
    2
  }    
  
  ### Function to perform the calculations of the linkage chains once the correct data set is selected
  RiskFilter = function (dataset){
    AllRisk = sum (dataset[ , MethodColumn])
    dataset$relRisk = dataset[ , MethodColumn]/AllRisk
    dataset = dataset[order (-dataset$relRisk), ]
    dataset = dataset [1:(nrow(dataset)/PercentFactor), ]
    subset(dataset) 
  }
  
  data1 = data
  
  ### For the highlighted linkages (those that will be red), filter depending on selection boxes
  dataselected = if (InSector == "All Sectors" & InPressure == "All Pressures" & InEco == "All") {
    RiskFilter(data1)
  } else if (InSector != "All Sectors" & InPressure == "All Pressures" & InEco == "All") {
    data1 = subset (data1, data1$Sector == InSector)
    RiskFilter(data1)
  } else if (InSector != "All Sectors" & InPressure != "All Pressures" & InEco == "All") {
    data1 = subset (data1, data1$Sector == InSector & data1$Pressure == InPressure)
    RiskFilter(data1)
  } else if (InSector != "All Sectors" & InPressure != "All Pressures" & InEco != "All") {
    data1 = subset (data1, data1$Sector == InSector & data1$Pressure == InPressure & data1$Ecological.Characteristic == InEco)
    RiskFilter(data1)
  } else if (InSector != "All Sectors" & InPressure == "All Pressures" & InEco != "All") {
    data1 = subset (data1, data1$Sector == InSector & data1$Ecological.Characteristic == InEco)
    RiskFilter(data1)
  } else if (InSector == "All Sectors" & InPressure != "All Pressures" & InEco != "All") {
    data1 = subset (data1, data1$Pressure == InPressure & data1$Ecological.Characteristic == InEco)
    RiskFilter(data1)
  } else if (InSector == "All Sectors" & InPressure == "All Pressures" & InEco != "All") {
    data1 = subset (data1, data1$Ecological.Characteristic == InEco)
    RiskFilter(data1)
  } else if (InSector == "All Sectors" & InPressure != "All Pressures" & InEco == "All") {
    data1 = subset (data1, data1$Pressure == InPressure)
    RiskFilter(data1)
  }
  
  ## change the order so that selected rows are first in the dataset. They are therefore drawn first    
  data$Selected = ifelse (data$key %in% dataselected$key, 1 , 2)
  data = data[order (data$Selected), ]
  
  ### is that chain selected? If so, it will be red and thick  
  data$Colour = ifelse (data$key %in% dataselected$key, 
                        paste0 (data$Links, " [penwidth = 20, color = red]; ", sep = " "),
                        paste0 (data$Links, " [penwidth = 1, color = black];", sep = " "))  
  
  
  Links = subset(data, select = Colour)
  LinksAll <- capture.output(print(Links, row.names = FALSE))[-1]
  LinksAll2 <- paste(LinksAll,"", collapse= " " )
  
  
  
  obj <- paste0("digraph{ 
                graph [bgcolor='white'; 
                overlap=true;
                ratio=auto; 
                rankdir=LR;
                concentrate=true]
                
                node [color=Black,fontname=Helvetica,shape=box, fontsize =150, style=bold]
                
                subgraph habitats {' Abyssal Sediment '; ' Abyssal Rock & Reef '; ' Bathyal Sediment ';' Bathyal Rock & Reef ';
                ' Slope Sediment ';' Slope Rock & Reef ';' Shelf Sediment ';' Shelf Rock & Reef ';' Shallow Mud ';' Shallow Sediment ';' Shallow Rock & Reef ';
                ' Littoral Sediment ';' Littoral Rock & Reef ' ; ' Oceanic Pelagic ';' Shelf Pelagic ';' Coastal Pelagic '};
                
                subgraph benthic {' Abyssal Sediment '; ' Abyssal Rock & Reef '; ' Bathyal Sediment ';' Bathyal Rock & Reef ';
                ' Slope Sediment ';' Slope Rock & Reef ';' Shelf Sediment ';' Shelf Rock & Reef ';' Shallow Mud ';' Shallow Sediment ';' Shallow Rock & Reef ';
                ' Littoral Sediment ';' Littoral Rock & Reef ' };
                
                subgraph pelagic {' Oceanic Pelagic ';' Shelf Pelagic ';' Coastal Pelagic ' }
                
                subgraph abyssal {' Abyssal Sediment '; ' Abyssal Rock & Reef '}          
                
                subgraph bathyal {' Bathyal Sediment '; ' Bathyal Rock & Reef '}   
                
                subgraph deepsea {' Abyssal Sediment '; ' Abyssal Rock & Reef ', ' Bathyal Sediment '; ' Bathyal Rock & Reef '} 
                
                subgraph littoral {' Littoral Sediment ';' Littoral Rock & Reef '}  
                
                subgraph shallow {' Shallow Mud ';' Shallow Sediment ';' Shallow Rock & Reef '}  
                
                subgraph shelf {' Shelf Sediment ';' Shelf Rock & Reef '}
                
                subgraph slope {' Slope Sediment ';' Slope Rock & Reef '}
                
                subgraph inshore { ' Littoral Sediment ';' Littoral Rock & Reef '; ' Shallow Mud ';
                ' Shallow Sediment ';' Shallow Rock & Reef '; ' Shelf Sediment ';' Shelf Rock & Reef ' }
                
                subgraph beasts {' Deep Sea Fish ';' Demersal Fish ';' Pelagic Fish ';
                ' Deep Sea Elasmo ';' Demersal Elasmo ';' Pelagic Elasmo '; ' Cephalopods ';  ' Reptiles '; ' Seals '; ' Seabirds ';
                ' Toothed Whales '; ' Baleen Whales '}     
                
                subgraph fish {' Deep Sea Fish ';' Demersal Fish ';' Pelagic Fish ';
                ' Deep Sea Elasmo ';' Demersal Elasmo ';' Pelagic Elasmo ' }  
                
                subgraph mammals { ' Seals '; ' Toothed Whales '; ' Baleen Whales '}
                
                subgraph whales { ' Toothed Whales '; ' Baleen Whales '}
                
                subgraph fish {' Deep Sea Fish ';' Demersal Fish ';' Pelagic Fish '}
                
                subgraph elasmo {' Deep Sea Elasmo ';' Demersal Elasmo ';' Pelagic Elasmo ' }
                
                
                nodesep=1.5 // increases the separation between nodes
                ranksep= 25
                
                edge [arrowhead = none]
                ",LinksAll2,"
}", sep= " ")
  
  
  }

write (obj, "H:\\ODEMM\\Analysis\\Network Plot\\Horrendogram\\objmsfd.dot" )


## ui.R ----
# Step 1:
# Define UI for app  ----
ui <- fluidPage(
  # App title ----
  titlePanel("Celtic Seas ODEMM"),
  
  # Top Selection with input and output definitions ----
  fluidRow(
    column(3,offset = 1,
           # Input: dropdown for each category ----
           selectInput(inputId = "Sector",
                       label = "Choose a 
                       sector:",
                       c('All Sectors'='All Sectors',
                         'Aggregates'='Aggregates',
                         'Agriculture'='Agriculture',
                         'Aquaculture'='Aquaculture',
                         'Coastal Infrastructure'='Coastal Infrastructure' ,
                         'Fishing'='Fishing',
                         'Harvesting/Collecting'='Harvesting/Collecting',
                         'Land-based Industry'='Land-based Industry',
                         'Military'= 'Military',
                         'Navigational Dredging'='Navigational Dredging',
                         'Non-renewable'='Non-renewable',
                         'Renewable Energy'='Renewable Energy',
                         'Research'='Research',
                         'Shipping'='Shipping',
                         'Telecommunications'='Telecommunications',
                         'Tourism/Recreation'='Tourism/Recreation',
                         'Waste Water'='Waste Water'))),
    
    column(3,offset = 0.5,
           # Input: dropdown for each category ----
           selectInput(inputId = "Pressure",
                       label = "Choose a 
                       pressure:",
                       c('All Pressures'='All Pressures',
                         'Abrasion'='Abrasion',
                         'Barriers'='Barriers',
                         'Bycatch'='Bycatch',
                         'Current Changes'='Current Changes' ,
                         'Emergence Regime'='Emergence Regime',
                         'EMF'='EMF',
                         'Incidental Loss'='Incidental Loss',
                         'Invasive Species'= 'Invasive Species',
                         'Litter'='Litter',
                         'Nitrogen & Phosphorus'='Nitrogen & Phosphorus',
                         'Noise'='Noise',
                         'Non-living Resources'='Non-living Resources',
                         'Non-synthetic Compounds'='Non-synthetic Compounds',
                         'Organic Matter'='Organic Matter',
                         'pH Changes'='pH Changes',
                         'Salinity Regime'='Salinity Regime',
                         'Sealing'='Sealing',
                         'Siltation'='Siltation',
                         'Smothering'='Smothering',
                         'Species Extraction'='Species Extraction',
                         'Synthetic Compounds'='Synthetic Compounds',
                         'Thermal Regime'='Thermal Regime',
                         'Wave Exposure'='Wave Exposure'))),
    
    column(3,offset = 0.5,
           # Input: dropdown for each category ----
           selectInput(inputId = "Ecological",
                       label = "Ecological characteristic:",
                       c('All'='All',
                         'Abyssal Rock & Reef'='Abyssal Rock & Reef',
                         'Abyssal Sediment'='Abyssal Sediment',
                         'Baleen Whales'='Baleen Whales',
                         'Bathyal Rock & Reef'='Bathyal Rock & Reef' ,
                         'Bathyal Sediment'='Bathyal Sediment',
                         'Cephalopods'='Cephalopods',
                         'Coastal Pelagic'='Coastal Pelagic',
                         'Deep Sea Elasmo'= 'Deep Sea Elasmo',
                         'Deep Sea Fish'='Deep Sea Fish',
                         'Demersal Elasmo'='Demersal Elasmo',
                         'Demersal Fish'='Demersal Fish',
                         'Littoral Rock & Reef'='Littoral Rock & Reef',
                         'Littoral Sediment'='Littoral Sediment',
                         'Oceanic Pelagic'='Oceanic Pelagic',
                         'Pelagic Elasmo'='Pelagic Elasmo',
                         'Pelagic Fish'='Pelagic Fish',
                         'Reptiles'='Reptiles',
                         'Seabirds'='Seabirds',
                         'Seals'='Seals',
                         'Shallow Mud'='Shallow Mud',
                         'Shallow Rock & Reef'='Shallow Rock & Reef',
                         'Shallow Sediment'='Shallow Sediment',
                         'Shelf Pelagic'='Shelf Pelagic',
                         'Shelf Rock & Reef'='Shelf Rock & Reef',
                         'Shelf Sediment'='Shelf Sediment',
                         'Slope Rock & Reef'='Slope Rock & Reef',
                         'Slope Sediment'='Slope Sediment' ,
                         'Toothed Whales'='Toothed Whales'))),
    
    column(3, offset = 1,
           selectInput(inputId = "Method",
                       label = "Risk Assessment:",
                       c('Total Risk' = "Total Risk",
                         'Impact Risk' = 'Impact Risk',
                         'Recovery Lag' = 'Recovery Lag'))),
    
    column(3, offset = 0.5,
           selectInput(inputId = "Percent",
                       label = "Top Percentage of Links:",
                       c('All' = "All",
                         '10%' = '10%',
                         '20%' = '20%',
                         '50%' = '50%'))),
    
    
    # Main panel for displaying outputs ----
    fluidRow(
      
      # Output: horrendogram ----
      grVizOutput(outputId = "horrendogram", width = "100%", height = "700px")
    )
  ))

### server.R ---
# Step 2:
# Define plot information (turn inputs into outputs)  ----
server <- function (input, output){
  
  output$horrendogram <- renderGrViz(
    grViz(
      graph_obj(data, input$Sector, input$Pressure, input$Ecological, input$Method, input$Percent)
    )
  )
  
}

# Step 3: 
# knit UI and server together

shinyApp (ui =ui, server = server)
