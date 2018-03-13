library(shiny)
library(DiagrammeR)

packageVersion("base")
options(max.print=1000000000)

raw = read.csv("Data//Celtic SeaS Pressure Assessment PB 28_02_18.csv")


data = raw[!raw$Overlap == "NO", ]

# Only need the sectors, pressures, eco char and their scores
data = data[ , c(1:8)]
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



data$Sector = as.character(data$Sector)
data$Sector[data$Sector == "Non-renewable (oil & gas)"] <- "Non-renewable"


data$Links = paste("'", data$Sector, "'", " -> ", "'", data$Pressure, "'", " -> ", "'", data$Ecological.Characteristic, "'")

data = data[ , c(1:3, 14:17)]

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
    6
  }  else if (method == "Impact Risk"){
    4
  } else if (method == "Recovery Lag"){
    5
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
  
  data$Selected = ifelse (data$key %in% dataselected$key, 1 , 2)
  data = data[order (data$Selected), ]
  
  
  data$Colour = ifelse (data$key %in% dataselected$key, 
                        paste0 (data$Links, " [penwidth = 20, color = red]; ", sep = " "),
                        paste0 (data$Links, " [penwidth = 2, color = black];", sep = " "))  
  
  Links = subset(data, select = Colour)
  LinksAll <- capture.output(print(Links, row.names = FALSE))[-1]
  LinksAll2 <- paste(LinksAll,"", collapse= " " )
  
  ######################
  #### Which nodes should be highlighted
  
  SelectedNodes = if (nrow(data) != nrow(dataselected)){
    dataselected$Sector2 = paste("'", dataselected$Sector, "';")
    dataselected$Pressure2 = paste("'", dataselected$Pressure, "';")
    dataselected$Ecological.Characteristic2 = paste("'", dataselected$Ecological.Characteristic, "';")
    
    
    NodesSector = unique(subset(dataselected, select = Sector2))
    NodesPressure = unique(subset(dataselected, select = Pressure2))
    NodesEco = unique(subset(dataselected, select = Ecological.Characteristic2))
    
    NodesSector2 <- capture.output(print(NodesSector, row.names = FALSE))[-1]
    NodesPressure2 <- capture.output(print(NodesPressure, row.names = FALSE))[-1]
    NodesEco2 <- capture.output(print(NodesEco, row.names = FALSE))[-1]

    NodesSector3 <- paste(NodesSector2,"", collapse= " " )
    NodesPressure3 <- paste(NodesPressure2,"", collapse= " " )
    NodesEco3 <- paste(NodesEco2,"", collapse= " " )
    
    paste(NodesSector3, NodesPressure3, NodesEco3)
  } else { " "}
  
########## create object to make plot
  obj <- paste0("digraph{ 
                graph [bgcolor='white'; 
                overlap=true;
                ratio=auto; 
                rankdir=LR;
                concentrate=true]
                
                node [fontname=Helvetica,shape=box, fontsize =150, style=bold, style = filled, color = black, penwidth = 10,fillcolor = yellow]                
                {" ,SelectedNodes, " }
                
                node [fontname=Helvetica,shape=box, fontsize =150, style=bold, style = empty, color = black, penwidth = 10]
                
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

#write (obj, "H:\\ODEMM\\Analysis\\Network Plot\\Horrendogram\\objline.dot" )


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
      grVizOutput(outputId = "horrendogram", width = "95%", height = "700px")
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



