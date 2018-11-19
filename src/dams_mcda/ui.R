####################################################################################################################################################
###Dam Decision-Making Criteria & Alternatives

###Developer: Emma Fox (Ph.D. student,University of Maine)                                                                   
###emma.fox@maine.edu                                                                 
####################################################################################################################################################

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#


setwd("~/Beatrice2/R_ELF/R_NEST/MCDA_App_Shiny/MCDA_11132018/WSM_Tool")

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for Shiny web application 
shinyUI(fluidPage(
  
  titlePanel("A Watershed-Scale Dam Decision Making Tool"),
  #h5(
  # HTML("<b>Created by: Emma Fox</b>"),
  
  #HTML("<br>Shiny app code available on GITHUB for download: https://github.com/elbfox?tab=repositories")
  #),  
  navlistPanel(
    #Define layout
    widths = c(4,8),
    
    #Define Instructions tab
    tabPanel("Multi-Criteria Decision Analysis",
             textOutput("Introduction"),
             textOutput("Citations")),  
    
    
    tabPanel("Alternative 1: Dam Removal",
             HTML("Indicate your level of preference associated with each of the following criteria in the case of dam removal.<br>
                  <br>In each case, 1 = not at all important and 5 = extremely important.<br>"),
             #Fish Biomass
             sliderInput(inputId = "FishBiomass1", label = "Please rate the importance of fish biomass:", value=3, min=1, max=5, step = 0.25),
             #River Recreation
             sliderInput(inputId = "RiverRec1", label = "Please rate the importance of River Recreation:", value=3, min=1, max=5, step = 0.25),
             #Reservoir Storage
             sliderInput(inputId = "Reservoir1", label = "Please rate the importance of Reservoir Storage:", value=3, min=1, max=5, step = 0.25),
             #One-Time Project Costs
             sliderInput(inputId = "ProjCost1", label = "Please rate the importance of One-Time Project Costs:", value=3, min=1, max=5, step = 0.25),
             #Dam Safety
             sliderInput(inputId = "Safety1", label = "Please rate the importance of Dam Safety:", value=3, min=1, max=5, step = 0.25),
             #Number of Properties
             sliderInput(inputId = "NumProperties1", label = "Please rate the importance of Number of Properties Impacted:", value=3, min=1, max=5, step = 0.25),
             #Hydropower Capacity
             sliderInput(inputId = "HydroCapacity1", label = "Please rate the importance of Hydropower Capacity:", value=3, min=1, max=5, step = 0.25),
             
             actionButton("updateBtn", "Update"),
             
             tableOutput("SummTable1"), 
             plotOutput("SummPlot1")), #THIS DOESN'T EXIST YET###
    
    #End Alternative 1: Dam Removal Tab
    
    tabPanel("Alternative 2: Improve Fish Passage Facilities", 
             HTML("Indicate your level of preference associated with each of the following criteria in the case of improvements to fish passage facilities.<br>
                  <br> In each case, 1 = not at all important and 5 = extremely important.<br>"),
             #Fish Biomass
             sliderInput(inputId = "FishBiomass2", label = "Please rate the importance of fish biomass:", value=3, min=1, max=5, step = 0.25),
             #River Recreation
             sliderInput(inputId = "RiverRec2", label = "Please rate the importance of River Recreation:", value=3, min=1, max=5, step = 0.25),
             #Reservoir Storage
             sliderInput(inputId = "Reservoir2", label = "Please rate the importance of Reservoir Storage:", value=3, min=1, max=5, step = 0.25),
             #One-Time Project Costs
             sliderInput(inputId = "ProjCost2", label = "Please rate the importance of One-Time Project Costs:", value=3, min=1, max=5, step = 0.25),
             #Dam Safety
             sliderInput(inputId = "Safety2", label = "Please rate the importance of Dam Safety:", value=3, min=1, max=5, step = 0.25),
             #Number of Properties
             sliderInput(inputId = "NumProperties2", label = "Please rate the importance of Number of Properties Impacted:", value=3, min=1, max=5, step = 0.25),
             #Hydropower Capacity
             sliderInput(inputId = "HydroCapacity2", label = "Please rate the importance of Hydropower Capacity:", value=3, min=1, max=5, step = 0.25),
             
             actionButton("updateBtn", "Update"),
             
             tableOutput("SummTable2"), 
             plotOutput("SummPlot2")), #End main panel
    
    #End Alternative 2: Fish Passage Facility Improvements Tab
    
    tabPanel("Alternative 3: Upgrade or Replace Turbines at Existing Powered Dams", 
             HTML("Indicate your level of preference associated with each of the following criteria in the case of turbine upgrades or replacements.<br>
                  <br> In each case, 1 = not at all important and 5 = extremely important.<br>"),
             #Fish Biomass
             sliderInput(inputId = "FishBiomass3", label = "Please rate the importance of fish biomass:", value=3, min=1, max=5, step = 0.25),
             #River Recreation
             sliderInput(inputId = "RiverRec3", label = "Please rate the importance of River Recreation:", value=3, min=1, max=5, step = 0.25),
             #Reservoir Storage
             sliderInput(inputId = "Reservoir3", label = "Please rate the importance of Reservoir Storage:", value=3, min=1, max=5, step = 0.25),
             #One-Time Project Costs
             sliderInput(inputId = "ProjCost3", label = "Please rate the importance of One-Time Project Costs:", value=3, min=1, max=5, step = 0.25),
             #Dam Safety
             sliderInput(inputId = "Safety3", label = "Please rate the importance of Dam Safety:", value=3, min=1, max=5, step = 0.25),
             #Number of Properties
             sliderInput(inputId = "NumProperties3", label = "Please rate the importance of Number of Properties Impacted:", value=3, min=1, max=5, step = 0.25),
             #Hydropower Capacity
             sliderInput(inputId = "HydroCapacity3", label = "Please rate the importance of Hydropower Capacity:", value=3, min=1, max=5, step = 0.25),
             
             actionButton("updateBtn", "Update"),
             
             tableOutput("SummTable3"), 
             plotOutput("SummPlot3")), #End main panel
    
    #End Alternative 3: Turbine Upgrades or Replacements Tab  
    
    tabPanel("Alternative 4: Installing Turbines or Expanding Existing Capacity", 
             HTML("Indicate your level of preference associated with each of the following criteria in the case of installing turbines or expanding hyropower capacity.<br>
                  <br> In each case, 1 = not at all important and 5 = extremely important.<br>"),
             #Fish Biomass
             sliderInput(inputId = "FishBiomass4", label = "Please rate the importance of fish biomass:", value=3, min=1, max=5, step = 0.25),
             #River Recreation
             sliderInput(inputId = "RiverRec4", label = "Please rate the importance of River Recreation:", value=3, min=1, max=5, step = 0.25),
             #Reservoir Storage
             sliderInput(inputId = "Reservoir4", label = "Please rate the importance of Reservoir Storage:", value=3, min=1, max=5, step = 0.25),
             #One-Time Project Costs
             sliderInput(inputId = "ProjCost4", label = "Please rate the importance of One-Time Project Costs:", value=3, min=1, max=5, step = 0.25),
             #Dam Safety
             sliderInput(inputId = "Safety4", label = "Please rate the importance of Dam Safety:", value=3, min=1, max=5, step = 0.25),
             #Number of Properties
             sliderInput(inputId = "NumProperties4", label = "Please rate the importance of Number of Properties Impacted:", value=3, min=1, max=5, step = 0.25),
             #Hydropower Capacity
             sliderInput(inputId = "HydroCapacity4", label = "Please rate the importance of Hydropower Capacity:", value=3, min=1, max=5, step = 0.25),
             
             actionButton("updateBtn", "Update"),
             
             tableOutput("SummTable4"), 
             plotOutput("SummPlot4")), #End main panel
    
    #End Alternative 4: Installing or expanding hydropower capacity Tab
    
    tabPanel("Alternative 5: Refurbishment, Restoration, or Maintenance", 
             HTML("Indicate your level of preference associated with each of the following criteria in the case of refurbishment, restoration, or maintenance.<br>
                  <br> In each case, 1 = not at all important and 5 = extremely important.<br>"),
             #Fish Biomass
             sliderInput(inputId = "FishBiomass5", label = "Please rate the importance of fish biomass:", value=3, min=1, max=5, step = 0.25),
             #River Recreation
             sliderInput(inputId = "RiverRec5", label = "Please rate the importance of River Recreation:", value=3, min=1, max=5, step = 0.25),
             #Reservoir Storage
             sliderInput(inputId = "Reservoir5", label = "Please rate the importance of Reservoir Storage:", value=3, min=1, max=5, step = 0.25),
             #One-Time Project Costs
             sliderInput(inputId = "ProjCost5", label = "Please rate the importance of One-Time Project Costs:", value=3, min=1, max=5, step = 0.25),
             #Dam Safety
             sliderInput(inputId = "Safety5", label = "Please rate the importance of Dam Safety:", value=3, min=1, max=5, step = 0.25),
             #Number of Properties
             sliderInput(inputId = "NumProperties5", label = "Please rate the importance of Number of Properties Impacted:", value=3, min=1, max=5, step = 0.25),
             #Hydropower Capacity
             sliderInput(inputId = "HydroCapacity5", label = "Please rate the importance of Hydropower Capacity:", value=3, min=1, max=5, step = 0.25),
             
             actionButton("updateBtn", "Update"),
             
             tableOutput("SummTable5"), 
             plotOutput("SummPlot5")), #End main panel
    
    #End Alternative 5: Refurbishment, Restoration, or Maintenance Tab
    
    tabPanel("Alternative 6: Keep Dam (Do Nothing)", 
             HTML("Indicate your level of preference associated with each of the following criteria in the case of keeping the dam (doing nothing).<br>
                  <br> In each case, 1 = not at all important and 5 = extremely important.<br>"),
             #Fish Biomass
             sliderInput(inputId = "FishBiomass6", label = "Please rate the importance of fish biomass:", value=3, min=1, max=5, step = 0.25),
             #River Recreation
             sliderInput(inputId = "RiverRec6", label = "Please rate the importance of River Recreation:", value=3, min=1, max=5, step = 0.25),
             #Reservoir Storage
             sliderInput(inputId = "Reservoir6", label = "Please rate the importance of Reservoir Storage:", value=3, min=1, max=5, step = 0.25),
             #One-Time Project Costs
             sliderInput(inputId = "ProjCost6", label = "Please rate the importance of One-Time Project Costs:", value=3, min=1, max=5, step = 0.25),
             #Dam Safety
             sliderInput(inputId = "Safety6", label = "Please rate the importance of Dam Safety:", value=3, min=1, max=5, step = 0.25),
             #Number of Properties
             sliderInput(inputId = "NumProperties6", label = "Please rate the importance of Number of Properties Impacted:", value=3, min=1, max=5, step = 0.25),
             #Hydropower Capacity
             sliderInput(inputId = "HydroCapacity6", label = "Please rate the importance of Hydropower Capacity:", value=3, min=1, max=5, step = 0.25),
             
             actionButton("updateBtn", "Update"),
             
             tableOutput("SummTable6"), 
             plotOutput("SummPlot6")), #End main panel
    
    #End Alternative 6: Keep Dam (Do Nothing) Tab  
    tabPanel("Output",
             HTML("<br><b>Use this button to get results:"),
             actionButton("updateBtn", "Update"),
             
             tableOutput("WSMTable"),
             plotOutput("WSMPlot")),    
    
    id = "tabs"
    )
))
