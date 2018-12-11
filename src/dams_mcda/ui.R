# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
#base_dir <- "~/Beatrice2/R_ELF/R_NEST/MCDA_App_Shiny/"
#working_dir <- paste("MCDA_11132018/WSM_Tool", "", sep="")
#setwd(working_dir)

base_dir <- "/srv/shiny-server/dams_mcda/"
setwd(base_dir)

library(shiny)
library(ggplot2)
library(dplyr)
source("server.R")

# default graph dimensions
graph_width <- 1200
graph_height <- 300


# Define UI for Shiny web application
ui <- shinyUI(fluidPage(
	# link css
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "dams_mcda.css")
	),

	titlePanel("A Watershed-Scale Dam Decision Making Tool"),

	navlistPanel(
		# Define layout widths
		widths = c(4,8),

		#Define Instructions tab
		tabPanel("Multi-Criteria Decision Analysis",
				textOutput("Introduction"),
				# HTML("<h5><b>Created by: Emma Fox</b></h5>"),
				# HTML("Shiny app code available on GITHUB for download: https://github.com/elbfox?tab=repositories")
				textOutput("Citations")),

		tabPanel(
				#"Alternative 1: Dam Removal",
				htmlOutput("Alt1"), # status and title
				HTML("Indicate your level of preference associated with each of the following criteria in the case of dam removal.<br>
					 <br>In each case, 0 = not at all important and 5 = extremely important.<br>"),

				#Fish Biomass
				sliderInput(inputId = "FishBiomass1", label = "Please rate the importance of fish biomass:", value=0, min=0, max=5, step = 0.25),
				#River Recreation
				sliderInput(inputId = "RiverRec1", label = "Please rate the importance of River Recreation:", value=0, min=0, max=5, step = 0.25),
				#Reservoir Storage
				sliderInput(inputId = "Reservoir1", label = "Please rate the importance of Reservoir Storage:", value=0, min=0, max=5, step = 0.25),
				#One-Time Project Costs
				sliderInput(inputId = "ProjectCost1", label = "Please rate the importance of One-Time Project Costs:", value=0, min=0, max=5, step = 0.25),
				#Dam Safety
				sliderInput(inputId = "Safety1", label = "Please rate the importance of Dam Safety:", value=0, min=0, max=5, step = 0.25),
				#Number of Properties
				sliderInput(inputId = "NumProperties1", label = "Please rate the importance of Number of Properties Impacted:", value=0, min=0, max=5, step = 0.25),
				#Hydropower Capacity
				sliderInput(inputId = "HydroCapacity1", label = "Please rate the importance of Hydropower Capacity:", value=0, min=0, max=5, step = 0.25),

				actionButton("updateBtn1", "Update"),

				tableOutput("SummTable1"),
				plotOutput("SummPlot1", height=graph_height, width=graph_width)), # end
		#End Alternative 1: Dam Removal Tab

		tabPanel(
				#"Alternative 2: Improve Fish Passage Facilities",
				htmlOutput("Alt2"), # status and title
				HTML("Indicate your level of preference associated with each of the following criteria in the case of improvements to fish passage facilities.<br>
					 <br> In each case, 0 = not at all important and 5 = extremely important.<br>"),

				#Fish Biomass
				sliderInput(inputId = "FishBiomass2", label = "Please rate the importance of fish biomass:", value=0, min=0, max=5, step = 0.25),
				#River Recreation
				sliderInput(inputId = "RiverRec2", label = "Please rate the importance of River Recreation:", value=0, min=0, max=5, step = 0.25),
				#Reservoir Storage
				sliderInput(inputId = "Reservoir2", label = "Please rate the importance of Reservoir Storage:", value=0, min=0, max=5, step = 0.25),
				#One-Time Project Costs
				sliderInput(inputId = "ProjectCost2", label = "Please rate the importance of One-Time Project Costs:", value=0, min=0, max=5, step = 0.25),
				#Dam Safety
				sliderInput(inputId = "Safety2", label = "Please rate the importance of Dam Safety:", value=0, min=0, max=5, step = 0.25),
				#Number of Properties
				sliderInput(inputId = "NumProperties2", label = "Please rate the importance of Number of Properties Impacted:", value=0, min=0, max=5, step = 0.25),
				#Hydropower Capacity
				sliderInput(inputId = "HydroCapacity2", label = "Please rate the importance of Hydropower Capacity:", value=0, min=0, max=5, step = 0.25),

				actionButton("updateBtn2", "Update"),

				tableOutput("SummTable2"),
				plotOutput("SummPlot2", height=graph_height, width=graph_width)), # end
		#End Alternative 2: Fish Passage Facility Improvements Tab


		tabPanel(
				#"Alternative 3: Upgrade or Replace Turbines at Existing Powered Dams",
				htmlOutput("Alt3"), # status and title
				HTML("Indicate your level of preference associated with each of the following criteria in the case of turbine upgrades or replacements.<br>
					<br> In each case, 0 = not at all important and 5 = extremely important.<br>"),

				#Fish Biomass
				sliderInput(inputId = "FishBiomass3", label = "Please rate the importance of fish biomass:", value=0, min=0, max=5, step = 0.25),
				#River Recreation
				sliderInput(inputId = "RiverRec3", label = "Please rate the importance of River Recreation:", value=0, min=0, max=5, step = 0.25),
				#Reservoir Storage
				sliderInput(inputId = "Reservoir3", label = "Please rate the importance of Reservoir Storage:", value=0, min=0, max=5, step = 0.25),
				#One-Time Project Costs
				sliderInput(inputId = "ProjectCost3", label = "Please rate the importance of One-Time Project Costs:", value=0, min=0, max=5, step = 0.25),
				#Dam Safety
				sliderInput(inputId = "Safety3", label = "Please rate the importance of Dam Safety:", value=0, min=0, max=5, step = 0.25),
				#Number of Properties
				sliderInput(inputId = "NumProperties3", label = "Please rate the importance of Number of Properties Impacted:", value=0, min=0, max=5, step = 0.25),
				#Hydropower Capacity
				sliderInput(inputId = "HydroCapacity3", label = "Please rate the importance of Hydropower Capacity:", value=0, min=0, max=5, step = 0.25),

				actionButton("updateBtn3", "Update"),

				tableOutput("SummTable3"),
				plotOutput("SummPlot3", height=graph_height, width=graph_width)), # end
		#End Alternative 3: Turbine Upgrades or Replacements Tab


		tabPanel(
				#"Alternative 4: Installing Turbines or Expanding Existing Capacity",
				htmlOutput("Alt4"), # status and title
				HTML("Indicate your level of preference associated with each of the following criteria in the case of installing turbines or expanding hyropower capacity.<br>
				     <br> In each case, 0 = not at all important and 5 = extremely important.<br>"),
				#Fish Biomass
				sliderInput(inputId = "FishBiomass4", label = "Please rate the importance of fish biomass:", value=0, min=0, max=5, step = 0.25),
				#River Recreation
				sliderInput(inputId = "RiverRec4", label = "Please rate the importance of River Recreation:", value=0, min=0, max=5, step = 0.25),
				#Reservoir Storage
				sliderInput(inputId = "Reservoir4", label = "Please rate the importance of Reservoir Storage:", value=0, min=0, max=5, step = 0.25),
				#One-Time Project Costs
				sliderInput(inputId = "ProjectCost4", label = "Please rate the importance of One-Time Project Costs:", value=0, min=0, max=5, step = 0.25),
				#Dam Safety
				sliderInput(inputId = "Safety4", label = "Please rate the importance of Dam Safety:", value=0, min=0, max=5, step = 0.25),
				#Number of Properties
				sliderInput(inputId = "NumProperties4", label = "Please rate the importance of Number of Properties Impacted:", value=0, min=0, max=5, step = 0.25),
				#Hydropower Capacity
				sliderInput(inputId = "HydroCapacity4", label = "Please rate the importance of Hydropower Capacity:", value=0, min=0, max=5, step = 0.25),

				actionButton("updateBtn4", "Update"),

				tableOutput("SummTable4"),
				plotOutput("SummPlot4", height=graph_height, width=graph_width)), # end
		#End Alternative 4: Installing or expanding hydropower capacity Tab

		tabPanel(
				#"Alternative 5: Refurbishment, Restoration, or Maintenance",
				htmlOutput("Alt5"), # status and title
				HTML("Indicate your level of preference associated with each of the following criteria in the case of refurbishment, restoration, or maintenance.<br>
					<br> In each case, 0 = not at all important and 5 = extremely important.<br>"),

				#Fish Biomass
				sliderInput(inputId = "FishBiomass5", label = "Please rate the importance of fish biomass:", value=0, min=0, max=5, step = 0.25),
				#River Recreation
				sliderInput(inputId = "RiverRec5", label = "Please rate the importance of River Recreation:", value=0, min=0, max=5, step = 0.25),
				#Reservoir Storage
				sliderInput(inputId = "Reservoir5", label = "Please rate the importance of Reservoir Storage:", value=0, min=0, max=5, step = 0.25),
				#One-Time Project Costs
				sliderInput(inputId = "ProjectCost5", label = "Please rate the importance of One-Time Project Costs:", value=0, min=0, max=5, step = 0.25),
				#Dam Safety
				sliderInput(inputId = "Safety5", label = "Please rate the importance of Dam Safety:", value=0, min=0, max=5, step = 0.25),
				#Number of Properties
				sliderInput(inputId = "NumProperties5", label = "Please rate the importance of Number of Properties Impacted:", value=0, min=0, max=5, step = 0.25),
				#Hydropower Capacity
				sliderInput(inputId = "HydroCapacity5", label = "Please rate the importance of Hydropower Capacity:", value=0, min=0, max=5, step = 0.25),

				actionButton("updateBtn5", "Update"),

				tableOutput("SummTable5"),
				plotOutput("SummPlot5", height=graph_height, width=graph_width)), # end
		#End Alternative 5: Refurbishment, Restoration, or Maintenance Tab

		tabPanel("Output",
				#TODO:?  could warn user here if they havent completed everything
				HTML("<br><b>After completing all Alternatives use this button to get results:</b>"),

				# event
				# TODO:? we could make event fire by checking if all alternatives have been 'updated'
				actionButton("generateMatrix", "Generate"),

				# TODO remove
				HTML("<br><b>(for debugging output) auto complete all alternatives and generate:</b>"),
				actionButton("autoGenerateMatrix", "Auto Complete"),

				# output tables
				HTML('<br>FilledCriteriaTable<br>'),
				tableOutput("FilledCriteriaTable"), # for debugging criteria table
				HTML('<br>WSMTable<br>'),
				tableOutput("WSMTable"),
				HTML('<br>Summed Scores<br>'),
				plotOutput("WSMPlot", height=300, width=1000),

		id = "tabs"
    )
)))


# create the application with ui in this file and imported server from server.R
shinyApp(ui, server)
