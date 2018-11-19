# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
base_dir <- "/srv/shiny-server/dams_mcda/"
response_dir <- paste(base_dir, "responses/", sep="")
working_dir <- paste(base_dir, "", sep="")


library(shiny)
library(ggplot2)
library(dplyr)

# Original Working Directory
setwd(working_dir)
source("WSM.R")


##########################################################################
#############################INFO ON DATA STORAGE#########################

# identify which fields get saved
#fieldsAll <- c(Fish, Rec, Res, Cost, Safe, Houses, Power, WSMResults)

# get current Epoch time
epochTime <- function() {
	return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid characters in Windows filenames)
humanTime <- function() {
	format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
	fileName <- sprintf("ResultsRaw.csv",
						humanTime(),
						digest::digest(data))

	write.csv(x = data, file = file.path(responsesDir, fileName),
			  row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
	files <- list.files(file.path(responsesDir), full.names = TRUE)
	data <- lapply(files, read.csv, stringsAsFactors = FALSE)
	data <- dplyr::rbind_all(data)
	data <- do.call(rbind, data)
	data
}

# directory where responses get stored
responsesDir <- file.path(response_dir)

##########################################################################
#######################SERVER#############################################

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

	#####INTRO TEXT#######
	output$Introduction    <- renderText("This R Shiny app supports decision making about hydropower dams at a watershed scale, using a set of criteria and decision alternatives identified through stakeholder interviews. The tool uses a Weighted Sum approach to Multi-Criteria Decision Analysis (MCDA) to compare decision maker preferences for criteria over a set of decision alternatives. Toggle through the ALTERNATIVE pages at left to compare criteria under a single decision alternative and view alternative-specific results. Select the OUTPUT tab and click UPDATE to view results after completing all alternative comparisons.")
	output$Citations       <- renderText("SEE ALSO: Raymond, G. (2018). Web App: Multi-Criteria Decision Analysis of Fuel Pathways.https://fuel-production-pathway-comparison-tool.shinyapps.io/gr_ui_sep_models/")

	######## RAW OUTPUT for each Alternative ########

	#ALTERNATIVE 1
	observeEvent(input$updateBtn, {
		Alt1        <- cbind(c(input$FishBiomass1, input$RiverRec1, input$Reservoir1, input$ProjCost1, input$Safety1, input$NumProperties1, input$HydroCapacity1))
		Alt1_Table      <- as.matrix(data.frame(Alt1))
		row.names(Alt1_Table) <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity")
		colnames(Alt1_Table)  <- "Raw Score"
		t_Alt1_Table    <-t(Alt1_Table)
		Alt1_Bar        <-barplot(t_Alt1_Table, main="Raw Scores Alternative 1",
								xlab=c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity"),
								col=c("darkblue", "purple", "green", "red", "yellow", "orange", "pink"))
		Alt1_Results <- list(Alt1_Table, Alt1_Bar)
}) #end observe event
	output$SummTable1      <- renderTable(Alt1_Results[1])
	output$SummPlot1       <- renderPlot(Alt1_Results[2])


	#ALTERNATIVE 2
	observeEvent(input$updateBtn, {
		Alt2        <- cbind(c(input$FishBiomass2, input$RiverRec2, input$Reservoir2, input$ProjCost2, input$Safety2, input$NumProperties2, input$HydroCapacity2))
		Alt2_Table      <- as.matrix(data.frame(Alt2))
		row.names(Alt2_Table) <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity")
		colnames(Alt2_Table)  <- "Raw Score"
		t_Alt2_Table    <-t(Alt2_Table)
		Alt2_Bar        <-barplot(t_Alt2_Table, main="Raw Scores Alternative 1",
							   xlab=c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity"),
							   col=c("darkblue", "purple", "green", "red", "yellow", "orange", "pink"))
		Alt2_Results <- list(Alt2_Table, Alt2_Bar)
	}) #end observe event
	output$SummTable2      <- renderTable(Alt2_Results[1])
	output$SummPlot2       <- renderPlot(Alt2_Results[2])


	#ALTERNATIVE 3
	observeEvent(input$updateBtn, {
		Alt3        <- cbind(c(input$FishBiomass3, input$RiverRec3, input$Reservoir3, input$ProjCost3, input$Safety3, input$NumProperties3, input$HydroCapacity3))
		Alt3_Table      <- as.matrix(data.frame(Alt3))
		row.names(Alt3_Table) <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity")
		colnames(Alt3_Table)  <- "Raw Score"
		t_Alt3_Table    <-t(Alt3_Table)
		Alt3_Bar        <-barplot(t_Alt3_Table, main="Raw Scores Alternative 1",
								  xlab=c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity"),
								  col=c("darkblue", "purple", "green", "red", "yellow", "orange", "pink"))
		Alt3_Results <- list(Alt3_Table, Alt3_Bar)
	}) #end observe event
	output$SummTable3      <- renderTable(Alt3_Results[1])
	output$SummPlot3       <- renderPlot(Alt3_Results[2])


	#ALTERNATIVE 4
	observeEvent(input$updateBtn, {
		Alt4        <- cbind(c(input$FishBiomass4, input$RiverRec4, input$Reservoir4, input$ProjCost4, input$Safety4, input$NumProperties4, input$HydroCapacity4))
		Alt4_Table      <- as.matrix(data.frame(Alt4))
		row.names(Alt4_Table) <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity")
		colnames(Alt4_Table)  <- "Raw Score"
		t_Alt4_Table    <-t(Alt4_Table)
		Alt4_Bar        <-barplot(t_Alt4_Table, main="Raw Scores Alternative 1",
								  xlab=c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity"),
								  col=c("darkblue", "purple", "green", "red", "yellow", "orange", "pink"))
		Alt4_Results <- list(Alt4_Table, Alt4_Bar)
	}) #end observe event
	output$SummTable4      <- renderTable(Alt4_Results[1])
	output$SummPlot4       <- renderPlot(Alt4_Results[2])


	#ALTERNATIVE 5
	observeEvent(input$updateBtn, {
		Alt5        <- cbind(c(input$FishBiomass5, input$RiverRec5, input$Reservoir5, input$ProjCost5, input$Safety5, input$NumProperties5, input$HydroCapacity5))
		Alt5_Table      <- as.matrix(data.frame(Alt5))
		row.names(Alt5_Table) <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity")
		colnames(Alt5_Table)  <- "Raw Score"
		t_Alt5_Table    <-t(Alt5_Table)
		Alt5_Bar        <-barplot(t_Alt5_Table, main="Raw Scores Alternative 1",
								  xlab=c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity"),
								  col=c("darkblue", "purple", "green", "red", "yellow", "orange", "pink"))
		Alt5_Results <- list(Alt5_Table, Alt5_Bar)
	}) #end observe event
	output$SummTable5      <- renderTable(Alt5_Results[1])
	output$SummPlot5       <- renderPlot(Alt5_Results[2])


	#ALTERNATIVE 6
	observeEvent(input$updateBtn, {
		Alt6        <- cbind(c(input$FishBiomass6, input$RiverRec6, input$Reservoir6, input$ProjCost6, input$Safety6, input$NumProperties6, input$HydroCapacity6))
		Alt6_Table      <- as.matrix(data.frame(Alt6))
		row.names(Alt6_Table) <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity")
		colnames(Alt6_Table)  <- "Raw Score"
		t_Alt6_Table    <-t(Alt6_Table)
		Alt6_Bar        <-barplot(t_Alt6_Table, main="Raw Scores Alternative 1",
								  xlab=c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity"),
								  col=c("darkblue", "purple", "green", "red", "yellow", "orange", "pink"))
		Alt6_Results <- list(Alt6_Table, Alt2_Bar)
	}) #end observe event
	output$SummTable6      <- renderTable(Alt6_Results[1])
	output$SummPlot6       <- renderPlot(Alt6_Results[2])


	#####################MCDA Table ############################
	#Build empty matrix.
	RawCriteriaMatrix            <- data.frame(matrix(data=NA, nrow=6, ncol=7))

	######fill empty matrix with user input values
	observeEvent(input$updateBtn, {
					 Fish <- c(input$FishBiomass1, input$FishBiomass2, input$FishBiomass3, input$FishBiomass4, input$FishBiomass5, input$FishBiomass6)
					 Rec <- c(input$RiverRec1, input$RiverRec2, input$RiverRec3, input$RiverRec4, input$RiverRec5, input$RiverRec6)
					 Res <- c(input$Reservoir1, input$Reservoir2, input$Reservoir3, input$Reservoir4, input$Reservoir5, input$Reservoir6)
					 Cost <- c(input$ProjectCost1, input$ProjectCost2, input$ProjectCost3, input$ProjectCost4, input$ProjectCost5, input$ProjectCost6)
					 Safe <- c(input$Safety1, input$Safety2, input$Safety3, input$Safety4, input$Safety5, input$Safety6)
					 Houses <- c(input$NumProperties1, input$NumProperties2, input$NumProperties3, input$NumProperties4, input$NumProperties5, input$NumProperties6)
					 Power <- c(input$HydroCapacity1, input$HydroCapacity2, input$HydroCapacity3, input$HydroCapacity4, input$HydroCapacity5, input$HydroCapacity6)
	})   #end observe event

	RawCriteriaMatrix <- data.frame(cbind(Fish, Rec, Res, Cost, Safe, Houses, Power))

	row.names(RawCriteriaMatrix) <- paste(c("Dam Removal", "Fish Improve", "Turbine Improve", "Turbine Add or Expand", "Dam Refurbish or Maintain", "Keep Dam"), sep = " ")
	colnames(RawCriteriaMatrix) <- paste(c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Number of Properties Impacted", "Dam Safety", "Hydropower Capacity"), sep = " ")

	CritImportance    <- c(Fish, Rec, Res, Cost, Houses, Safe, Power)/sum(Fish, Rec, Res, Cost, Houses, Safe, Power)

	#### Call WSM function to produce ranked alternatives result#####

	WSMResults <- WSM(CritImportance=CritImportance, RawCriteriaMatrix=RawCriteriaMatrix)
	output$WSMTable <- renderTable(WSMResults[1])  
	output$WSMBar  <-renderPlot({WSMResults[2]}) 

}) #end server 
