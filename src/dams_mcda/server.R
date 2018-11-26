library(shiny)
library(ggplot2)
library(dplyr)
source("WSM.R")


#--------------------------------------------------------------------------------
# Static Variables
#--------------------------------------------------------------------------------

#----------------------------------------
# working directories
# path where files are saved/opened (if any)
#----------------------------------------
base_dir <- "/srv/shiny-server/dams_mcda/" # root
response_dir <- paste(base_dir, "responses/", sep="") # where responses are saved
working_dir <- paste(base_dir, "", sep="") # default

setwd(working_dir) # initial directory
responsesDir <- file.path(response_dir) # directory where responses get stored

#----------------------------------------
# Output
#----------------------------------------
# table output: set to TRUE to show row names
enable_rownames <- TRUE

#----------------------------------------
# Defaults
#----------------------------------------
# default graph color array
colors <- c("darkblue", "purple", "green", "red", "yellow", "orange", "pink")
# default graph score range
score_range = c(1, 5)
# default names of the fields
variable_names <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity")
wsm_bar_plot_names <- c("Dam Removal", "Fish Improve", "Turbine Improve", "Turbine Add or Expand", "Dam Refurbish or Maintain", "Keep Dam")

# Keep track of completed sections
alternatives_completed = vector()

#----------------------------------------
# Misc.
#----------------------------------------
# fix R.plots.pdf error
# (see: https://stackoverflow.com/questions/36777416/plotly-plot-not-rendering-on-shiny-server)
# needed when calling barplot
pdf(NULL)

# TODO: do we need this?
# identify which fields get saved
#fieldsAll <- c(Fish, Rec, Res, Cost, Safe, Houses, Power, WSMResults)
# End of static variables

#--------------------------------------------------------------------------------
# FILE/DATA STORAGE
#--------------------------------------------------------------------------------

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
#	data
}


# renderBarPlotFunction
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
renderBarPlot <- function(data, title, names, x_label, y_label, colors, x_limit, y_limit) {
	# debug data
	message('------------------')
	message('BarPlot title:', title, '\ndata:\n', data, "\ndatatype:\n", typeof(data))
	message('------------------')
	return( renderPlot(barplot(
			data,
			main=title,
			names.arg=names,
			xlab=x_label, ylab=y_label,
			xlim=x_limit, ylim=y_limit,
			col=colors,
			xpd=FALSE
			)
		)
	)
}

# remove and refill progress of a status
# action is "remove" or "add"
# TODO: finish
updateProgress <- function(action, id){
	if (id %in% alternatives_completed & action == "remove"){
	}else if (action =="add"){
	}
}

#--------------------------------------------------------------------------------
# SERVER
# Define server logic required to draw a histogram
#--------------------------------------------------------------------------------
shinyServer(function(input, output) {
	#----------------------------------------
	# INTRO TEXT
	#----------------------------------------
	output$Introduction    <- renderText("This R Shiny app supports decision making about hydropower dams at a watershed scale, using a set of criteria and decision alternatives identified through stakeholder interviews. The tool uses a Weighted Sum approach to Multi-Criteria Decision Analysis (MCDA) to compare decision maker preferences for criteria over a set of decision alternatives. Toggle through the ALTERNATIVE pages at left to compare criteria under a single decision alternative and view alternative-specific results. Select the OUTPUT tab and click UPDATE to view results after completing all alternative comparisons.")
	output$Citations       <- renderText("SEE ALSO: Raymond, G. (2018). Web App: Multi-Criteria Decision Analysis of Fuel Pathways.https://fuel-production-pathway-comparison-tool.shinyapps.io/gr_ui_sep_models/")

	# initial tabs status
	output$Alt1 <- renderUI(list(
		"Alternative 1: Dam Removal",
		tags$span('Not Complete', class="alt-not-complete")
	))

	######## RAW OUTPUT for each Alternative ########

	#----------------------------------------
	# ALTERNATIVE 1
	#----------------------------------------
	# update event
	observeEvent(input$updateBtn1, {
		# update the tab status
		output$Alt1 <- renderUI(list(
			'Alternative 1: Dam Removal',
			tags$span('Complete', class="alt-complete")
		))

		# get decision inputs
		Alt1 <- c(input$FishBiomass1, input$RiverRec1, input$Reservoir1, input$ProjectCost1, input$Safety1, input$NumProperties1, input$HydroCapacity1)

		# create table matrix 1x5
		Alt1_Table <- as.matrix(data.frame(Alt1))
		row.names(Alt1_Table) <- variable_names
		colnames(Alt1_Table) <- "Raw Score"

		# results
		output$SummTable1 <- renderTable(Alt1_Table, rownames=enable_rownames)
		output$SummPlot1 <- renderBarPlot(
								Alt1, # data
								"Raw Scores of Alternative 1", # title
								variable_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
	}) # end observe event


	#----------------------------------------
	# ALTERNATIVE 2
	#----------------------------------------
	observeEvent(input$updateBtn2, {
		# get decision inputs
		Alt2 <- c(input$FishBiomass2, input$RiverRec2, input$Reservoir2, input$ProjectCost2, input$Safety2, input$NumProperties2, input$HydroCapacity2)

		# create table matrix 1x5
		Alt2_Table <- as.matrix(data.frame(Alt2))
		row.names(Alt2_Table) <- variable_names
		colnames(Alt2_Table) <- "Raw Score"

		# results
		output$SummTable2 <- renderTable(Alt2_Table, rownames=enable_rownames)
		output$SummPlot2 <- renderBarPlot(
								Alt2, # data
								"Raw Scores of Alternative 2", # title
								variable_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
	}) #end observe event


	#----------------------------------------
	# ALTERNATIVE 3
	#----------------------------------------
	observeEvent(input$updateBtn3, {
		# get decision inputs
		Alt3 <- c(input$FishBiomass3, input$RiverRec3, input$Reservoir3, input$ProjectCost3, input$Safety3, input$NumProperties3, input$HydroCapacity3)

		# create table matrix 1x5
		Alt3_Table <- as.matrix(data.frame(Alt3))
		row.names(Alt3_Table) <- variable_names
		colnames(Alt3_Table) <- "Raw Score"

		# results
		output$SummTable3 <- renderTable(Alt3_Table, rownames=enable_rownames)
		output$SummPlot3 <- renderBarPlot(
								Alt3, # data
								"Raw Scores of Alternative 3", # title
								variable_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
	}) #end observe event


	#----------------------------------------
	# ALTERNATIVE 4
	#----------------------------------------
	observeEvent(input$updateBtn4, {
		# get decision inputs
		Alt4 <- cbind(c(input$FishBiomass4, input$RiverRec4, input$Reservoir4, input$ProjectCost4, input$Safety4, input$NumProperties4, input$HydroCapacity4))

		# create table matrix 1x5
		Alt4_Table <- as.matrix(data.frame(Alt4))
		row.names(Alt4_Table) <- variable_names
		colnames(Alt4_Table) <- "Raw Score"

		# results
		output$SummTable4 <- renderTable(Alt4_Table, rownames=enable_rownames)
		output$SummPlot4 <- renderBarPlot(
								Alt4, # data
								"Raw Scores of Alternative 4", # title
								variable_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
	}) #end observe event


	#----------------------------------------
	# ALTERNATIVE 5
	#----------------------------------------
	observeEvent(input$updateBtn5, {
		# get decision inputs
		Alt5 <- cbind(c(input$FishBiomass5, input$RiverRec5, input$Reservoir5, input$ProjectCost5, input$Safety5, input$NumProperties5, input$HydroCapacity5))

		# create table matrix 1x5
		Alt5_Table <- as.matrix(data.frame(Alt5))
		row.names(Alt5_Table) <- variable_names
		colnames(Alt5_Table) <- "Raw Score"

		# results
		output$SummTable5 <- renderTable(Alt5_Table, rownames=enable_rownames)
		output$SummPlot5 <- renderBarPlot(
								Alt5, # data
								"Raw Scores of Alternative 5", # title
								variable_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
	}) #end observe event


	#----------------------------------------
	# ALTERNATIVE 6
	#----------------------------------------
	observeEvent(input$updateBtn6, {
		# get decision inputs
		Alt6 <- cbind(c(input$FishBiomass6, input$RiverRec6, input$Reservoir6, input$ProjectCost6, input$Safety6, input$NumProperties6, input$HydroCapacity6))

		# create table matrix 1x6
		Alt6_Table <- as.matrix(data.frame(Alt6))
		row.names(Alt6_Table) <- variable_names
		colnames(Alt6_Table) <- "Raw Score"

		# results
		output$SummTable6 <- renderTable(Alt6_Table, rownames=enable_rownames)
		output$SummPlot6 <- renderBarPlot(
								Alt6, # data
								"Raw Scores of Alternative 6", # title
								variable_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
	}) #end observe event


	#--------------------------------------------------------------------------------
	# MCDA Table Output
	#--------------------------------------------------------------------------------
	# initial empty matrix.
	RawCriteriaMatrix            <- data.frame(matrix(data=NA, nrow=6, ncol=7))

	# on 'Output > Generate' button event: fill matrix with user input values
	observeEvent(input$generateMatrix, {
		Fish <- c(input$FishBiomass1, input$FishBiomass2, input$FishBiomass3, input$FishBiomass4, input$FishBiomass5, input$FishBiomass6)
		Rec <- c(input$RiverRec1, input$RiverRec2, input$RiverRec3, input$RiverRec4, input$RiverRec5, input$RiverRec6)
		Res <- c(input$Reservoir1, input$Reservoir2, input$Reservoir3, input$Reservoir4, input$Reservoir5, input$Reservoir6)
		Cost <- c(input$ProjectCost1, input$ProjectCost2, input$ProjectCost3, input$ProjectCost4, input$ProjectCost5, input$ProjectCost6)
		Safe <- c(input$Safety1, input$Safety2, input$Safety3, input$Safety4, input$Safety5, input$Safety6)
		Houses <- c(input$NumProperties1, input$NumProperties2, input$NumProperties3, input$NumProperties4, input$NumProperties5, input$NumProperties6)
		Power <- c(input$HydroCapacity1, input$HydroCapacity2, input$HydroCapacity3, input$HydroCapacity4, input$HydroCapacity5, input$HydroCapacity6)

		# assign values in new matrix
		RawCriteriaMatrix <- data.frame(cbind(Fish, Rec, Res, Cost, Safe, Houses, Power))

		# assign table row, column names
		row.names(RawCriteriaMatrix) <- paste(c("Dam Removal", "Fish Improve", "Turbine Improve", "Turbine Add or Expand", "Dam Refurbish or Maintain", "Keep Dam"), sep = " ")
		colnames(RawCriteriaMatrix) <- paste(c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Number of Properties Impacted", "Dam Safety", "Hydropower Capacity"), sep = " ")

		CritImportance    <- c(Fish, Rec, Res, Cost, Houses, Safe, Power)/sum(Fish, Rec, Res, Cost, Houses, Safe, Power)

		# for debugging table size
		output$FilledCriteriaTable <- renderTable(RawCriteriaMatrix, rownames=enable_rownames)

		# Call WSM function to produce ranked alternatives result
		WSMResults <- WSM(CritImportance=CritImportance, RawCriteriaMatrix=RawCriteriaMatrix)
		message('Results Done')
		message(WSMResults[1])
		message(WSMResults[2])
		output$WSMTable <- renderTable(WSMResults[1], rownames=enable_rownames)
		output$WSMPlot <- renderBarPlot(
								WSMResults[2], # data
								"WSM Ranked Alternatives", # title
								wsm_bar_plot_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
	})   # end 'output' tab > on generate button event

}) # end server
