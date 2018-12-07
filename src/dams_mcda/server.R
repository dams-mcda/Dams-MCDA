source("WSM.R")


#--------------------------------------------------------------------------------
# Static Variables
#--------------------------------------------------------------------------------

#----------------------------------------
# Working Directories: path where files are saved/opened (if any)
#----------------------------------------
base_dir <- "/srv/shiny-server/dams_mcda/" # root
response_dir <- paste(base_dir, "responses/", sep="") # where responses are saved
working_dir <- paste(base_dir, "", sep="") # default

setwd(working_dir) # initial directory
responsesDir <- file.path(response_dir) # directory where responses get stored

#----------------------------------------
# Output
#----------------------------------------
enable_rownames <- TRUE # set to TRUE to show row names on tables

#----------------------------------------
# Defaults
#----------------------------------------
# default graph color array
colors <- c("darkblue", "purple", "green", "red", "yellow", "orange", "pink")
# default graph score range
score_range <- c(0, 5)
summed_score_range <- c(0, 1)
# default names of the fields
variable_names <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Safety", "Number of Properties Impacted", "Hydropower Capacity")
wsm_bar_plot_names <- c("Dam Removal", "Fish Improve", "Turbine Improve", "Turbine Add or Expand", "Keep and Maintain Dam")


# list of alternatives
available_alternatives <- seq(1:5)

# list of criterion input identifiers
criterion_inputs <- c('FishBiomass', 'RiverRec', 'Reservoir', 'ProjectCost', 'Safety', 'NumProperties', 'HydroCapacity')


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

# epochTime
#----------------------------------------
# get current Epoch time
epochTime <- function() {
	return(as.integer(Sys.time()))
}


# humanTime
#----------------------------------------
# get a formatted string of the timestamp (exclude colons as they are invalid characters in Windows filenames)
humanTime <- function() {
	format(Sys.time(), "%Y%m%d-%H%M%OS")
}


# saveData
#----------------------------------------
# save the results to a file
saveData <- function(data) {
	fileName <- sprintf("ResultsRaw.csv",
						humanTime(),
						digest::digest(data))

	write.csv(x = data, file = file.path(responsesDir, fileName),
			  row.names = FALSE, quote = TRUE)
}

# loadData
#----------------------------------------
# load all responses into a data.frame
loadData <- function() {
	files <- list.files(file.path(responsesDir), full.names = TRUE)
	data <- lapply(files, read.csv, stringsAsFactors = FALSE)
	data <- dplyr::rbind_all(data)
	data <- do.call(rbind, data)
#	data
}


# renderBarPlotFunction
#----------------------------------------
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
renderBarPlot <- function(data, title, x_names, x_label, y_label, colors, x_limit, y_limit) {
	# debug data
	message('------------------')
	message('BarPlot title:', title, '\ndata:', data, "\n#(values):", length(data), "\nclasstype: ", class(data), "\ndatatype: ", typeof(data), "\nnames:", x_names, "\n#(names):", length(x_names))
	message('------------------')
	return( renderPlot(barplot(
				data,
				main=title,
				names.arg=x_names,
				xlab=x_label, ylab=y_label,
				xlim=x_limit, ylim=y_limit,
				col=colors,
				xpd=FALSE
			)
		)
	)
}


# updateAlternativeStatus
#----------------------------------------
# remove and refill progress of a status
# action is status to apply "remove" or "add"
updateAlternativeStatus <- function(completed, action, id){
	message('------------------')
	message('updateAlternativeStatus vector')
	message('------------------')

	if (id %in% completed & action == "remove"){
		completed <- completed[which(x==id)]
	}else if (action =="add" & !(id %in% completed)){
		completed <- c(completed, id)
	}else{
		#message('no Alternative Status Changes')
	}
	return(completed)
}


# alternativesCompleted
#----------------------------------------
# check all available_alternatives are in alternatives_completed
# returns boolean
alternativesCompleted <- function(completed){
	message('------------------')
	message('bool alternativesCompleted(array_completed)')
	message('------------------')

	# available_alternatives is array of alt sections ids
	for (value in available_alternatives){
		if (!(value %in% completed)){
			return(FALSE)
		}
	}
	return(TRUE)
}




#--------------------------------------------------------------------------------
# SERVER
# Define server logic required to draw a histogram
#--------------------------------------------------------------------------------
server <- function(input, output, session) {

	#------------------------------------------------------------
	# updateAlt1
	# logic for updating alternative 1
	#------------------------------------------------------------
	updateAlt1 <- function (){
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
		names(Alt1_Table) <- "Raw Score"

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

		# mark the alternative as complete when update
		# or apply logic here to make other contstraints for "complete"
		session$userData[['alternatives_completed']] <- updateAlternativeStatus(session$userData[['alternatives_completed']], "add", 1)
	}

	#------------------------------------------------------------
	# updateAlt2
	# logic for updating alternative 2
	#------------------------------------------------------------
	updateAlt2 <- function() {
		output$Alt2 <- renderUI(list(
			"Alternative 2: Improve Fish Passage Facilities",
			tags$span('Complete', class="alt-complete")
		))
		# get decision inputs
		Alt2 <- c(input$FishBiomass2, input$RiverRec2, input$Reservoir2, input$ProjectCost2, input$Safety2, input$NumProperties2, input$HydroCapacity2)

		# create table matrix 1x5
		Alt2_Table <- as.matrix(data.frame(Alt2))
		row.names(Alt2_Table) <- variable_names
		names(Alt2_Table) <- "Raw Score"

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

		# mark the alternative as complete when update
		# or apply logic here to make other contstraints for "complete"
		session$userData[['alternatives_completed']] <- updateAlternativeStatus(session$userData[['alternatives_completed']], "add", 2)
	}

	#------------------------------------------------------------
	# updateAlt3
	# logic for updating alternative 3
	#------------------------------------------------------------
	updateAlt3 <- function() {
		output$Alt3 <- renderUI(list(
			"Alternative 3: Upgrade or Replace Turbines at Existing Powered Dams",
			tags$span('Complete', class="alt-complete")
		))

		# get decision inputs
		Alt3 <- c(input$FishBiomass3, input$RiverRec3, input$Reservoir3, input$ProjectCost3, input$Safety3, input$NumProperties3, input$HydroCapacity3)

		# create table matrix 1x5
		Alt3_Table <- as.matrix(data.frame(Alt3))
		row.names(Alt3_Table) <- variable_names
		names(Alt3_Table) <- "Raw Score"

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
		# mark the alternative as complete when update
		# or apply logic here to make other contstraints for "complete"
		#updateAlternativeStatus("add", 3)
		session$userData[['alternatives_completed']] <- updateAlternativeStatus(session$userData[['alternatives_completed']], "add", 3)
	}

	#------------------------------------------------------------
	# updateAlt4
	# logic for updating alternative 4
	#------------------------------------------------------------
	updateAlt4 <- function() {
		output$Alt4 <- renderUI(list(
			"Alternative 4: Installing Turbines or Expanding Existing Capacity",
			tags$span('Complete', class="alt-complete")
		))

		# get decision inputs
		Alt4 <- c(input$FishBiomass4, input$RiverRec4, input$Reservoir4, input$ProjectCost4, input$Safety4, input$NumProperties4, input$HydroCapacity4)

		# create table matrix 1x5
		Alt4_Table <- as.matrix(data.frame(Alt4))
		row.names(Alt4_Table) <- variable_names
		names(Alt4_Table) <- "Raw Score"

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
		# mark the alternative as complete when update
		# or apply logic here to make other contstraints for "complete"
		#updateAlternativeStatus("add", 4)
		session$userData[['alternatives_completed']] <- updateAlternativeStatus(session$userData[['alternatives_completed']], "add", 4)
	}


	#------------------------------------------------------------
	# updateAlt5
	# logic for updating alternative 5
	#------------------------------------------------------------
	updateAlt5 <- function() {
		output$Alt5 <- renderUI(list(
			"Alternative 5: Refurbishment, Restoration, or Maintenance",
			tags$span('Complete', class="alt-complete")
		))

		# get decision inputs
		Alt5 <- c(input$FishBiomass5, input$RiverRec5, input$Reservoir5, input$ProjectCost5, input$Safety5, input$NumProperties5, input$HydroCapacity5)

		# create table matrix 1x5
		Alt5_Table <- as.matrix(data.frame(Alt5))
		row.names(Alt5_Table) <- variable_names
		names(Alt5_Table) <- "Raw Score"

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
		# mark the alternative as complete when update
		# or apply logic here to make other contstraints for "complete"
		#updateAlternativeStatus("add", 5)
		session$userData[['alternatives_completed']] <- updateAlternativeStatus(session$userData[['alternatives_completed']], "add", 5)
	}


	#------------------------------------------------------------
	# generateOutput
	# generate the final table and barplot
	#------------------------------------------------------------
	generateOutput <- function (){

	    if ( !alternativesCompleted(session$userData[['alternatives_completed']]) ){
			# user isnt finished filling out alternatives
			showModal(modalDialog(
				title = "Not Finished!",
				'Please Complete All Alternatives before generating results'
			))

		}else{
			#------------------------------------------------------------
			# get 2d array of values based on length/values of criterion_inputs and available_alternatives
			# criterion -> columns
			# alternatives -> rows
			# example 6 criterion 5 alternatives results in 6 column by 5 row 2d data structure
			#------------------------------------------------------------
			#criterion <- vector("list", length(criterion_inputs))
			alternatives <- vector("list", length(available_alternatives))
			for (row_id in 1:length(available_alternatives)){
				# for each criteria in alternatives
				r <- vector("list", length(available_alternatives))

				for (id in criterion_inputs){
					input_name <- paste(id, toString(row_id), sep='')
					value <- input[[input_name]]
					r[[id]] <- value

					if (is.null(value)){
						# debug nulls, doesn't modify data
						message('input ', input_name, " isNull ")
					}
				}

				alternatives[[row_id]] <- unlist(r) # we want in c and not list
			}
			alternatives <- unlist(alternatives)

			# assign values in new matrix
			RawCriteriaMatrix <- data.frame(
				matrix(alternatives, nrow=length(available_alternatives), byrow=length(criterion_inputs))
			)

			# assign table row, column names
			row.names(RawCriteriaMatrix) <- c("Dam Removal", "Fish Improve", "Turbine Improve", "Turbine Add or Expand", "Keep and Maintain Dam")
			names(RawCriteriaMatrix) <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Number of Properties Impacted", "Dam Safety", "Hydropower Capacity")

			CritImportance    <- alternatives/sum(alternatives)

			# origial scores in table form
			# for debugging table size
			output$FilledCriteriaTable <- renderTable(RawCriteriaMatrix, rownames=enable_rownames)

			#----------------------------------------
			# Call WSM and format response
			#----------------------------------------
			WSMResults <- WSM(CritImportance=CritImportance, RawCriteriaMatrix=RawCriteriaMatrix)

			TableMatrix <- WSMResults[1]

			TableMatrix$summedScore <- WSMResults[2]

			testTableOutput <- data.frame( TableMatrix, row.names=wsm_bar_plot_names, check.names=FALSE)
			names(testTableOutput) <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Number of Properties Impacted", "Dam Safety", "Hydropower Capacity", "Summed Score")

			#----------------------------------------
			# Final Outputs
			#----------------------------------------
			# final output table
			output$WSMTable <- renderTable(testTableOutput, rownames=enable_rownames)

			# final output barplot
			output$WSMPlot <- renderBarPlot(
									# !important!
									unlist(WSMResults[2]), # scoresum data
									"WSM Ranked Alternatives", # title
									wsm_bar_plot_names, # x_labels
									"Topic", # x axis label
									"Score", # y axis label
									colors, # colors
									NULL, # x value limit
									summed_score_range # y value limit (1-5 value range)
								)
		}
	}


	#--------------------------------------------------------------------------------
	# Initial Application State for session
	#--------------------------------------------------------------------------------
	observe({
		#----------------------------------------
		# Keep track of completed sections
		#----------------------------------------
		session$userData[['alternatives_completed']] <- c()

		#----------------------------------------
		# Initial Intro Text
		#----------------------------------------
		output$Introduction    <- renderText("This R Shiny app supports decision making about hydropower dams at a watershed scale, using a set of criteria and decision alternatives identified through stakeholder interviews. The tool uses a Weighted Sum approach to Multi-Criteria Decision Analysis (MCDA) to compare decision maker preferences for criteria over a set of decision alternatives. Toggle through the ALTERNATIVE pages at left to compare criteria under a single decision alternative and view alternative-specific results. Select the OUTPUT tab and click UPDATE to view results after completing all alternative comparisons.")
		output$Citations       <- renderText("SEE ALSO: Raymond, G. (2018). Web App: Multi-Criteria Decision Analysis of Fuel Pathways.https://fuel-production-pathway-comparison-tool.shinyapps.io/gr_ui_sep_models/")

		#----------------------------------------
		# Initial Alternative Tabs Text
		#----------------------------------------
		output$Alt1 <- renderUI(list(
			"Alternative 1: Dam Removal",
			tags$span('Not Complete', class="alt-not-complete")
		))
		output$Alt2 <- renderUI(list(
			"Alternative 2: Improve Fish Passage Facilities",
			tags$span('Not Complete', class="alt-not-complete")
		))
		output$Alt3 <- renderUI(list(
			"Alternative 3: Upgrade or Replace Turbines at Existing Powered Dams",
			tags$span('Not Complete', class="alt-not-complete")
		))
		output$Alt4 <- renderUI(list(
			"Alternative 4: Installing Turbines or Expanding Existing Capacity",
			tags$span('Not Complete', class="alt-not-complete")
		))
		output$Alt5 <- renderUI(list(
			"Alternative 5: Refurbishment, Restoration, or Maintenance",
			tags$span('Not Complete', class="alt-not-complete")
		))
		output$Alt6 <- renderUI(list(
			"Alternative 6: Keep Dam (Do Nothing)",
			tags$span('Not Complete', class="alt-not-complete")
		))
	})

	#--------------------------------------------------------------------------------
	# Alternative Update Event Listeners
	# these trigger the updates on button click
	#--------------------------------------------------------------------------------

	# ALTERNATIVE 1
	#----------------------------------------
	observeEvent(input$updateBtn1, {
		 updateAlt1()
	})

	# ALTERNATIVE 2
	#----------------------------------------
	observeEvent(input$updateBtn2, {
		 updateAlt2()
	})

	# ALTERNATIVE 3
	#----------------------------------------
	observeEvent(input$updateBtn3, {
		 updateAlt3()
	})

	# ALTERNATIVE 4
	#----------------------------------------
	observeEvent(input$updateBtn4, {
		 updateAlt4()
	})

	# ALTERNATIVE 5
	#----------------------------------------
	observeEvent(input$updateBtn5, {
		 updateAlt5()
	})

	#--------------------------------------------------------------------------------
	# MCDA Table Output
	#--------------------------------------------------------------------------------
	# initial empty matrix.
	RawCriteriaMatrix  <- data.frame(matrix(data=NA, nrow=length(available_alternatives), ncol=length(criterion_inputs) ))

	# on 'Output > Generate' button event: fill matrix with user input values
	observeEvent(input$generateMatrix, {
		generateOutput()
	})   # end 'output' tab > on generate button event

	#TODO: remove as this is for fast debugging output results
	observeEvent(input$autoGenerateMatrix, {
		# update all alt
		updateAlt1()
		updateAlt2()
		updateAlt3()
		updateAlt4()
		updateAlt5()
		message('alts updated')
		# generate
		generateOutput()
	})

} # end server
