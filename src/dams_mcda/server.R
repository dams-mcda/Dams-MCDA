# barPlot wrappers
source("plots.R")

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
score_range <- c(0, 1)
# range of final graph of summed scores
summed_score_range <- c(0, 1)
# list of alternatives
available_alternatives <- seq(1:5)

# smallest input slider increment
smallest_increment <- 0.025
# make valid progress values range smaller than the smallest increment
upper_bound <- (1.0 + (smallest_increment/2))
lower_bound <- (1.0 - (smallest_increment/2))

# criteria input identifiers
criteria_inputs <- c(
	"FishBiomass",
	"RiverRec",
	"Reservoir",
	"ProjectCost",
	"Safety",
	"NumProperties",
	"ElectricityGeneration",
	"AvoidEmissions",
	"IndigenousHeritage",
	"IndustrialHistory",
	"CommunityIdentity",
	"Aesthetics"
)

# criteria display names (for labeling tables and graphs)
criteria_names <- c(
	"Fish Survival",
	"River Recreation Area",
	"Reservoir Storage",
	"Annuitized Project Costs",
	"Breach Damage Potential",
	"Number of Properties Impacted",
	"Annual Electricity Generation",
	"CO2 Emissions Reduction",
	"Indigenous Cultural Heritage",
	"Industrial Historical Value",
	"Town/City Identity",
	"Aesthetic Value"
)

# alternative display names (for labeling tables and graphs)
alternative_names <- c(
   "Remove Dam",
   "Improve Fish Passage",
   "Improve Hydro",
   "Improve Hydro AND Fish Passage",
   "Keep and Maintain Dam"
)

# append summed score to criteria_names array
criteria_names_and_sum <- as.list(criteria_names) # vector to list
criteria_names_and_sum[[length(criteria_names_and_sum) + 1]] <- "Summed Score" # append summed score
criteria_names_and_sum <- unlist(criteria_names_and_sum) # return to vector

# End of global variables

#----------------------------------------
# Misc.
#----------------------------------------
# fix R.plots.pdf error
# (see: https://stackoverflow.com/questions/36777416/plotly-plot-not-rendering-on-shiny-server)
# needed when calling barplot
pdf(NULL)


#--------------------------------------------------------------------------------
# FILE/DATA STORAGE
#--------------------------------------------------------------------------------
# has to be global (this is data the user can download after finishing
response_data <<- ("no data")

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


# saveResponse
#----------------------------------------
# save the results to a file
saveResponse <- function(table_data) {
	response_data <<- table_data;
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
#
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
			"Alternative 1: Remove Dam",
			tags$span('Complete', class="alt-complete")
		))

		# get decision inputs
		Alt1 <- c(
			input$FishBiomass1,
			input$RiverRec1,
			input$Reservoir1,
			input$ProjectCost1,
			input$Safety1,
			input$NumProperties1,
			input$ElectricityGeneration1,
			input$AvoidEmissions1,
			input$IndigenousHeritage1,
			input$IndustrialHistory1,
			input$CommunityIdentity1,
			input$Aesthetics1
		)

		# create table matrix 1x5
		Alt1_Table <- as.matrix(data.frame(Alt1))
		row.names(Alt1_Table) <- criteria_names
		names(Alt1_Table) <- "Raw Score"

		# results
		output$SummPlot1 <- renderBarPlot(
								Alt1, # data
								"Raw Scores of Alternative 1", # title
								criteria_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
		shinyjs::show(id="alt-1-output")

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
			"Alternative 2: Improve Fish Passage",
			tags$span('Complete', class="alt-complete")
		))
		# get decision inputs
		Alt2 <- c(
			input$FishBiomass2,
			input$RiverRec2,
			input$Reservoir2,
			input$ProjectCost2,
			input$Safety2,
			input$NumProperties2,
			input$ElectricityGeneration2,
			input$AvoidEmissions2,
			input$IndigenousHeritage2,
			input$IndustrialHistory2,
			input$CommunityIdentity2,
			input$Aesthetics2
			)

		# create table matrix 1x5
		Alt2_Table <- as.matrix(data.frame(Alt2))
		row.names(Alt2_Table) <- criteria_names
		names(Alt2_Table) <- "Raw Score"

		# results
		output$SummPlot2 <- renderBarPlot(
								Alt2, # data
								"Raw Scores of Alternative 2", # title
								criteria_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
		shinyjs::show(id="alt-2-output")

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
			"Alternative 3: Improve Hydropower Generation",
			tags$span('Complete', class="alt-complete")
		))

		# get decision inputs
		Alt3 <- c(
			input$FishBiomass3,
			input$RiverRec3,
			input$Reservoir3,
			input$ProjectCost3,
			input$Safety3,
			input$NumProperties3,
			input$ElectricityGeneration3,
			input$AvoidEmissions3,
			input$IndigenousHeritage3,
			input$IndustrialHistory3,
			input$CommunityIdentity3,
			input$Aesthetics3
			)


		# create table matrix 1x5
		Alt3_Table <- as.matrix(data.frame(Alt3))
		row.names(Alt3_Table) <- criteria_names
		names(Alt3_Table) <- "Raw Score"

		# results
		output$SummPlot3 <- renderBarPlot(
								Alt3, # data
								"Raw Scores of Alternative 3", # title
								criteria_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
		shinyjs::show(id="alt-3-output")
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
			"Alternative 4: Improve Hydropower Generation AND Fish Passage",
			tags$span('Complete', class="alt-complete")
		))

		# get decision inputs
		Alt4 <- c(
			input$FishBiomass4,
			input$RiverRec4,
			input$Reservoir4,
			input$ProjectCost4,
			input$Safety4,
			input$NumProperties4,
			input$ElectricityGeneration4,
			input$AvoidEmissions4,
			input$IndigenousHeritage4,
			input$IndustrialHistory4,
			input$CommunityIdentity4,
			input$Aesthetics4
			)

		# create table matrix 1x5
		Alt4_Table <- as.matrix(data.frame(Alt4))
		row.names(Alt4_Table) <- criteria_names
		names(Alt4_Table) <- "Raw Score"

		# results
		output$SummPlot4 <- renderBarPlot(
								Alt4, # data
								"Raw Scores of Alternative 4", # title
								criteria_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
		shinyjs::show(id="alt-4-output")
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
			"Alternative 5: Keep and Maintain Dam",
			tags$span('Complete', class="alt-complete")
		))

		# get decision inputs
		Alt5 <- c(
			input$FishBiomass5,
			input$RiverRec5,
			input$Reservoir5,
			input$ProjectCost5,
			input$Safety5,
			input$NumProperties5,
			input$ElectricityGeneration5,
			input$AvoidEmissions5,
			input$IndigenousHeritage5,
			input$IndustrialHistory5,
			input$CommunityIdentity5,
			input$Aesthetics5
		)

		# create table matrix 1x5
		Alt5_Table <- as.matrix(data.frame(Alt5))
		row.names(Alt5_Table) <- criteria_names
		names(Alt5_Table) <- "Raw Score"

		# results
		output$SummPlot5 <- renderBarPlot(
								Alt5, # data
								"Raw Scores of Alternative 5", # title
								criteria_names, # x_labels
								"Topic", # x axis label
								"Score", # y axis label
								colors, # colors
								NULL, # x value limit
								score_range # y value limit (1-5 value range)
							)
		shinyjs::show(id="alt-5-output")
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
			# get 2d array of values based on length/values of criteria_inputs and available_alternatives
			# criterion -> columns
			# alternatives -> rows
			# example 6 criterion 5 alternatives results in 6 column by 5 row 2d data structure
			#------------------------------------------------------------
			#criterion <- vector("list", length(criteria_inputs))
			alternatives <- vector("list", length(available_alternatives))
			for (row_id in 1:length(available_alternatives)){
				# for each criteria in alternatives
				r <- vector("list", length(available_alternatives))

				for (id in criteria_inputs){
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
				matrix(alternatives, nrow=length(available_alternatives), byrow=length(criteria_inputs))
			)

			# assign table row, column names
			row.names(RawCriteriaMatrix) <- alternative_names
			colnames(RawCriteriaMatrix) <- criteria_names

			# origial scores in table form
			# for debugging table size
			output$FilledCriteriaTable <- renderTable(RawCriteriaMatrix, rownames=enable_rownames)

			#----------------------------------------
			# Call WSM and format response
			#----------------------------------------
			# matrix setup
			matrix_cols <- length(criteria_inputs) # 7 default (output size, adds summedscore)
			matrix_rows <- length(available_alternatives) # 5 default

			IntermediateMatrix <- data.frame(matrix(data=NA, nrow=matrix_rows, ncol=matrix_cols))
			IntermediateMatrix <- round(RawCriteriaMatrix,3)

			#----------------------------------------
			# Score Sum
			#----------------------------------------
			# total score is last column of returned Data Table
			scoresum <- list("list", matrix_rows)

			for (i in 1:matrix_rows){
			  scoresum[[i]] <- sum(as.numeric(IntermediateMatrix[i, 1:matrix_cols]))
			}

			scoresum <- unlist(scoresum)

			# warning adding things to list has side effects!
			WSMResults <- list(IntermediateMatrix, scoresum)
			TableMatrix <- WSMResults[1]

			TableMatrix$summedScore <- WSMResults[2]

			WSMTableOutput <- data.frame( TableMatrix, row.names=alternative_names, check.names=FALSE)
			# this ones different because it has sum row
			names(WSMTableOutput) <- criteria_names_and_sum

			#----------------------------------------
			# Final Outputs
			#----------------------------------------
			# final output table commented out due to redundancy
			#output$WSMTable <- renderTable(WSMTableOutput, rownames=enable_rownames)

			saveResponse(WSMTableOutput)

			# stacked bars data table
			Alternative <- c(rep(alternative_names, each=length(criteria_names)))
			Criteria <- c(rep(criteria_names, times=length(alternative_names)))
			Score <- alternatives
			Data <- data.frame(Alternative, Criteria, Score)


			# stacked bar plot 1
			output$WSMPlot1 <- renderPlot(
				ggplot(
				  data=Data,
				  aes(x=Alternative, y=Score, fill=Criteria, label=Score),
				  environment = environment()
				)
				+ geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
				+ theme_minimal()
				+ theme(text=element_text(size=20), )
				+ coord_flip()
				+ geom_text(data=subset(Data, Score != 0), size=6, position = position_stack(vjust = 0.5))
				+ theme(
					axis.text.x = element_text(angle = 90, hjust = 1),
					axis.text.y = element_text(angle = 0, hjust = 1)
				)
				+ scale_x_discrete(limits=rev(alternative_names))
			)

			# stacked bar plot 2
			output$WSMPlot2 <- renderPlot(
				ggplot(
				  data=Data,
				  aes(x=Criteria, y=Score, fill=Alternative, label=Score),
				  environment = environment()
				)
				+ geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
				+ theme_minimal()
				+ theme(text=element_text(size=20), )
				+ coord_flip()
				+ theme(
					axis.text.y = element_text(angle = 0, hjust = 1)
				)
				+ scale_x_discrete(limits=rev(criteria_names))
			)

			# show output html elements
			shinyjs::show(id="generated-output")
		}
	}


	#--------------------------------------------------------------------------------
	# Initial Application State for session
	#--------------------------------------------------------------------------------
	observe({
		# hide output html elements
		shinyjs::hide(id="generated-output")
		shinyjs::hide(id="alt-1-output")
		shinyjs::hide(id="alt-2-output")
		shinyjs::hide(id="alt-3-output")
		shinyjs::hide(id="alt-4-output")
		shinyjs::hide(id="alt-5-output")

		#----------------------------------------
		# Keep track of completed sections
		#----------------------------------------
		session$userData[['alternatives_completed']] <- c()

		#----------------------------------------
		# Initial Alternative Tabs Text
		#----------------------------------------
		output$Alt1 <- renderUI(list(
			"Alternative 1: Dam Removal",
			tags$span('Requires User Input', class="alt-not-complete")
		))
		output$Alt2 <- renderUI(list(
			"Alternative 2: Improve Fish Passage",
			tags$span('Requires User Input', class="alt-not-complete")
		))
		output$Alt3 <- renderUI(list(
			"Alternative 3: Improve Hydropower Generation",
			tags$span('Requires User Input', class="alt-not-complete")
		))
		output$Alt4 <- renderUI(list(
			"Alternative 4: Improve Hydropower Generation AND Fish Passage",
			tags$span('Requires User Input', class="alt-not-complete")
		))
		output$Alt5 <- renderUI(list(
			"Alternative 5: Keep and Maintain Dam",
			tags$span('Requires User Input', class="alt-not-complete")
		))

	})

	#------------------------------------------------------------
	# setUp ProgressBars
	# reactive value for input progress
	#------------------------------------------------------------
	# alt1
	progress1 <- reactive({
		sum <- 0.0
		for (id in criteria_inputs){
			sum <- as.numeric(sum + input[[paste0(id, toString(1))]])
		}
		return(sum)
	})
	# alt2
	progress2 <- reactive({
		sum <- 0.0
		for (id in criteria_inputs){
			sum <- as.numeric(sum + input[[paste0(id, toString(2))]])
		}
		return(sum)
	})
	# alt3
	progress3 <- reactive({
		sum <- 0.0
		for (id in criteria_inputs){
			sum <- as.numeric(sum + input[[paste0(id, toString(3))]])
		}
		return(sum)
	})
	# alt4
	progress4 <- reactive({
		sum <- 0.0
		for (id in criteria_inputs){
			sum <- as.numeric(sum + input[[paste0(id, toString(4))]])
		}
		return(sum)
	})
	# alt5
	progress5 <- reactive({
		sum <- 0.0
		for (id in criteria_inputs){
			sum <- as.numeric(sum + input[[paste0(id, toString(5))]])
		}
		return(sum)
	})

	# alt1
	output[[paste0("Alt", 1,"Progress")]] <- renderUI(list(
		paste0("Progress for Alternative ", 1, ": "),
		if( progress1() > upper_bound || progress1() < lower_bound)
			tags$span(paste0(progress1(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# alt2
	output[[paste0("Alt", 2,"Progress")]] <- renderUI(list(
		paste0("Progress for Alternative ", 2, ": "),
		if( progress2() > upper_bound || progress2() < lower_bound)
			tags$span(paste0(progress2(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# alt3
	output[[paste0("Alt", 3,"Progress")]] <- renderUI(list(
		paste0("Progress for Alternative ", 3, ": "),
		if( progress3() > upper_bound || progress3() < lower_bound)
			tags$span(paste0(progress3(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# alt4
	output[[paste0("Alt", 4,"Progress")]] <- renderUI(list(
		paste0("Progress for Alternative ", 4, ": "),
		if( progress4() > upper_bound || progress4() < lower_bound)
			tags$span(paste0(progress4(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# alt5
	output[[paste0("Alt", 5,"Progress")]] <- renderUI(list(
		paste0("Progress for Alternative ", 5, ": "),
		if( progress5() > upper_bound || progress5() < lower_bound)
			tags$span(paste0(progress5(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))

	#--------------------------------------------------------------------------------
	# Alternative Update Event Listeners
	# these trigger the updates on button click
	#--------------------------------------------------------------------------------

	# ALTERNATIVE 1
	#----------------------------------------
	observeEvent(input$updateBtn1, {
		if(progress1() > upper_bound || progress1() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress1())
			))
		}else{
			 updateAlt1()
		}
	})

	# ALTERNATIVE 2
	#----------------------------------------
	observeEvent(input$updateBtn2, {
		if(progress2() > upper_bound || progress2() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress2())
			))
		}else{
			updateAlt2()
		}
	})

	# ALTERNATIVE 3
	#----------------------------------------
	observeEvent(input$updateBtn3, {
		if(progress3() > upper_bound || progress3() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress3())
			))
		}else{
			 updateAlt3()
		}
	})

	# ALTERNATIVE 4
	#----------------------------------------
	observeEvent(input$updateBtn4, {
		if(progress4() > upper_bound || progress4() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress4())
			))
		}else{
			updateAlt4()
		}
	})

	# ALTERNATIVE 5
	#----------------------------------------
	observeEvent(input$updateBtn5, {
		if(progress5() > upper_bound || progress5() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress5())
			))
		}else{
			updateAlt5()
		}
	})

	#--------------------------------------------------------------------------------
	# MCDA Table Output
	#--------------------------------------------------------------------------------
	# initial empty matrix.
	RawCriteriaMatrix  <- data.frame(matrix(data=NA, nrow=length(available_alternatives), ncol=length(criteria_inputs) ))

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
		# generate
		generateOutput()
	})

	# Downloadable csv of selected dataset ----
	output$downloadData <- downloadHandler(
		filename = function() {
			# format date & time in filename
			# date format( year, month, day, hour, minute, second, UTC offset )

		   format(Sys.time(), "dams_mcda_results_%Y-%m-%d_%H-%M-%S_%z.csv")
		},
		content = function(file) {
		   write.csv(
			response_data,
			file,
			row.names = TRUE,
			quote=TRUE
		   )
		}
	)


} # end server
