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
score_range <- c(0, 1)
# range of final graph of summed scores
summed_score_range <- c(0, 1)
# list of alternatives
available_alternatives <- seq(1:5)

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
	"Greenhouse Gas Emissions Reduction",
	"Indigenous Cultural Heritage",
	"Industrial Historical Value",
	"Town/City Identity",
	"Aesthetic Value"
)

# alternative display names (for labeling tables and graphs)
alternative_names <- c(
   "Remove Dam",
   "Fish Improve",
   "Turbine Improve",
   "Turbine Add or Expand",
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

# has to be global (this is data the user can download after finishing
response_data <<- ("no data")
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
	# new graph (ggplot2) requires a data frame not vectors
	if (is.vector(data)){
		df <- data.frame(Criteria=x_names, Score=data)
	}

	result <-  renderPlot(
		ggplot(
		  df,
		  aes(x=Criteria, y=Score, fill=Criteria)
		)
		+ geom_bar(stat="identity")
		+ geom_text(data=subset(df, Score != 0), aes(label=Score), color="white", hjust=1, vjust=0.4, size=6)
		+ geom_text(data=subset(df, Score == 0), aes(label=Score), color="black", hjust=-1, vjust=0.4, size=6)
		+ coord_flip()
		+ theme_minimal()
		+ theme(legend.position="none", text=element_text(size=20), )
		+ scale_x_discrete(limits=rev(x_names))
		+ ylab(y_label)
		+ xlab(x_label)
	)
	return(result)
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

	par("mar"=c(15.1, 4.1, 4.1, 2.1))

	#------------------------------------------------------------
	# updateAlt1
	# logic for updating alternative 1
	#------------------------------------------------------------
	updateAlt1 <- function (){
		# update the tab status
		output$Alt1 <- renderUI(list(
			'Alternative 1: Remove Dam',
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
			"Alternative 2: Improve Fish Passage Facilities",
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
			"Alternative 3: Upgrade or Replace Turbines",
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
			"Alternative 4: Install Turbines or Expand Power Capacity",
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
			names(RawCriteriaMatrix) <- criteria_names
			# origial scores in table form
			# for debugging table size
			output$FilledCriteriaTable <- renderTable(RawCriteriaMatrix, rownames=enable_rownames)

			#----------------------------------------
			# Call WSM and format response
			#----------------------------------------
			CritImportance <- alternatives/sum(alternatives)

			WSMResults <- WSM(CritImportance=CritImportance, RawCriteriaMatrix=RawCriteriaMatrix)

			TableMatrix <- WSMResults[1]

			TableMatrix$summedScore <- WSMResults[2]

			WSMTableOutput <- data.frame( TableMatrix, row.names=alternative_names, check.names=FALSE)
			# this ones different becaues
			names(WSMTableOutput) <- criteria_names_and_sum

			#----------------------------------------
			# Final Outputs
			#----------------------------------------
			# final output table
			#output$WSMTable <- renderTable(WSMTableOutput, rownames=enable_rownames)
			output$WSMTable <- renderTable(t(WSMTableOutput), rownames=enable_rownames)

			saveResponse(WSMTableOutput)

			# final output barplot
			output$WSMPlot <- renderBarPlot(
				# !important!
				unlist(WSMResults[2]), # scoresum data
				"Weighted Sum MCDA Ranked Alternatives", # title
				alternative_names, # x_labels
				"Alternative", # x axis label
				"Score", # y axis label
				colors, # colors
				NULL, # x value limit
				summed_score_range # y value limit (1-5 value range)
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
		# Initial Intro Text
		#----------------------------------------
		output$Introduction    <- renderText("This R Shiny app supports decision making about a dam, using a set of decision criteria and decision alternatives identified through stakeholder interviews*. The tool uses a Weighted Sum approach to Multi-Criteria Decision Analysis (MCDA) to compare decision-maker preferences for decision criteria over a set of decision alternatives.

		                                     Picture a watershed: rain falls and runs downhill toward tributaries that flow into the river and ultimately to the ocean. This watershed is home to valuable ecosystem services, including pristine natural lakes, clean water sources, and significant biodiversity, including several sea-run fish species (e.g. Atlantic salmon, American eel, Blueback herring, and Alewife). The river and its tributaries are home to many dams that also provide services, including reservoirs for drinking water and recreation, flood protection, and generation of reliable, on-demand renewable hydropower, critical to reducing emissions that contribute to climate change and poor human health. Dams across the U.S. are aging and pose potential safety hazards, increasing the need for regular maintenance or more extensive repair. Dams may interrupt flows and prevent sea-run fish passage, contributing to large population declines. They may also contribute to poor water quality downstream, increased predation, and climate change. Dams have long threatened indigenous cultural heritage in the U.S., while at the same time helping to shape post-industrial community identity over the last two centuries. To use a specific realistic decision context example situated in Maine's Penobscot River watershed, click here.

		                                     For this activity, imagine that the future of a watershed is directly in your hands. You (the decision maker) are personally tasked with using your professional expertise to make sustainable decisions for a set of dams within a given watershed. In this MCDA tool, you will select a numeric score for each decision criterion (e.g., annuitized cost, greenhouse gas emissions reductions, sea-run fish survival, etc.) involved in each decision alternative (e.g., remove dam, keep dam, improve fish passage facilities, etc.). The output from the MCDA tool will be a visual ranking of decision alternatives based on your preferences for decision criteria.

		                                     Your Task: What do you envision for the future of this set of dams? Your task is to consider each of the decision alternatives for decisions across the set of dams:
		                                     (1) Remove dam
		                                     (2) Improve fish passage facilities
		                                     (3) Upgrade or replace turbines
		                                     (4) Install turbines or expand power capacity
		                                     (5) Keep and maintain dam

		                                     Please also consider the following decision criteria, to be applied in some combination for the set of dams:

		                                     (1) Fish survival (thousands of lbs or metric tonnes per acre): proxy criteria estimated as sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) biomass calculated using functional habitat units (Roy et al., 2018).
		                                     (2) River recreation area (square miles or kilometers): estimated area of river that may increase or decrease with a dam decision alternative, combines functional area for whitewater and flatwater recreation defined by Roy et al. (2018).
		                                     (3) Reservoir storage (cubic miles or kilometers): estimated storage potential of the reservoir, based on its volume (Roy et al., 2018).
		                                     (4) Annuitized project costs (2018 $USD): estimated total project costs (capital and operation & maintenance) on an annual basis using a 10% discount rate.
		                                     (5) Number of properties impacted: estimated number based on potential changes in viewshed or property value (Roy et al., 2018).
		                                     (6) Breach damage potential (unitless): a proxy for safety based on the State hazard rating, which indicates the potential for downstream property damage, injury, and death in the case of dam breach.
		                                     (7) Annual electricity generation (MWh/yr): estimate based on nameplate capacity.
		                                     (8) Greenhouse gas emissions reduction (lbs or metric tonnes per year): estimated based on avoided fossil fuel-generated electricity, using the State's energy mix.
		                                     (9) Indigenous cultural heritage (unitless): to convey the importance of the decision alternative for preserving/restoring the culture of indigenous people.
		                                     (10) Town/city identity (unitless): rating provided by decision-maker to convey the importance of the decision alternative for preserving the existing identity of the community of town/city residents.
		                                     (11) Industrial historical value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for preserving/restoring the industrial historical value of the infrastructure.
		                                     (12) Aesthetic value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for improving or preserving the aesthetics (e.g, appearance, scenic value, smell, sound).

		                                     Toggle through the ALTERNATIVE pages at left to compare criteria under a single decision alternative and click UPDATE button to view alternative-specific results and mark the alternative COMPLETE. After you have finished rating criteria under every ALTERNATIVE tab, select the OUTPUT tab and click GENERATE to view the results.")

		output$Developers <- renderText("Emma Fox- Lead Developer (Ph.D. candidate, University of Maine Ecology and Environmental Science Program). Designed initial user interface and server functionality based on Raymond & Klein (2018). Adjusted WSM function for new dam decision application and advised model-related changes. Wrote app text, and designed accompanying multi-dam decision example fact sheets.
                                          Sharon Klein- Development Advisor (Associate Professor, University of Maine School of Economics). Advised user-friendliness enhancements to WSM model and UI/UX, refined criteria definitions, revised app text.
										  William Winslow - Developer  (Software Engineer, GeoSpatial Science Center(GSSC), University of New Hampshire). Deployment (Docker, Apache), server code reorganization, debugging/bug fixes, misc. feature implementations (UI/UX).
										  Garrett Raymond- Technical Consultant. Built WSM function in R and provided basic web app design. See also: Raymond, G. and Klein, S. (2018). Web App: Multi-Criteria Decision Analysis of Fuel Pathways.https://fuel-production-pathway-comparison-tool.shinyapps.io/gr_ui_sep_models/")
		output$Acknowledgment <- renderText("Support for the Future of Dams project provided by the National Science Foundation's Research Infrastructure Improvement NSF #IIA-1539071, USDA National Institute of Food and Agriculture, Hatch project 0230040, and Department of the Interior, U.S. Geological Survey Grant No. G16AP00057 through the Senator George J. Mitchell Center at the University of Maine.
											Data Discovery Center at the University of New Hampshire- host for the Shiny web app. https://ddc.unh.edu")

		#----------------------------------------
		# Initial Alternative Tabs Text
		#----------------------------------------
		output$Alt1 <- renderUI(list(
			"Alternative 1: Dam Removal",
			tags$span('Requires User Input', class="alt-not-complete")
		))
		output$Alt2 <- renderUI(list(
			"Alternative 2: Improve Fish Passage Facilities",
			tags$span('Requires User Input', class="alt-not-complete")
		))
		output$Alt3 <- renderUI(list(
			"Alternative 3: Upgrade or Replace Turbines at Existing Powered Dams",
			tags$span('Requires User Input', class="alt-not-complete")
		))
		output$Alt4 <- renderUI(list(
			"Alternative 4: Installing Turbines or Expanding Existing Capacity",
			tags$span('Requires User Input', class="alt-not-complete")
		))
		output$Alt5 <- renderUI(list(
			"Alternative 5: Refurbishment, Restoration, or Maintenance",
			tags$span('Requires User Input', class="alt-not-complete")
		))
		output$Alt6 <- renderUI(list(
			"Alternative 6: Keep Dam (Do Nothing)",
			tags$span('Requires User Input', class="alt-not-complete")
		))
	})

	#------------------------------------------------------------
	# setUp ProgressBars
	# reactive value for input progress
	#------------------------------------------------------------
	# alt1
	progress1 <- reactive({
		sum <- 0
		for (id in criteria_inputs){
			sum <- (sum + input[[paste0(id, toString(1))]])
		}
		return(sum)
	})
	output[[paste0("Alt", 1,"Progress")]] <- renderUI(list(
		paste0("Progress for Alternative ", 1, ": "),
		if( as.integer(progress1()) != 1)
			tags$span(paste0(progress1(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# alt2
	progress2 <- reactive({
		sum <- 0
		for (id in criteria_inputs){
			sum <- (sum + input[[paste0(id, toString(2))]])
		}
		return(sum)
	})
	output[[paste0("Alt", 2,"Progress")]] <- renderUI(list(
		paste0("Progress for Alternative ", 2, ": "),
		if( as.integer(progress2()) != 1)
			tags$span(paste0(progress2(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# alt3
	progress3 <- reactive({
		sum <- 0
		for (id in criteria_inputs){
			sum <- (sum + input[[paste0(id, toString(3))]])
		}
		return(sum)
	})
	output[[paste0("Alt", 3,"Progress")]] <- renderUI(list(
		paste0("Progress for Alternative ", 3, ": "),
		if( as.integer(progress3()) != 1)
			tags$span(paste0(progress3(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# alt4
	progress4 <- reactive({
		sum <- 0
		for (id in criteria_inputs){
			sum <- (sum + input[[paste0(id, toString(4))]])
		}
		return(sum)
	})
	output[[paste0("Alt", 4,"Progress")]] <- renderUI(list(
		paste0("Progress for Alternative ", 4, ": "),
		if( as.integer(progress4()) != 1)
			tags$span(paste0(progress4(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# alt5
	progress5 <- reactive({
		sum <- 0
		for (id in criteria_inputs){
			sum <- (sum + input[[paste0(id, toString(5))]])
		}
		return(sum)
	})
	output[[paste0("Alt", 5,"Progress")]] <- renderUI(list(
		paste0("Progress for Alternative ", 5, ": "),
		if( as.integer(progress5()) != 1)
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
		if( as.integer(progress1()) == 1){
			 updateAlt1()
		}else{
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress1())
			))
		}
	})

	# ALTERNATIVE 2
	#----------------------------------------
	observeEvent(input$updateBtn2, {
		if( as.integer(progress2()) == 1){
			 updateAlt2()
		}else{
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress2())
			))
		}
	})

	# ALTERNATIVE 3
	#----------------------------------------
	observeEvent(input$updateBtn3, {
		if( as.integer(progress3()) == 1){
			 updateAlt3()
		}else{
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress3())
			))
		}
	})

	# ALTERNATIVE 4
	#----------------------------------------
	observeEvent(input$updateBtn4, {
		if( as.integer(progress4()) == 1){
			 updateAlt4()
		}else{
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress4())
			))
		}
	})

	# ALTERNATIVE 5
	#----------------------------------------
	observeEvent(input$updateBtn5, {
		if( as.integer(progress5()) == 1){
			 updateAlt5()
		}else{
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress5())
			))
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
