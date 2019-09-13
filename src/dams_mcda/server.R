source("plots.R")
source("WSM.R")

#pull from WSM script
DamsData <- read.csv('DamsData.csv') #might delete later
DamsData <- data.frame(DamsData) #might delete later
source(file='f_raw.RData')
source(file = 'f_nrge2.RData') #these are the NORMALIZED dams data from Sam's MOGA fitness function, where the'levels' data are for all 995 'scenarios' of 8 dams, 5 decision alts/dam
NormalizedMatrix <- as.array(f_nrge)
#DamsData <- as.array(f)
source(file='Decisions.RData') #this is 2 dimensions from f_nrge: rows = 995 'scenarios' with their decision alternative code for each dam, cols = 8 dams
Decisions <- as.array(Decisions)# need this for graphing

library(abind)
library(plotly, warn.conflicts =  FALSE)
library(R.matlab)
library(rjson)

#--------------------------------------------------------------------------------
# Global Variables
#--------------------------------------------------------------------------------

#----------------------------------------
# Working Directories
# path where files are saved/opened (if any)
#----------------------------------------
#base_dir <- "/srv/shiny-server/dams_mcda/" # root
#response_dir <- paste(base_dir, "responses/", sep="") # where responses are saved
#working_dir <- paste(base_dir, "", sep="") # default
#setwd(working_dir) # initial directory
#responsesDir <- file.path(response_dir) # directory where responses get stored

#-----------------------------------------
# FILE/DATA STORAGE
#-----------------------------------------
max_file_size <- 5 # size in MB
options(shiny.maxRequestSize=max_file_size*1024^2)
# user download file (has to be global)
response_data <<- ("no data")

#-----------------------------------------
# App Specific
#-----------------------------------------
# list of dams
available_dams <- seq(1:8)
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
	"IndigenousLifeways",
	"IndustrialHistory",
	"CommunityIdentity",
	"Aesthetics",
	"Health",
	"Justice"
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
	"Indigenous Cultural Traditions and Lifeways",
	"Industrial Historical Value",
	"Community Identity",
	"Aesthetic Value",
	"Public Health",
	"Socio-Environmental Justice"
)

# alternative display names (for labeling tables and graphs)
alternative_names <- c(
  "Keep and Maintain Dam",
  "Improve Hydro",
  "Improve Fish Passage",
  "Improve Hydro AND Fish Passage",
  "Remove Dam"
)

# dam display names (for labeling tables and graphs)
dam_names <- c(
    "West Enfield Dam",
    "Medway Dam",
    "Millinocket/Quakish",
    "East Millinocket",
    "North Twin",
    "Dolby",
    "Millinocket Lake",
    "Ripogenus"
)

# append summed score to criteria_names array
criteria_names_and_sum <- as.list(criteria_names) # vector to list
criteria_names_and_sum[[length(criteria_names_and_sum) + 1]] <- "Summed Score" # append summed score
criteria_names_and_sum <- unlist(criteria_names_and_sum) # return to vector

#--------------------------------------------------
# User Interface Default Values
#--------------------------------------------------
# default graph color array
colors <- c("darkblue", "purple", "green", "red", "yellow", "orange", "pink")
# default graph score range
score_range <- c(0, 100)
# range of final graph of summed scores
summed_score_range <- c(0, 100)
# set to TRUE to show row names on tables
enable_rownames <<- TRUE

#----------------------------------------
# Preference Input Value Constraints
#----------------------------------------
# smallest input slider increment
smallest_increment <- 5
max_slider_value <- 100.0
# make valid progress values range smaller than the smallest increment
upper_bound <- (max_slider_value + (smallest_increment/2))
lower_bound <- (max_slider_value - (smallest_increment/2))
# for checking if all prefernces
total_upper_bound <- (length(available_dams) * max_slider_value + (smallest_increment/2))
total_lower_bound <- (length(available_dams) * max_slider_value - (smallest_increment/2))

#----------------------------------------
# Matlab
#----------------------------------------
# track matlab port for session
#session_matlab_port <- 9998
# for production make sure this is TRUE
#retry_matlab_connection <- TRUE
#max_retries <- 3

#--------------------------------------------------------------------------------
# End of global variables
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# Misc.
#--------------------------------------------------------------------------------
# fix R.plots.pdf error
# (see: https://stackoverflow.com/questions/36777416/plotly-plot-not-rendering-on-shiny-server)
# needed when calling barplot
pdf(NULL)

#--------------------------------------------------------------------------------
# Methods
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


# saveResponse
#----------------------------------------
# save the results to a file
saveResponse <- function(table_data) {
	response_data <<- table_data
}


# saveData
#----------------------------------------
# save the results to a file
saveData <- function(data) {
	fileName <- sprintf("ResultsRaw.csv", humanTime(), digest::digest(data))
	write.csv(x = data, file = file.path(responsesDir, fileName), row.names = FALSE, quote = TRUE)
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


# updateDamStatus
#----------------------------------------
# remove and refill progress of a status
# action is status to apply "remove" or "add"
updateDamStatus <- function(completed, action, id){
	message('------------------')
	message('updateDamStatus vector')
	message('------------------')

	if (id %in% completed & action == "remove"){
		completed <- completed[which(x==id)]
	}else if (action =="add" & !(id %in% completed)){
		completed <- c(completed, id)
	}else{
		#message('no Dam Status Changes')
	}
	return(completed)
}


# damsCompleted
#----------------------------------------
# check all available_Dams are in Dams_completed
# returns boolean
damsCompleted <- function(completed){
	message('------------------')
	message('bool damsCompleted(array_completed)')
	message('------------------')

	# available_dams is array of dam sections ids
	for (value in available_dams){
		if (!(value %in% completed)){
			return(FALSE)
		}
	}
	return(TRUE)
}



#--------------------------------------------------------------------------------
# SERVER
#--------------------------------------------------------------------------------
server <- function(input, output, session) {
	#------------------------------------------------------------
	# JS data passing test
	#------------------------------------------------------------
	# debug/validate authentication
	session$sendCustomMessage("validateSession", "any message")


	#------------------------------------------------------------
	# Session Mode
	#
	# 	on app init prompts a modal requiring the user to decide which mode
	#
	# 	for now mode can be either:
	# 		group or individual
	#   could be expanded to include different modes depending on the application state requirements
	#------------------------------------------------------------
	session_mode <<- "individual" # default mode of session

	intro_modal_visible <<- TRUE # intro modal is visible on page load
	upload_modal_visible <<- FALSE # file upload modal

	# choose individual/group modal
	showModal(
		modalDialog(
			title = "Group or Individual",
			footer=NULL, # NULL to disable dismiss button
			easyClose=FALSE, # False to disable closing by clicking outside of modal
			div(
				HTML( "<h4>Are you entering <b>(a) individual</b> or <b>(b) group</b> preference information?</h4>"),

				actionButton("selectIndividualSessionMode", "Individual Preferences"),
				actionButton("selectGroupSessionMode", "Group Preferences"),
				actionButton("uploadBtn", "UPLOAD DATA"),

				HTML(
					"<h4>Instructions for Uploading</h4>\
					Use this option only if you have done this activity before. Your input file should be in .CSV format, \
          and your data should be organized in 8 rows (dams) with 14 columns (decision criteria). Cells should be\
          populated with preference values for each criterion at each dam. Press the UPLOAD button, then browse and \
          select the appropriate .CSV file to upload for you or (if you are using the tool as part of a group) the \
					average preference values for the group. <br>"
				)
			)
		)
	)

	# track the user group
	# NOTE: only set when the user is using application in group input mode
	# this is important because changing the value of this variable causes effects
	observeEvent(input$session_user_group, {
		if (input$session_user_group == "false"){
			message("Group Mode Set with no group: ", input$session_user_group)
			# no group attached to user, but they select group mode!
		}else{
			message("Group Mode Set for group_id: ", input$session_user_group)
			removeModal()
		}
	})

	# userHasGroup will query the users group relation
	# will set the variable input$session_user_group on completion
	checkUserHasGroup <- function(){
		session$sendCustomMessage("checkUserHasGroup", "")
	}

	# on mode update
	setSessionMode <- function(newMode){
		session_mode <- newMode

		if (newMode == "group"){
			# check if user already picked a group
			checkUserHasGroup()
		}else if (intro_modal_visible){
			removeModal()
			intro_modal_visible <<- FALSE
		}
	}

	# on mode file upload
	pickUploadFile <- function(){
		# hide other modal
		if (intro_modal_visible){
			removeModal()
			intro_modal_visible <<- FALSE
		}

		# mark as visible
		upload_modal_visible <<- TRUE

		# upload file modal
		showModal(
			modalDialog(
				title = "File Upload",
				footer=NULL, # NULL to disable dismiss button
				easyClose=FALSE, # False to disable closing by clicking outside of modal
				div(
					HTML(
						"<h4>Instructions for Uploading</h4>\
						Use this option only if you have done this activity before and have used the blank decision matrix HERE to organize your data. Press the UPLOAD button, and select the appropriate .xlsx or .csv file to upload the preference values\
						for you or the average preference values for your group. <br><br>"
					),

					#TODO: add xlsx support?
					fileInput("file1",
						label=p(paste0("Upload File (Maximum size: ", max_file_size, " MB)")),
						width="100%",
						multiple=FALSE,
						accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
					),

					# confirm upload
					actionButton("confirmUploadBtn", width="100%", "Continue")
				)
			)
		)
	}


	# ----------------------------------------
	# uploadFile
	# validate and process a user uploaded file
	# ----------------------------------------
	uploadFile <- function(){
		# make sure user selected a file before trying to validate
		if(is.null(input$file1)){
			message("input file null")
			session$sendCustomMessage("noFileSelected", "any message")
			return(NA) # break (stop execution of function)
		}

		#message("uploaded file temp path: ", input$file1$datapath)

		# open the file locally
		df <- read.csv(input$file1$datapath,
             header = TRUE,
             sep = ",",
             quote = '"')

		# debug contents of file
		#message("file header: ", head(df, n=1))
		#message("file columns: ", length(head(df,n=1)), " rows: ", length(head(t(df), n=1)))

		# ------------------------------
		# verify contents of file
		# ------------------------------
		# check it has correct amount of columns and rows
		row_count <- length(head(t(df),n=1))
		column_count <- length(head(df,n=1))
		# TODO: set required_* variables to size of valid input
		required_rows <- 5
		required_cols <- 3

		# valid unless proven otherwise
		file_valid <- TRUE
		fail_reason <- "File is invalid:"

		if (row_count != required_rows){
			file_valid <- FALSE
			fail_reason <- paste(fail_reason, "incorrect number of rows", sep=" ")
		} else if (column_count != required_cols){
			file_valid <- FALSE
			fail_reason <- paste(fail_reason, "incorrect number of columns", sep=" ")
		}

		# if valid remove modal and process the file
		if (upload_modal_visible && file_valid){
			#message("file upload success")
			removeModal()
			upload_modal_visible <<- FALSE
			#TODO: process file here
		}else{
			# warn the user that the file is not acceptable
			#message("file upload fail", fail_reason)
			session$sendCustomMessage("invalidFileSelected", fail_reason)
		}

	}

	# select mode / file upload event listeners
	observeEvent(input$selectGroupSessionMode, { setSessionMode("group") })
	observeEvent(input$selectIndividualSessionMode, { setSessionMode("individual") })
	observeEvent(input$uploadBtn, { pickUploadFile() })
	observeEvent(input$confirmUploadBtn, { uploadFile() })


	#------------------------------------------------------------
	# initDamMap
	# initialize the leaflet map for displaying dam location/attributes
	# random dam as marker
	# random dam as center of viewport
	#------------------------------------------------------------
	map_data <- read.csv('dam_csv/map_dams.csv')

	# format on-hover labels to be header of dam name, followed by attribute table
	map_marker_table <- lapply(seq(nrow(map_data)), function(i){
		return(HTML(
			paste0(
				"<h4>",
				map_data[i, "Name"],
				"</h4>",
				"<table class='map-data'><tr><th>Attribute</th><th>Value</th></tr><tr><td>Single or Multi-Dam</td><td>",
				map_data[i, "Single_or_Multi"],
				"</td></tr><tr><td>Power Capacity (MW)</td><td>",
				map_data[i, "Power_Capacity"],
				"</td></tr><tr><td>Avg. Annual Electricity<br>Generation (GWh)</td><td>",
				map_data[i, "Average_Annual_Electricity_Generation"],
				"</td></tr><tr><td>Date Installed (Year)</td><td>",
				map_data[i, "Year_of_Installation"],
				"</td></tr></table>"
			)
		))
	})

	# render leaflet dam inspector map
	output$dam_map <- renderLeaflet({
		map <- leaflet(data=map_data) %>%
			addTiles() %>%
			addCircleMarkers(
				lat=~map_data[,4], lng=~map_data[,5],
				radius=6, # size
				color="black", # border color
				weight=1, # border stroke
				opacity=1, # line opacity
				fillColor="lime",
				fillOpacity=0.9,
				label=lapply(map_marker_table, htmltools::HTML),
				labelOptions=labelOptions(style=list("padding-left"="1.2em", "font-size"="1em")), # padding on label
				popup=lapply(map_marker_table, htmltools::HTML)
		    ) %>%
			setView(lng=-69.17626004, lat=45.88144746, zoom=8)
		map
	})


	#------------------------------------------------------------
	# updateDamGraph
	# given a dam index and vector of scores makes the raw score graphs
	# scoreVector needs to be vector and not data frame or it will repeat for each criteria
	#------------------------------------------------------------
	updateDamGraph <- function(damId, scoreVector){
		# decision criteria ids
		Criteria <- c(rep(criteria_names, times=length(1)))

		# two columns, score and criteria of score
		Data <- data.frame(score=scoreVector, criteria=Criteria)

		# Figure 1 raw pref plot
		output[[paste0("SummPlot", damId)]] <- renderBarPlot(
			Data, # data
			paste("Raw Preference Scores for", dam_names[damId], sep=" "), # title
			criteria_names, # x_labels
			"Topic", # x axis label
			"Score", # y axis label
			colors, # colors
			NULL, # x value limit
			score_range # y value limit (0-1 value range)
		)

		# Graph2

		#NOTE: ggplot2 Error Bar Example

		#output[[paste0("ErrorPlot", damId)]] <- renderBarErrorPlot(
		#	Data, # data
		#	paste("Raw Preference Scores for", dam_names[damId], sep=" "), # title
		#	criteria_names, # x_labels
		#	"Topic", # x axis label
		#	"Score", # y axis label
		#	colors, # colors
		#	NULL, # x value limit
		#	score_range, # y value limit (0-100 value range)
		#	0.1, # error amount
		#	0.4, # error bar width
		#	"red" # error bar color
		#)
	}


	#------------------------------------------------------------
	# getRawScores
	# helper method for generating RawCriteriaMatrix from input fields
	#------------------------------------------------------------
	getRawScores <- function(){
		dams <- vector("list")
		for (row_id in 1:length(available_dams)){
			q <- vector("list")

			for (id in criteria_inputs){
				input_name <- paste(id, toString(row_id), sep='')
				value <- input[[input_name]]
				q[[id]] <- value

				if (is.null(value)){
					# debug nulls, doesn't modify data
					message('input ', input_name, " isNull ")
				}
			}

			dams[[row_id]] <- unlist(q) # we want in c and not list
		}
		dams <- unlist(dams)
		return(dams)
	}


	#------------------------------------------------------------
	# getDamPreferences
	# given index of the dam in dam_names list returns user selected values for inputs
	#------------------------------------------------------------
	getDamPreferences <- function(damIndex){
		damPrefs <- c(
			input[[paste0("FishBiomass", damIndex)]],
			input[[paste0("RiverRec", damIndex)]],
			input[[paste0("Reservoir", damIndex)]],
			input[[paste0("ProjectCost", damIndex)]],
			input[[paste0("Safety", damIndex)]],
			input[[paste0("NumProperties", damIndex)]],
			input[[paste0("ElectricityGeneration", damIndex)]],
			input[[paste0("AvoidEmissions", damIndex)]],
			input[[paste0("IndigenousLifeways", damIndex)]],
			input[[paste0("IndustrialHistory", damIndex)]],
			input[[paste0("CommunityIdentity", damIndex)]],
			input[[paste0("Aesthetics", damIndex)]],
			input[[paste0("Health", damIndex)]],
			input[[paste0("Justice", damIndex)]]
		)
		return(damPrefs)
	}


	#------------------------------------------------------------
	# updateDam1
	# logic for updating West Enfield Dam
	#------------------------------------------------------------
	updateDam1 <- function (){
		damId <- 1
		# update the tab status
		output[[paste0("Dam", damId)]] <- renderUI(list(
			"Dam 1: West Enfield",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam1 <- as.vector(getDamPreferences(damId))

		# create table matrix
		Dam1_Table <- as.matrix(data.frame(Dam1))
		row.names(Dam1_Table) <- criteria_names
		names(Dam1_Table) <- "Raw Score"
		
		output$RawPrefsDam1 = DT::renderDataTable({
		  Dam1_Table
		})
		

		# update dam specific graphs
		updateDamGraph(damId, Dam1)
		# make the container of those graphs visible
		shinyjs::show(id="dam-1-output")

		# mark the alternative as complete when update
		# or apply logic here to make other contstraints for "complete"
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 1)
	}


	#------------------------------------------------------------
	# updateDam2
	# logic for updating Medway Dam
	#------------------------------------------------------------
	updateDam2 <- function() {
		damId <- 2
		output[[paste0("Dam", damId)]] <- renderUI(list(
			"Dam 2: Medway Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam2 <- getDamPreferences(damId)

		# create table matrix
		Dam2_Table <- as.matrix(data.frame(Dam2))
		row.names(Dam2_Table) <- criteria_names
		names(Dam2_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(damId, Dam2)
		# make the container of those graphs visible
		shinyjs::show(id="dam-2-output")

		# mark the dam as complete when update
		# or apply logic here to make other contstraints for "complete"
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 2)
	}


	#------------------------------------------------------------
	# updateDam3
	# logic for updating Millinocket Dam
	#------------------------------------------------------------
	updateDam3 <- function() {
		damId <- 3
		output[[paste0("Dam", damId)]] <- renderUI(list(
			"Dam 3: Millinocket Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam3 <- getDamPreferences(damId)

		# create table matrix
		Dam3_Table <- as.matrix(data.frame(Dam3))
		row.names(Dam3_Table) <- criteria_names
		names(Dam3_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(damId, Dam3)
		# make the container of those graphs visible
		shinyjs::show(id="dam-3-output")

		# mark the dam as complete when update
		# or apply logic here to make other contstraints for "complete"
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 3)
	}


	#------------------------------------------------------------
	# updateDam4
	# logic for updating East Millinocket Dam
	#------------------------------------------------------------
	updateDam4 <- function() {
		damId <- 4
		output[[paste0("Dam", damId)]] <- renderUI(list(
			"Dam 4: East Millinocket Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam4 <- getDamPreferences(damId)

		# create table matrix
		Dam4_Table <- as.matrix(data.frame(Dam4))
		row.names(Dam4_Table) <- criteria_names
		names(Dam4_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(damId, Dam4)
		# make the container of those graphs visible
		shinyjs::show(id="dam-4-output")

		# mark the alternative as complete when update
		# or apply logic here to make other contstraints for "complete"
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 4)
	}


	#------------------------------------------------------------
	# updateDam5
	# logic for updating North Twin Dam
	#------------------------------------------------------------
	updateDam5 <- function() {
		damId <- 5
		output[[paste0("Dam", damId)]] <- renderUI(list(
			"Dam 5: North Twin Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam5 <- getDamPreferences(damId)

		# create table matrix
		Dam5_Table <- as.matrix(data.frame(Dam5))
		row.names(Dam5_Table) <- criteria_names
		names(Dam5_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(damId, Dam5)
		# make the container of those graphs visible
		shinyjs::show(id="dam-5-output")

		# mark the dam as complete when update
		# or apply logic here to make other contstraints for "complete"
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 5)
	}


	#------------------------------------------------------------
	# updateDam6
	# logic for updating Dolby Dam
	#------------------------------------------------------------
	updateDam6 <- function() {

		damId <- 6
		output[[paste0("Dam", damId)]] <- renderUI(list(
			"Dam 6:Dolby Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam6 <- getDamPreferences(damId)

		# create table matrix
		Dam6_Table <- as.matrix(data.frame(Dam6))
		row.names(Dam6_Table) <- criteria_names
		names(Dam6_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(damId, Dam6)
		# make the container of those graphs visible
		shinyjs::show(id="dam-6-output")

		# mark the dam as complete when update
		# or apply logic here to make other contstraints for "complete"
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 6)
	}


	#------------------------------------------------------------
	# updateDam7
	# logic for updating Millinocket Lake Dam
	#------------------------------------------------------------
	updateDam7 <- function() {
		damId <- 7
		output[[paste0("Dam", damId)]] <- renderUI(list(

			"Dam 7: Millinocket Lake Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam7 <- getDamPreferences(damId)

		# create table matrix
		Dam7_Table <- as.matrix(data.frame(Dam7))
		row.names(Dam7_Table) <- criteria_names
		names(Dam7_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(damId, Dam7)
		# make the container of those graphs visible
		shinyjs::show(id="dam-7-output")

		# mark the dam as complete when update
		# or apply logic here to make other contstraints for "complete"
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 7)
	}


	#------------------------------------------------------------
	# updateDam8
	# logic for updating Ripogenus Dam
	#------------------------------------------------------------
	updateDam8 <- function() {
		damId <- 8

		output[[paste0("Dam", damId)]] <- renderUI(list(
			"Dam 8: Ripogenus Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam8 <- getDamPreferences(damId)

		# create table matrix
		Dam8_Table <- as.matrix(data.frame(Dam8))
		row.names(Dam8_Table) <- criteria_names
		names(Dam8_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(8, Dam8)
		# make the container of those graphs visible
		shinyjs::show(id="dam-8-output")

		# mark the dam as complete when update
		# or apply logic here to make other contstraints for "complete"
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 8)
	}


	#------------------------------------------------------------
	# generateOutput
	# generate the final table and barplot
	#------------------------------------------------------------
	generateOutput <- function (){

	    if ( !damsCompleted(session$userData[['dams_completed']]) ){
			# user isnt finished filling out dams
			showModal(modalDialog(
				title = "Not Finished!",
				'Please complete all dams tabs before generating results'
			))
		}else{
			#------------------------------------------------------------
			# get 2d array of values based on length/values of criteria_inputs and available_dams
			# criterion -> columns
			# dams -> rows
			# example 14 criterion 8 dams results in 14 column (criteria) by 8 row (dams) 2d data structure 
			#------------------------------------------------------------

			dams <- vector("list")
			for (row_id in 1:length(available_dams)){
				# for each criterion for dam
				q <- vector("list")

				for (id in criteria_inputs){
					input_name <- paste(id, toString(row_id), sep='')
					value <- input[[input_name]]
					q[[id]] <- value

					if (is.null(value)){
						# debug nulls, doesn't modify data
						message('input ', input_name, " isNull ")
					}
				}
				dams[[row_id]] <- unlist(q) # we want in c and not list
			}
			dams <- unlist(dams)

			#NOT SURE HOW TO RECONCILE THIS SPECIFIC TO EACH INDIVIDuaL DAM
			#for alternatives in tables/graphs, this generates a blank vector with labels
			#alternatives <- vector("list", length(available_alternatives))
			#for (row_id in 1:length(available_alternatives)){
			#  # for each criterion in alternatives
			#  r <- vector("list", length(available_alternatives))

			#  for (id in criteria_inputs){
			#    input_name <- paste(id, toString(row_id), sep='')
			#    value <- input[[input_name]]
			#    r[[id]] <- value

			#    if (is.null(value)){
			#      # debug nulls, doesn't modify data
			#      message('input ', input_name, " isNull ")
			#    }
			#    alternatives[[row_id]] <- unlist(r)
			#  }
			#  alternatives <- unlist(alternatives)
			#}

			# -------------------------------------------------------------#
			# assign values in new matrix
			RawCriteriaMatrix <- data.frame(
				matrix(getRawScores(), nrow=length(available_dams), byrow=length(criteria_inputs))
			)
			# assign table row, column names
			row.names(RawCriteriaMatrix) <- dam_names
			colnames(RawCriteriaMatrix) <- criteria_names
			#message("generateOutput RawCriteriaMatrix: ", RawCriteriaMatrix)

			# origial scores in table form
			# for debugging table size
			# invert matrix to better fit on screen for mobile users
			#output$FilledCriteriaTable <- renderTable(t(RawCriteriaMatrix), rownames=enable_rownames)


			WSMResults <- WSM(RawCriteriaMatrix, NormalizedMatrix, DamsData, Decisions)

			WSMMatrix <- array(unlist(WSMResults[1]), dim=c(40,14))
			WSMTableOutput <- data.frame(WSMMatrix)
			#message("server got results from WSM ", WSMMatrix, " DIM: ", dim(WSMMatrix), " class ", class(WSMMatrix))

			WSMSummedScore <- array(unlist(WSMResults[3]), dim=c(8,5))
			#message("server got results from WSMSummedScore ", WSMSummedScore, " DIM: ", dim(WSMSummedScore), " class ", class(WSMSummedScore))

			map_name <- WSMResults[4]
			#message("WSM map name: ", map_name, " type ", class(map_name))
			shinyjs::html("MapRecommendation", paste0("<img src='", map_name, "'>"))

			#message("WSMTableOutput Matrix: ", dim(WSMMatrix), " summedScore: ", dim(WSMSummedScore))
			#message("WSMTableOutput length(dam_names): ", length(dam_names))

			#----------------------------------------
			# Final Outputs
			#----------------------------------------

			# Preference scores for all dams
			output$FilledCriteriaGraph <- renderCombinedBarPlot(
				RawCriteriaMatrix, # data
				"Preferences for all dams", # title
				criteria_names, # x_labels
				"Criteria", # x axis label
				"Score", # y axis label
				colors, # colors
				NULL, # x value limit
				NULL # y value limit (Unknown in this case)
			)

			# Preference scores by criteria
			output$FilledCriteriaGraph2 <- renderCombinedBarPlot2(
				RawCriteriaMatrix, # data
				"Preferences for all dams", # title
				criteria_names, # x_labels
				"Criteria", # x axis label
				"Score", # y axis label
				colors, # colors
				NULL, # x value limit
				NULL # y value limit (Unknown in this case)
			)

			# weighted scores
			output$WSMTable <- renderTable(WSMTableOutput, rownames=enable_rownames)
			message("WSMTableOutput: ", WSMTableOutput)

			message('saveResponse')
			#saveResponse(WSMTableOutput)

			## stacked bars data table
			Alternative <- c(rep(alternative_names, each=length(criteria_names)))
			Criteria <- c(rep(criteria_names, times=length(alternative_names)))
			#Score <- alternatives
			#Data <- data.frame(Alternative, Criteria, Score)

			# show output html elements (as of now generateOutput does all individual dams + combined)
			shinyjs::show(id="generated-output-1")
			shinyjs::show(id="generated-output-2")
			shinyjs::show(id="generated-output-3")
			shinyjs::show(id="generated-output-4")
			shinyjs::show(id="generated-output-5")
			shinyjs::show(id="generated-output-6")
			shinyjs::show(id="generated-output-7")
			shinyjs::show(id="generated-output-8")
			shinyjs::show(id="combined-output")
			message("generateOutput done")
		}
	}


	#--------------------------------------------------------------------------------
	# Initial Application State for session
	#--------------------------------------------------------------------------------
	observe(autoDestroy=TRUE, {
		# hide output html elements
		shinyjs::hide(id="generated-output")
		shinyjs::hide(id="dam-1-output")
		shinyjs::hide(id="dam-2-output")
		shinyjs::hide(id="dam-3-output")
		shinyjs::hide(id="dam-4-output")
		shinyjs::hide(id="dam-5-output")
		shinyjs::hide(id="dam-6-output")
		shinyjs::hide(id="dam-7-output")
		shinyjs::hide(id="dam-8-output")
		shinyjs::hide(id="generated-output-1")
		shinyjs::hide(id="generated-output-2")
		shinyjs::hide(id="generated-output-3")
		shinyjs::hide(id="generated-output-4")
		shinyjs::hide(id="generated-output-5")
		shinyjs::hide(id="generated-output-6")
		shinyjs::hide(id="generated-output-7")
		shinyjs::hide(id="generated-output-8")
		shinyjs::hide(id="combined-output")

		#----------------------------------------
		# Keep track of completed sections
		#----------------------------------------
		session$userData[['dams_completed']] <- c()

		#----------------------------------------
		# Initial Dam Tabs Text
		#----------------------------------------
		output$Dam1 <- renderUI(list(
			"Dam 1: West Enfield Dam",
			tags$span('Requires User Input', class="dam-not-complete")
		))
		output$Dam2 <- renderUI(list(
			"Dam 2: Medway Dam",
			tags$span('Requires User Input', class="dam-not-complete")
		))
		output$Dam3 <- renderUI(list(
			"Dam 3: Millinocket Dam",
			tags$span('Requires User Input', class="dam-not-complete")
		))
		output$Dam4 <- renderUI(list(
			"Dam 4: East Millinocket Dam",
			tags$span('Requires User Input', class="dam-not-complete")
		))
		output$Dam5 <- renderUI(list(
			"Dam 5: North Twin Dam",
			tags$span('Requires User Input', class="dam-not-complete")
		))
		output$Dam6 <- renderUI(list(
		  "Dam 6: Dolby Dam",
		  tags$span('Requires User Input', class="dam-not-complete")
		))
		output$Dam7 <- renderUI(list(
		  "Dam 7: Millinocket Lake Dam",
		  tags$span('Requires User Input', class="dam-not-complete")
		))
		output$Dam8 <- renderUI(list(
		  "Dam 8: Ripogenus Dam",
		  tags$span('Requires User Input', class="dam-not-complete")
		))
	})

	#------------------------------------------------------------
	# setUp ProgressBars
	# reactive value for input progress
	#------------------------------------------------------------
	# dam1
	progress1 <- reactive({
		sum <- 0.0
		for (id in criteria_inputs){
			sum <- as.numeric(sum + input[[paste0(id, toString(1))]])
		}
		return(sum)
	})
	# dam2
	progress2 <- reactive({
		sum <- 0.0
		for (id in criteria_inputs){
			sum <- as.numeric(sum + input[[paste0(id, toString(2))]])
		}
		return(sum)
	})
	# dam3
	progress3 <- reactive({
		sum <- 0.0
		for (id in criteria_inputs){
			sum <- as.numeric(sum + input[[paste0(id, toString(3))]])
		}
		return(sum)
	})
	# dam4
	progress4 <- reactive({
		sum <- 0.0
		for (id in criteria_inputs){
			sum <- as.numeric(sum + input[[paste0(id, toString(4))]])
		}
		return(sum)
	})
	# dam5
	progress5 <- reactive({
		sum <- 0.0
		for (id in criteria_inputs){
			sum <- as.numeric(sum + input[[paste0(id, toString(5))]])
		}
		return(sum)
	})
	# dam6
	progress6 <- reactive({
	  sum <- 0.0
	  for (id in criteria_inputs){
	    sum <- as.numeric(sum + input[[paste0(id, toString(6))]])
	  }
	  return(sum)
	})
	# dam7
	progress7 <- reactive({
	  sum <- 0.0
	  for (id in criteria_inputs){
	    sum <- as.numeric(sum + input[[paste0(id, toString(7))]])
	  }
	  return(sum)
	})
	# dam8
	progress8 <- reactive({
	  sum <- 0.0
	  for (id in criteria_inputs){
	    sum <- as.numeric(sum + input[[paste0(id, toString(8))]])
	  }
	  return(sum)
	})
	# list of dams
	available_dams <- seq(1:8)

	# alls dams
	total_progress <- reactive({
		sum <- 0.0
		for (index in 1:length(available_dams)){
			for (id in criteria_inputs){
				sum <- as.numeric(sum + input[[paste0(id, toString(index))]])
			}
		}
		return(sum)
	})

	# Total Progress
	output[["TotalProgress"]] <- renderUI({
		parts <- list("Total Progress: ", NA, "%")
		# sum of all preferences for each dam
		progress <- total_progress()
		# percent complete
		progress_pct <- as.integer(progress / length(available_dams))

		if( progress > total_upper_bound || progress < total_lower_bound){
			parts[2] <- paste0('<span class="not-complete">', progress_pct, '</span>')
		}else{
			parts[2] <- paste0('<span class="complete">', progress_pct, '</span>')
		}
		# doesnt parse as html by default so force it
		return(HTML(unlist(parts)))
	})
	# Dam1
	output[[paste0("Dam", 1, "Progress")]] <- renderUI(list(
		paste0("Progress for Dam ", 1, ": "),
		if( progress1() > upper_bound || progress1() < lower_bound)
			tags$span(paste0(progress1(), " / 100"), class="not-complete")
		else
			tags$span("100 / 100", class="complete")
	))
	# Dam2
	output[[paste0("Dam", 2, "Progress")]] <- renderUI(list(
		paste0("Progress for Dam ", 2, ": "),
		if( progress2() > upper_bound || progress2() < lower_bound)
			tags$span(paste0(progress2(), " / 100"), class="not-complete")
		else
			tags$span("100 / 100", class="complete")
	))
	# Dam3
	output[[paste0("Dam", 3, "Progress")]] <- renderUI(list(
		paste0("Progress for Dam ", 3, ": "),
		if( progress3() > upper_bound || progress3() < lower_bound)
			tags$span(paste0(progress3(), " / 100"), class="not-complete")
		else
			tags$span("100 / 100", class="complete")
	))
	# Dam4
	output[[paste0("Dam", 4, "Progress")]] <- renderUI(list(
		paste0("Progress for Dam ", 4, ": "),
		if( progress4() > upper_bound || progress4() < lower_bound)
			tags$span(paste0(progress4(), " / 100"), class="not-complete")
		else
			tags$span("100 / 100", class="complete")
	))
	# Dam5
	output[[paste0("Dam", 5, "Progress")]] <- renderUI(list(
		paste0("Progress for Dam ", 5, ": "),
		if( progress5() > upper_bound || progress5() < lower_bound)
			tags$span(paste0(progress5(), " / 100"), class="not-complete")
		else
			tags$span("100 / 100", class="complete")
	))
	# Dam6
	output[[paste0("Dam", 6, "Progress")]] <- renderUI(list(
	  paste0("Progress for Dam ", 6, ": "),
	  if( progress6() > upper_bound || progress6() < lower_bound)
		tags$span(paste0(progress6(), " / 100"), class="not-complete")
	  else
		tags$span("100 / 100", class="complete")
	))
	# Dam7
	output[[paste0("Dam", 7, "Progress")]] <- renderUI(list(
	  paste0("Progress for Dam ", 7, ": "),
	  if( progress7() > upper_bound || progress7() < lower_bound)
		tags$span(paste0(progress7(), " / 100"), class="not-complete")
	  else
		tags$span("100 / 100", class="complete")
	))
	# Dam8
	output[[paste0("Dam", 8, "Progress")]] <- renderUI(list(
	  paste0("Progress for Dam ", 8, ": "),
	  if( progress8() > upper_bound || progress8() < lower_bound)
		tags$span(paste0(progress8(), " / 100"), class="not-complete")
	  else
		tags$span("100 / 100", class="complete")
	))

	# update button progress trackers
	output[[paste0("UpdateDam", 1, "Progress")]] <- renderUI(list(
		paste0("Progress for Dam ", 1, ": "),
		if( progress1() > upper_bound || progress1() < lower_bound)
			tags$span(paste0(progress1(), " / 100"), class="not-complete")
		else
			tags$span("100 / 100", class="complete")
	))
	output[[paste0("UpdateDam", 2, "Progress")]] <- renderUI(list(
		paste0("Progress for Dam ", 2, ": "),
		if( progress2() > upper_bound || progress2() < lower_bound)
			tags$span(paste0(progress2(), " / 100"), class="not-complete")
		else
			tags$span("100 / 100", class="complete")
	))
	output[[paste0("UpdateDam", 3, "Progress")]] <- renderUI(list(
		paste0("Progress for Dam ", 3, ": "),
		if( progress3() > upper_bound || progress3() < lower_bound)
			tags$span(paste0(progress3(), " / 100"), class="not-complete")
		else
			tags$span("100 / 100", class="complete")
	))
	output[[paste0("UpdateDam", 4, "Progress")]] <- renderUI(list(
		paste0("Progress for Dam ", 4, ": "),
		if( progress4() > upper_bound || progress4() < lower_bound)
			tags$span(paste0(progress4(), " / 100"), class="not-complete")
		else
			tags$span("100 / 100", class="complete")
	))
	output[[paste0("UpdateDam", 5, "Progress")]] <- renderUI(list(
		paste0("Progress for Dam ", 5, ": "),
		if( progress5() > upper_bound || progress5() < lower_bound)
			tags$span(paste0(progress5(), " / 100"), class="not-complete")
		else
			tags$span("100 / 100", class="complete")
	))
	output[[paste0("UpdateDam", 6, "Progress")]] <- renderUI(list(
	  paste0("Progress for Dam ", 6, ": "),
	  if( progress6() > upper_bound || progress6() < lower_bound)
		tags$span(paste0(progress6(), " / 100"), class="not-complete")
	  else
		tags$span("100 / 100", class="complete")
	))
	output[[paste0("UpdateDam", 7, "Progress")]] <- renderUI(list(
	  paste0("Progress for Dam ", 7, ": "),
	  if( progress7() > upper_bound || progress7() < lower_bound)
		tags$span(paste0(progress7(), " / 100"), class="not-complete")
	  else
		tags$span("100 / 100", class="complete")
	))
	output[[paste0("UpdateDam", 8, "Progress")]] <- renderUI(list(
	  paste0("Progress for Dam ", 8, ": "),
	  if( progress8() > upper_bound || progress8() < lower_bound)
		tags$span(paste0(progress8(), " / 100"), class="not-complete")
	  else
		tags$span("100 / 100", class="complete")
	))

	#--------------------------------------------------------------------------------
	# User Step 1 Event Listeners
	# these trigger on button click
	#--------------------------------------------------------------------------------

	# Individual vs. Group path
	#----------------------------------------
	observeEvent(input$IndividualBtn, {

	})

	observeEvent(input$GroupBtn, {

	})

	observeEvent(input$UploadBtn, {

	})

	#--------------------------------------------------------------------------------
	# Dam Update Event Listeners
	# these trigger the updates on button click
	#--------------------------------------------------------------------------------

	# West Enfield
	observeEvent(input$updateBtn1, {
		message("update button 1")
		if(progress1() > upper_bound || progress1() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 100! Currently the sum is: ', progress1())
			))
		}else{
			 updateDam1()
		}
	})

	# Medway
	observeEvent(input$updateBtn2, {
		if(progress2() > upper_bound || progress2() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 100! Currently the sum is: ', progress2())
			))
		}else{
			updateDam2()
		}
	})

	# Millinocket
	observeEvent(input$updateBtn3, {
		if(progress3() > upper_bound || progress3() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 100! Currently the sum is: ', progress3())
			))
		}else{
			 updateDam3()
		}
	})

	# East Millinocket
	observeEvent(input$updateBtn4, {
		if(progress4() > upper_bound || progress4() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 100! Currently the sum is: ', progress4())
			))
		}else{
			updateDam4()
		}
	})

	# North Twin
	observeEvent(input$updateBtn5, {
		if(progress5() > upper_bound || progress5() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 100! Currently the sum is: ', progress5())
			))
		}else{
			updateDam5()
		}
	})

	# Dolby
	observeEvent(input$updateBtn6, {
	  if(progress6() > upper_bound || progress6() < lower_bound){
	    showModal(modalDialog(
	      title = "Not Finished!",
	      paste0('The sum of all sliders must be equal to 100! Currently the sum is: ', progress6())
	    ))
	  }else{
	    updateDam6()
	  }
	})

	# Millinocket Lake
	observeEvent(input$updateBtn7, {
	  if(progress7() > upper_bound || progress7() < lower_bound){
	    showModal(modalDialog(
	      title = "Not Finished!",
	      paste0('The sum of all sliders must be equal to 100! Currently the sum is: ', progress7())
	    ))
	  }else{
	    updateDam7()
	  }
	})

	# Ripogenus Dam
	observeEvent(input$updateBtn8, {
	  if(progress8() > upper_bound || progress8() < lower_bound){
	    showModal(modalDialog(
	      title = "Not Finished!",
	      paste0('The sum of all sliders must be equal to 100! Currently the sum is: ', progress8())
	    ))
	  }else{
	    updateDam8()
	  }
	})


	#--------------------------------------------------------------------------------
	# MCDA Table Output
	#--------------------------------------------------------------------------------
	# initial empty matrix.
	RawCriteriaMatrix  <- data.frame(matrix(data=NA, nrow=length(available_dams), ncol=length(criteria_inputs) ))


	# on 'Output > Generate' button event: fill matrix with user input values
	observeEvent(input$generateMatrix1, {
		generateOutput()
	})   # end 'output' tab > on generate button event


	# on 'Output > Generate' button event: fill matrix with user input values
	observeEvent(input$generateCombinedMatrix, {
		generateOutput()
	})   # end 'output' tab > on generate button event

	observeEvent(input$testWSM, {
		message("testWSM")
		# source and pass data do wsm function

		raw_scores <- getRawScores()

		# assign values in new matrix
		RawCriteriaMatrix <- data.frame(
			matrix(raw_scores, nrow=length(available_dams), byrow=length(criteria_inputs))
		)

		NormalizedMatrix <- as.array(f_nrge)
		results <- WSM(RawCriteriaMatrix, NormalizedMatrix, DamsData, Decisions)
		message("server got results from WSM")
		# end source and pass data do wsm function
	})

	#NOTE: this is for fast debugging output results
	observeEvent(input$autoGenerateMatrix, {
		message('Auto Generate')
		# update all alt
		updateDam1()
		updateDam2()
		updateDam3()
		updateDam4()
		updateDam5()
		updateDam6()
		updateDam7()
		updateDam8()
		# generate
		generateOutput()
	})


	#NOTE: this is for testing shiny > js > django
	observeEvent(input$saveResultsToDjango, {

		raw_scores <- getRawScores()

		# assign values in new matrix
		RawCriteriaMatrix <- data.frame(
			matrix(raw_scores, nrow=length(available_dams), byrow=length(criteria_inputs))
		)

		# assign table row, column names
		row.names(RawCriteriaMatrix) <- dam_names
		colnames(RawCriteriaMatrix) <- criteria_names

		MatrixJson <- toJSON(RawCriteriaMatrix)

		# send the data through javascript
		session$sendCustomMessage("saveResultsToDjango", toString(MatrixJson))
	})


	#--------------------------------------------------------
	# Downloads
	#--------------------------------------------------------

	# Downloadable csv of selected dataset
	output$downloadData1 <- downloadHandler(
		filename = function() {
			# format date & time in filename
			# date format( year, month, day, hour, minute, second, UTC offset )
			format(Sys.time(), "WestEnfield_mcda_results_%Y-%m-%d_%H-%M-%S_%z.csv")
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

	output$downloadData2 <- downloadHandler(
	  filename = function() {
	    # format date & time in filename
	    # date format( year, month, day, hour, minute, second, UTC offset )
	    format(Sys.time(), "Medway_mcda_results_%Y-%m-%d_%H-%M-%S_%z.csv")
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

	output$downloadData3 <- downloadHandler(
	  filename = function() {
	    format(Sys.time(), "Millinocket_mcda_results_%Y-%m-%d_%H-%M-%S_%z.csv")
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

	output$downloadData4 <- downloadHandler(
	  filename = function() {
	    format(Sys.time(), "EastMillinocket_mcda_results_%Y-%m-%d_%H-%M-%S_%z.csv")
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

	output$downloadData5 <- downloadHandler(
	  filename = function() {
	    format(Sys.time(), "NorthTwin_mcda_results_%Y-%m-%d_%H-%M-%S_%z.csv")
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

	output$downloadData6 <- downloadHandler(
	  filename = function() {
	    format(Sys.time(), "Dolby_mcda_results_%Y-%m-%d_%H-%M-%S_%z.csv")
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

	output$downloadData7 <- downloadHandler(
	  filename = function() {
	    format(Sys.time(), "MillinocketLake_mcda_results_%Y-%m-%d_%H-%M-%S_%z.csv")
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

	output$downloadData8 <- downloadHandler(
	  filename = function() {
	    format(Sys.time(), "Ripogenus_mcda_results_%Y-%m-%d_%H-%M-%S_%z.csv")
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

