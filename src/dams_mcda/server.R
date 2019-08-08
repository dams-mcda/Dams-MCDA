# barPlot wrappers
source("plots.R")
library(plotly, warn.conflicts = FALSE)
library(R.matlab) 
set.seed(123)


# track matlab port for session
session_matlab_port <- 9998

# for production make sure this is TRUE
retry_matlab_connection <- TRUE
max_retries <- 3


runMatlab <- function(port=9998, attempt=1){
	# launches a matlab server, trys to connect for up to 30 seconds
	# if it fails it will try a different port
	# failed connections can take a long time due to the ~30 second possible connection time
	out <- tryCatch({
		# try starting a server
		Matlab$startServer(
			matlab="/usr/local/MATLAB/R2019a/bin/matlab",
			workdir='/srv/matlab-working-dir',
			port=port
	    )
		matlab <- Matlab(port=port)

		# for debugging R.Matlab connection
		# set to -2 for max verbosity
		#setVerbose(matlab, threshold = 0)

		isOpen <- open(matlab)

		if (!(isOpen)){
			throw("ERROR MATLAB server is not running: waited 30 seconds.")
		}else{
			message("--------------------------------------------------------------------------------")
			message("matlab is open: ", matlab)
			message("--------------------------------------------------------------------------------")

			#----------------------------------------
			# Required Functions
			#----------------------------------------
			mat_filepath <- file.path("/media/SamMATLAB", "Filesfrom_05062019mtg.mat");
			data <- readMat(mat_filepath)


			# things referenced in MutliRank.m

			#m_filepath <- file.path("/media/SamMATLAB", "DPPF_idx.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#m_filepath <- file.path("/media/SamMATLAB", "MultiRank_bydam_prefUnityCheck.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#m_filepath <- file.path("/media/SamMATLAB", "MultiRank_bydam_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			## (requires DamIndex to be set)
			#m_filepath <- file.path("/media/SamMATLAB", "DPPF_netOV_split_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			## (requires DamIndex to be set)
			#m_filepath <- file.path("/media/SamMATLAB", "DPPFwkshp_fitfn_split.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#m_filepath <- file.path("/media/SamMATLAB", "DPPFwkshp_fitfn_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			## (requires DamIndex to be set)
			#m_filepath <- file.path("/media/SamMATLAB", "DPPFwkshp_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#m_filepath <- file.path("/media/SamMATLAB", "DPPF_netOV_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#m_filepath <- file.path("/media/SamMATLAB", "DPPFwkshp_prep_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#----------------------------------------
			# Application Entrypoint "Instructions"
			#----------------------------------------

			#base_m_filepath <- file.path("/media/SamMATLAB", "INSTRUCTIONS_minimum.m");
			#m_file_as_str <- readChar(base_m_filepath, file.info(base_m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			## "instruction file" has been split into multiple files
			#base_m_filepath <- file.path("/media/SamMATLAB", "INSTRUCTIONS_minimum2.m");
			#m_file_as_str <- readChar(base_m_filepath, file.info(base_m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			base_m_filepath <- file.path("/media/SamMATLAB", "MultiRank.m");
			m_file_as_str <- readChar(base_m_filepath, file.info(base_m_filepath)$size)
			setFunction(matlab, m_file_as_str)

			eval_string = sprintf("idxRank=MultiRank(f,pref,varargin);")
			evaluate(matlab, eval_string)
			close(matlab)

			#eval_string = sprintf( "[DamIndex, x, out]=RunApplication2();")
			#evaluate(matlab, eval_string)
			#close(matlab)

			message("--------------------------------------------------------------------------------")
			message("matlab closed")
			message("--------------------------------------------------------------------------------")
			return(TRUE) # sucess
		}
	},
	error=function(cond){
		message("--------------------------------------------------------------------------------")
		message("runMatlab Error", cond)

		if (isOpen){
			message("Error but matlab Open, close it.")
			close(matlab)
		}

		if ( retry_matlab_connection && (attempt < max_retries) ){

			# server already open, assume someone else is using
			session_matlab_port <- (port -1)
			message("try running on another port")
			message("attempts: ", attempt)
			message("max possible attempts: ", max_retries)
			message("--------------------------------------------------------------------------------")
			return( runMatlab(port=(port-1), attempt=(attempt+1)) )

		}else{

			message("failing execution after max tries")
			message("--------------------------------------------------------------------------------")
			return(FALSE)
		}
	},
	warning=function(cond){
		message("--------------------------------------------------------------------------------")
		message("runMatlab Warning", cond)
		message("--------------------------------------------------------------------------------")
		return(FALSE) # fail?
	})

	message("attempt ", attempt)
	message("runMatlab RESULTS: success?:", out)
	return(out)
}


#--------------------------------------------------------------------------------
# Static Variables
#--------------------------------------------------------------------------------

#----------------------------------------
# Working Directories: path where files are saved/opened (if any)
#----------------------------------------
#base_dir <- "/srv/shiny-server/dams_mcda/" # root
#response_dir <- paste(base_dir, "responses/", sep="") # where responses are saved
#working_dir <- paste(base_dir, "", sep="") # default

#setwd(working_dir) # initial directory

#responsesDir <- file.path(response_dir) # directory where responses get stored

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
available_dams <- seq(1:8)
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
   "Remove Dam",
   "Improve Fish Passage",
   "Improve Hydro",
   "Improve Hydro AND Fish Passage",
   "Keep and Maintain Dam"
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
	response_data <<- table_data
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
#
# Define server logic required to draw a histogram
#--------------------------------------------------------------------------------
server <- function(input, output, session) {

	# debug matlab
	#runMatlab()

	#------------------------------------------------------------
	# JS data passing test
	#------------------------------------------------------------
	# debug/validate authentication
	session$sendCustomMessage("validateSession", "any message")


	#------------------------------------------------------------
	# updateDamGraph
	# given a dam index and vector of scores makes the raw score graphs
	#------------------------------------------------------------
	updateDamGraph <- function(damId, scoreVector){
		# Graph1

		# decision criteria ids
		Criteria <- c(rep(criteria_names, times=length(1)))
		# score needs to be vector and not data frame or it will repeat for each criteria
		#Score <- as.numeric(RawCriteriaMatrix[damId, ])
		# two columns, score and criteria of score
		Data <- data.frame(score=scoreVector, criteria=Criteria)

		# Figure 1 raw pref plot
		output[[paste0("SummPlot", damId)]] <- renderBarErrorPlot(
			Data, # data
			paste("Raw Preference Scores for", dam_names[damId], sep=" "), # title
			criteria_names, # x_labels
			"Topic", # x axis label
			"Score", # y axis label
			colors, # colors
			NULL, # x value limit
			score_range, # y value limit (0-100 value range)
			0.1, # error amount
			0.4, # error bar width
			"red" # error bar color
		)

		# Graph2
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
	# updateDam1
	# logic for updating West Enfield Dam
	#------------------------------------------------------------
	updateDam1 <- function (){
		# update the tab status
		output$Dam1 <- renderUI(list(
			"Dam 1: West Enfield",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam1 <- c(
			input$FishBiomass1,
			input$RiverRec1,
			input$Reservoir1,
			input$ProjectCost1,
			input$Safety1,
			input$NumProperties1,
			input$ElectricityGeneration1,
			input$AvoidEmissions1,
			input$IndigenousLifeways1,
			input$IndustrialHistory1,
			input$CommunityIdentity1,
			input$Aesthetics1,
			input$Health1,
			input$Justice1
		)

		# create table matrix
		Dam1_Table <- as.matrix(data.frame(Dam1))
		row.names(Dam1_Table) <- criteria_names
		names(Dam1_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(1, Dam1)
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
		output$Dam2 <- renderUI(list(
			"Dam 2: Medway Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam2 <- c(
			input$FishBiomass2,
			input$RiverRec2,
			input$Reservoir2,
			input$ProjectCost2,
			input$Safety2,
			input$NumProperties2,
			input$ElectricityGeneration2,
			input$AvoidEmissions2,
			input$IndigenousLifeways2,
			input$IndustrialHistory2,
			input$CommunityIdentity2,
			input$Aesthetics2,
			input$Health2,
			input$Justice2
		)

		# create table matrix
		Dam2_Table <- as.matrix(data.frame(Dam2))
		row.names(Dam2_Table) <- criteria_names
		names(Dam2_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(2, Dam2)
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
		output$Dam3 <- renderUI(list(
			"Dam 3: Millinocket Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam3 <- c(
			input$FishBiomass3,
			input$RiverRec3,
			input$Reservoir3,
			input$ProjectCost3,
			input$Safety3,
			input$NumProperties3,
			input$ElectricityGeneration3,
			input$AvoidEmissions3,
			input$IndigenousLifeways3,
			input$IndustrialHistory3,
			input$CommunityIdentity3,
			input$Aesthetics3,
			input$Health3,
			input$Justice3
		)

		# create table matrix
		Dam3_Table <- as.matrix(data.frame(Dam3))
		row.names(Dam3_Table) <- criteria_names
		names(Dam3_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(3, Dam3)
		# make the container of those graphs visible
		shinyjs::show(id="dam-3-output")

		# mark the dam as complete when update
		# or apply logic here to make other contstraints for "complete"
		#updateAlternativeStatus("add", 3)
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 3)
	}


	#------------------------------------------------------------
	# updateDam4
	# logic for updating East Millinocket Dam
	#------------------------------------------------------------
	updateDam4 <- function() {
		output$Dam4 <- renderUI(list(
			"Dam 4: East Millinocket Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam4 <- c(
			input$FishBiomass4,
			input$RiverRec4,
			input$Reservoir4,
			input$ProjectCost4,
			input$Safety4,
			input$NumProperties4,
			input$ElectricityGeneration4,
			input$AvoidEmissions4,
			input$IndigenousLifeways4,
			input$IndustrialHistory4,
			input$CommunityIdentity4,
			input$Aesthetics4,
			input$Health4,
			input$Justice4
		)

		# create table matrix
		Dam4_Table <- as.matrix(data.frame(Dam4))
		row.names(Dam4_Table) <- criteria_names
		names(Dam4_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(4, Dam4)
		# make the container of those graphs visible
		shinyjs::show(id="dam-4-output")

		# mark the alternative as complete when update
		# or apply logic here to make other contstraints for "complete"
		#updateAlternativeStatus("add", 4)
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 4)
	}


	#------------------------------------------------------------
	# updateDam5
	# logic for updating North Twin Dam
	#------------------------------------------------------------
	updateDam5 <- function() {
		output$Dam5 <- renderUI(list(
			"Dam 5: North Twin Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam5 <- c(
			input$FishBiomass5,
			input$RiverRec5,
			input$Reservoir5,
			input$ProjectCost5,
			input$Safety5,
			input$NumProperties5,
			input$ElectricityGeneration5,
			input$AvoidEmissions5,
			input$IndigenousLifeways5,
			input$IndustrialHistory5,
			input$CommunityIdentity5,
			input$Aesthetics5,
			input$Health5,
			input$Justice5
		)

		# create table matrix
		Dam5_Table <- as.matrix(data.frame(Dam5))
		row.names(Dam5_Table) <- criteria_names
		names(Dam5_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(5, Dam5)
		# make the container of those graphs visible
		shinyjs::show(id="dam-5-output")

		# mark the dam as complete when update
		# or apply logic here to make other contstraints for "complete"
		#updateAlternativeStatus("add", 5)
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 5)
	}


	#------------------------------------------------------------
	# updateDam6
	# logic for updating Dolby Dam
	#------------------------------------------------------------
	updateDam6 <- function() {
		output$Dam6 <- renderUI(list(
			"Dam 6:Dolby Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam6 <- c(
			input$FishBiomass6,
			input$RiverRec6,
			input$Reservoir6,
			input$ProjectCost6,
			input$Safety6,
			input$NumProperties6,
			input$ElectricityGeneration6,
			input$AvoidEmissions6,
			input$IndigenousLifeways6,
			input$IndustrialHistory6,
			input$CommunityIdentity6,
			input$Aesthetics6,
			input$Health6,
			input$Justice6
		)

		# create table matrix
		Dam6_Table <- as.matrix(data.frame(Dam6))
		row.names(Dam6_Table) <- criteria_names
		names(Dam6_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(6, Dam6)
		# make the container of those graphs visible
		shinyjs::show(id="dam-6-output")

		# mark the dam as complete when update
		# or apply logic here to make other contstraints for "complete"
		#updateAlternativeStatus("add", 6)
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 6)
	}


	#------------------------------------------------------------
	# updateDam7
	# logic for updating Millinocket Lake Dam
	#------------------------------------------------------------
	updateDam7 <- function() {
		output$Dam7 <- renderUI(list(
			"Dam 7: Millinocket Lake Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam7 <- c(
			input$FishBiomass7,
			input$RiverRec7,
			input$Reservoir7,
			input$ProjectCost7,
			input$Safety7,
			input$NumProperties7,
			input$ElectricityGeneration7,
			input$AvoidEmissions7,
			input$IndigenousLifeways7,
			input$IndustrialHistory7,
			input$CommunityIdentity7,
			input$Aesthetics7,
			input$Health7,
			input$Justice7
		)

		# create table matrix
		Dam7_Table <- as.matrix(data.frame(Dam7))
		row.names(Dam7_Table) <- criteria_names
		names(Dam7_Table) <- "Raw Score"

		# update dam specific graphs
		updateDamGraph(7, Dam7)
		# make the container of those graphs visible
		shinyjs::show(id="dam-7-output")

		# mark the dam as complete when update
		# or apply logic here to make other contstraints for "complete"
		#updateAlternativeStatus("add", 7)
		session$userData[['dams_completed']] <- updateDamStatus(session$userData[['dams_completed']], "add", 7)
	}


	#------------------------------------------------------------
	# updateDam8
	# logic for updating Ripogenus Dam
	#------------------------------------------------------------
	updateDam8 <- function() {
		output$Dam8 <- renderUI(list(
			"Dam 8: Ripogenus Dam",
			tags$span('Complete', class="dam-complete")
		))

		# get decision inputs
		Dam8 <- c(
			input$FishBiomass8,
			input$RiverRec8,
			input$Reservoir8,
			input$ProjectCost8,
			input$Safety8,
			input$NumProperties8,
			input$ElectricityGeneration8,
			input$AvoidEmissions8,
			input$IndigenousLifeways8,
			input$IndustrialHistory8,
			input$CommunityIdentity8,
			input$Aesthetics8,
			input$Health8,
			input$Justice8
		)

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
		#updateDamStatus("add", 8)
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
			# example 14 criterion 8 dams results in 14 column by 8 row 2d data structure specific to dams
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

			#for alternatives in tables/graphs, this generates a blank matrix with labels
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
			#  }

			#  alternatives[[row_id]] <- unlist(r) # we want in c and not list
			#}
			#alternatives <- unlist(alternatives)

			# -------------------------------NEED TO REWRITE BY DAM------------------------------#
			# assign values in new matrix
			raw_scores <- getRawScores()
			RawCriteriaMatrix <- data.frame(
				matrix(raw_scores, nrow=length(available_dams), byrow=length(criteria_inputs))
			)
			message("RawCriteriaMatrix", RawCriteriaMatrix)

			# assign table row, column names
			row.names(RawCriteriaMatrix) <- dam_names
			colnames(RawCriteriaMatrix) <- criteria_names

			# origial scores in table form
			# for debugging table size
			output$FilledCriteriaTable <- renderTable(RawCriteriaMatrix, rownames=enable_rownames)

			#----------------------------------------
			# Call WSM and format response
			#----------------------------------------
			# matrix setup
			matrix_cols <- length(criteria_inputs) # 14 default (output size, adds summedscore)
			matrix_rows <- length(available_dams) # 8 default

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

			WSMTableOutput <- data.frame( TableMatrix, row.names=dam_names, check.names=FALSE)
			# this ones different because it has sum row
			names(WSMTableOutput) <- criteria_names_and_sum
			# -------------------------------END REWRITE BY DAM------------------------------#

			#----------------------------------------
			# Final Outputs
			#----------------------------------------
			# final output table commented out due to redundancy
			output$WSMTable <- renderTable(WSMTableOutput, rownames=enable_rownames)

			message('saveResponse')
			saveResponse(WSMTableOutput)

			# decision criteria ids
			#Criteria <- c(rep(criteria_names, times=length(1)))
			## score needs to be vector and not data frame or it will repeat for each criteria
			#Score <- as.numeric(RawCriteriaMatrix[1, ])
			## two columns, score and criteria of score
			#Data <- data.frame(score=Score, criteria=Criteria)

			## Figure 1 raw pref plot
			#output$SummPlot1 <- renderBarPlot(
			#	Data, # data
			#	paste("Raw Preference Scores for", dam_names[1], sep=" "), # title
			#	criteria_names, # x_labels
			#	"Topic", # x axis label
			#	"Score", # y axis label
			#	colors, # colors
			#	NULL, # x value limit
			#	score_range # y value limit (0-100 value range)
			#)

			# Figure 2 Stacked Bar 100%
			#output$WSMPlot1 <- renderPlot(
			#	ggplot(
			#	  data=Data,
			#	  aes(x=Alternative, y=Score, fill=Criteria, label=Score),
			#	  environment = environment()
			#	)
			#	+ geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#	+ geom_text(data=subset(Data, Score != 0), size=4, position = position_stack(vjust = 0.5))
			#	#+ geom_text(data=subset(Data, (Score < 0.06) & (Score > 0)), size=4, angle = 90, position = position_stack(vjust = 0.5))
			#	+ theme_minimal()
			#	#+ coord_flip()
			#	+ theme(
			#		text=element_text(size=16),
			#		legend.position="bottom",
			#		axis.text.y = element_text(angle = 0, hjust = 1),
			#		axis.text.x = element_text(angle = 45, hjust = 1)
			#	)
			#	+ scale_x_discrete(limits=rev(alternative_names))
			#	+ scale_y_continuous(limits = c(0,1), expand = c(0, 0))
			#)

			# order of stacked bars for plot2
			#Data$Alternative <- factor(Data$Alternative, levels = rev(levels(Data$Alternative)))

			## Figure 3 stacked bar plot
			#output$WSMPlot2 <- renderPlot(
			#	ggplot(
			#	  data=Data,
			#	  aes(x=Criteria, y=Score, fill=Alternative, label=Score, order=Alternative),
			#	  environment = environment()
			#	)
			#	+ geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#	+ geom_text(
			#		data=subset(Data, Score > 0.05), size=4, position = position_stack(vjust = 0.5)
			#	)
			#	+ geom_text(
			#		data=subset(Data, (Score != 0) & (Score < 0.06)), size=2, position = position_stack(vjust = 0.5)
			#	)
			#	+ theme_minimal()
			#	#+ coord_flip()
			#	+ theme(
			#		text=element_text(size=16),
			#		legend.position="top",
			#		axis.text.x = element_text(angle = 45, hjust = 1)
			#	)
			#	+ scale_x_discrete(limits=rev(criteria_names))
			#	+ scale_y_continuous(expand = c(0, 0))
			#)

			## Figure 4 raw pref plot
			#output$SummPlot2 <- renderBarPlot(
			#  Dam8, # data
			#  "Raw Preference Scores for Medway", # title
			#  criteria_names, # x_labels
			#  "Topic", # x axis label
			#  "Score", # y axis label
			#  colors, # colors
			#  NULL, # x value limit
			#  score_range # y value limit (0-100 value range)
			#)

			## Figure 5 Stacked Bar 100%
			#output$WSMPlot3 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Alternative, y=Score, fill=Criteria, label=Score),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(data=subset(Data, Score != 0), size=4, position = position_stack(vjust = 0.5))
			#  #+ geom_text(data=subset(Data, (Score < 0.06) & (Score > 0)), size=4, angle = 90, position = position_stack(vjust = 0.5))
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="bottom",
			#    axis.text.y = element_text(angle = 0, hjust = 1),
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(alternative_names))
			#  + scale_y_continuous(limits = c(0,1), expand = c(0, 0))
			#)

			## order of stacked bars for plot2
			#Data$Alternative <- factor(Data$Alternative, levels = rev(levels(Data$Alternative)))

			## Figure 6 stacked bar plot
			#output$WSMPlot4 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Criteria, y=Score, fill=Alternative, label=Score, order=Alternative),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(
			#    data=subset(Data, Score > 0.05), size=4, position = position_stack(vjust = 0.5)
			#  )
			#  + geom_text(
			#    data=subset(Data, (Score != 0) & (Score < 0.06)), size=2, position = position_stack(vjust = 0.5)
			#  )
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="top",
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(criteria_names))
			#  + scale_y_continuous(expand = c(0, 0))
			#)
			#
			## Figure 7 raw pref plot
			#output$SummPlot3 <- renderBarPlot(
			#  Dam8, # data
			#  "Raw Preference Scores for Millinocket/Quakish", # title
			#  criteria_names, # x_labels
			#  "Topic", # x axis label
			#  "Score", # y axis label
			#  colors, # colors
			#  NULL, # x value limit
			#  score_range # y value limit (0-100 value range)
			#)
			#
			## Figure 8 Stacked Bar 100%
			#output$WSMPlot5 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Alternative, y=Score, fill=Criteria, label=Score),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(data=subset(Data, Score != 0), size=4, position = position_stack(vjust = 0.5))
			#  #+ geom_text(data=subset(Data, (Score < 0.06) & (Score > 0)), size=4, angle = 90, position = position_stack(vjust = 0.5))
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="bottom",
			#    axis.text.y = element_text(angle = 0, hjust = 1),
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(alternative_names))
			#  + scale_y_continuous(limits = c(0,1), expand = c(0, 0))
			#)
			#
			## order of stacked bars for plot2
			#Data$Alternative <- factor(Data$Alternative, levels = rev(levels(Data$Alternative)))
			#
			## Figure 9 stacked bar plot
			#output$WSMPlot6 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Criteria, y=Score, fill=Alternative, label=Score, order=Alternative),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(
			#    data=subset(Data, Score > 0.05), size=4, position = position_stack(vjust = 0.5)
			#  )
			#  + geom_text(
			#    data=subset(Data, (Score != 0) & (Score < 0.06)), size=2, position = position_stack(vjust = 0.5)
			#  )
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="top",
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(criteria_names))
			#  + scale_y_continuous(expand = c(0, 0))
			#)
			#
			## Figure 10 raw pref plot
			#output$SummPlot4 <- renderBarPlot(
			#  Dam8, # data
			#  "Raw Preference Scores for East Millinocket", # title
			#  criteria_names, # x_labels
			#  "Topic", # x axis label
			#  "Score", # y axis label
			#  colors, # colors
			#  NULL, # x value limit
			#  score_range # y value limit (0-100 value range)
			#)
			#
			## Figure 11 Stacked Bar 100%
			#output$WSMPlot7 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Alternative, y=Score, fill=Criteria, label=Score),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(data=subset(Data, Score != 0), size=4, position = position_stack(vjust = 0.5))
			#  #+ geom_text(data=subset(Data, (Score < 0.06) & (Score > 0)), size=4, angle = 90, position = position_stack(vjust = 0.5))
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="bottom",
			#    axis.text.y = element_text(angle = 0, hjust = 1),
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(alternative_names))
			#  + scale_y_continuous(limits = c(0,1), expand = c(0, 0))
			#)
			#
			## order of stacked bars for plot2
			#Data$Alternative <- factor(Data$Alternative, levels = rev(levels(Data$Alternative)))
			#
			## Figure 12 stacked bar plot
			#output$WSMPlot8 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Criteria, y=Score, fill=Alternative, label=Score, order=Alternative),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(
			#    data=subset(Data, Score > 0.05), size=4, position = position_stack(vjust = 0.5)
			#  )
			#  + geom_text(
			#    data=subset(Data, (Score != 0) & (Score < 0.06)), size=2, position = position_stack(vjust = 0.5)
			#  )
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="top",
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(criteria_names))
			#  + scale_y_continuous(expand = c(0, 0))
			#)
			#
			## Figure 13 raw pref plot
			#output$SummPlot5 <- renderBarPlot(
			#  Dam8, # data
			#  "Raw Preference Scores for North Twin", # title
			#  criteria_names, # x_labels
			#  "Topic", # x axis label
			#  "Score", # y axis label
			#  colors, # colors
			#  NULL, # x value limit
			#  score_range # y value limit (0-100 value range)
			#)
			#
			## Figure 14 Stacked Bar 100%
			#output$WSMPlot9 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Alternative, y=Score, fill=Criteria, label=Score),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(data=subset(Data, Score != 0), size=4, position = position_stack(vjust = 0.5))
			#  #+ geom_text(data=subset(Data, (Score < 0.06) & (Score > 0)), size=4, angle = 90, position = position_stack(vjust = 0.5))
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="bottom",
			#    axis.text.y = element_text(angle = 0, hjust = 1),
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(alternative_names))
			#  + scale_y_continuous(limits = c(0,1), expand = c(0, 0))
			#)
			#
			## order of stacked bars for plot2
			#Data$Alternative <- factor(Data$Alternative, levels = rev(levels(Data$Alternative)))
			#
			## Figure 15 stacked bar plot
			#output$WSMPlot10 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Criteria, y=Score, fill=Alternative, label=Score, order=Alternative),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(
			#    data=subset(Data, Score > 0.05), size=4, position = position_stack(vjust = 0.5)
			#  )
			#  + geom_text(
			#    data=subset(Data, (Score != 0) & (Score < 0.06)), size=2, position = position_stack(vjust = 0.5)
			#  )
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="top",
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(criteria_names))
			#  + scale_y_continuous(expand = c(0, 0))
			#)
			#
			## Figure 16 raw pref plot
			#output$SummPlot6 <- renderBarPlot(
			#  Dam8, # data
			#  "Raw Preference Scores for Dolby", # title
			#  criteria_names, # x_labels
			#  "Topic", # x axis label
			#  "Score", # y axis label
			#  colors, # colors
			#  NULL, # x value limit
			#  score_range # y value limit (0-100 value range)
			#)
			#
			## Figure 17 Stacked Bar 100%
			#output$WSMPlot11 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Alternative, y=Score, fill=Criteria, label=Score),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(data=subset(Data, Score != 0), size=4, position = position_stack(vjust = 0.5))
			#  #+ geom_text(data=subset(Data, (Score < 0.06) & (Score > 0)), size=4, angle = 90, position = position_stack(vjust = 0.5))
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="bottom",
			#    axis.text.y = element_text(angle = 0, hjust = 1),
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(alternative_names))
			#  + scale_y_continuous(limits = c(0,1), expand = c(0, 0))
			#)
			#
			## order of stacked bars for plot2
			#Data$Alternative <- factor(Data$Alternative, levels = rev(levels(Data$Alternative)))
			#
			## Figure 18 stacked bar plot
			#output$WSMPlot12 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Criteria, y=Score, fill=Alternative, label=Score, order=Alternative),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(
			#    data=subset(Data, Score > 0.05), size=4, position = position_stack(vjust = 0.5)
			#  )
			#  + geom_text(
			#    data=subset(Data, (Score != 0) & (Score < 0.06)), size=2, position = position_stack(vjust = 0.5)
			#  )
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="top",
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(criteria_names))
			#  + scale_y_continuous(expand = c(0, 0))
			#)
			#
			## Figure 19 raw pref plot
			#output$SummPlot7 <- renderBarPlot(
			#  Dam8, # data
			#  "Raw Preference Scores for Millinocket Lake", # title
			#  criteria_names, # x_labels
			#  "Topic", # x axis label
			#  "Score", # y axis label
			#  colors, # colors
			#  NULL, # x value limit
			#  score_range # y value limit (0-100 value range)
			#)
			#
			## Figure 20 Stacked Bar 100%
			#output$WSMPlot13 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Alternative, y=Score, fill=Criteria, label=Score),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(data=subset(Data, Score != 0), size=4, position = position_stack(vjust = 0.5))
			#  #+ geom_text(data=subset(Data, (Score < 0.06) & (Score > 0)), size=4, angle = 90, position = position_stack(vjust = 0.5))
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="bottom",
			#    axis.text.y = element_text(angle = 0, hjust = 1),
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(alternative_names))
			#  + scale_y_continuous(limits = c(0,1), expand = c(0, 0))
			#)
			#
			## order of stacked bars for plot2
			#Data$Alternative <- factor(Data$Alternative, levels = rev(levels(Data$Alternative)))
			#
			## Figure 21 stacked bar plot
			#output$WSMPlot14 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Criteria, y=Score, fill=Alternative, label=Score, order=Alternative),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(
			#    data=subset(Data, Score > 0.05), size=4, position = position_stack(vjust = 0.5)
			#  )
			#  + geom_text(
			#    data=subset(Data, (Score != 0) & (Score < 0.06)), size=2, position = position_stack(vjust = 0.5)
			#  )
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="top",
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(criteria_names))
			#  + scale_y_continuous(expand = c(0, 0))
			#)
			#
			## Figure 22 raw pref plot
			#output$SummPlot8 <- renderBarPlot(
			#  Dam8, # data
			#  "Raw Preference Scores for Ripogenus", # title
			#  criteria_names, # x_labels
			#  "Topic", # x axis label
			#  "Score", # y axis label
			#  colors, # colors
			#  NULL, # x value limit
			#  score_range # y value limit (0-100 value range)
			#)
			#
			## Figure 23 Stacked Bar 100%
			#output$WSMPlot15 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Alternative, y=Score, fill=Criteria, label=Score),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(data=subset(Data, Score != 0), size=4, position = position_stack(vjust = 0.5))
			#  #+ geom_text(data=subset(Data, (Score < 0.06) & (Score > 0)), size=4, angle = 90, position = position_stack(vjust = 0.5))
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="bottom",
			#    axis.text.y = element_text(angle = 0, hjust = 1),
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(alternative_names))
			#  + scale_y_continuous(limits = c(0,1), expand = c(0, 0))
			#)
			#
			## order of stacked bars for plot2
			#Data$Alternative <- factor(Data$Alternative, levels = rev(levels(Data$Alternative)))
			#
			## Figure 24 stacked bar plot
			#output$WSMPlot16 <- renderPlot(
			#  ggplot(
			#    data=Data,
			#    aes(x=Criteria, y=Score, fill=Alternative, label=Score, order=Alternative),
			#    environment = environment()
			#  )
			#  + geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#  + geom_text(
			#    data=subset(Data, Score > 0.05), size=4, position = position_stack(vjust = 0.5)
			#  )
			#  + geom_text(
			#    data=subset(Data, (Score != 0) & (Score < 0.06)), size=2, position = position_stack(vjust = 0.5)
			#  )
			#  + theme_minimal()
			#  #+ coord_flip()
			#  + theme(
			#    text=element_text(size=16),
			#    legend.position="top",
			#    axis.text.x = element_text(angle = 45, hjust = 1)
			#  )
			#  + scale_x_discrete(limits=rev(criteria_names))
			#  + scale_y_continuous(expand = c(0, 0))
			#)
			#
			# plotly example graph
			#output$WSMPlotly2 <- renderPlotly(
			#  ggplotly(
			#	ggplot(
			#	  data=Data,
			#	  aes(x=Criteria, y=Score, fill=Alternative, label=Score),
			#	  environment = environment()
			#	)
			#	+ geom_bar(data=subset(Data, Score != 0), stat="identity") # ignore empty values
			#	+ theme_minimal()
			#	+ coord_flip()
			#	+ theme(
			#		text=element_text(size=16),
			#		legend.position="top",
			#		axis.text.x = element_text(angle = 90, hjust = 1)
			#	)
			#	+ scale_x_discrete(limits=rev(criteria_names))
			#	+ scale_y_continuous(expand = c(0, 0))
			#  )
			#)

			# show output html elements
			shinyjs::show(id="generated-output")
			message("generateOutput done")
		}
	}


	#--------------------------------------------------------------------------------
	# Initial Application State for session
	#--------------------------------------------------------------------------------
	observe({
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
	# alt6
	progress6 <- reactive({
	  sum <- 0.0
	  for (id in criteria_inputs){
	    sum <- as.numeric(sum + input[[paste0(id, toString(6))]])
	  }
	  return(sum)
	})
	# alt7
	progress7 <- reactive({
	  sum <- 0.0
	  for (id in criteria_inputs){
	    sum <- as.numeric(sum + input[[paste0(id, toString(7))]])
	  }
	  return(sum)
	})
	# alt8
	progress8 <- reactive({
	  sum <- 0.0
	  for (id in criteria_inputs){
	    sum <- as.numeric(sum + input[[paste0(id, toString(8))]])
	  }
	  return(sum)
	})

	# dam1
	output[[paste0("Dam", 1,"Progress")]] <- renderUI(list(
		paste0("Progress for Dam", 1, ": "),
		if( progress1() > upper_bound || progress1() < lower_bound)
			tags$span(paste0(progress1(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# Dam2
	output[[paste0("Dam", 2,"Progress")]] <- renderUI(list(
		paste0("Progress for Dam", 2, ": "),
		if( progress2() > upper_bound || progress2() < lower_bound)
			tags$span(paste0(progress2(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# Dam3
	output[[paste0("Dam", 3,"Progress")]] <- renderUI(list(
		paste0("Progress for Dam", 3, ": "),
		if( progress3() > upper_bound || progress3() < lower_bound)
			tags$span(paste0(progress3(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# Dam4
	output[[paste0("Dam", 4,"Progress")]] <- renderUI(list(
		paste0("Progress for Dam", 4, ": "),
		if( progress4() > upper_bound || progress4() < lower_bound)
			tags$span(paste0(progress4(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# Dam5
	output[[paste0("Dam", 5,"Progress")]] <- renderUI(list(
		paste0("Progress for Dam", 5, ": "),
		if( progress5() > upper_bound || progress5() < lower_bound)
			tags$span(paste0(progress5(), " / 1.0"), class="not-complete")
		else
			tags$span("1.0 / 1.0", class="complete")
	))
	# Dam6
	output[[paste0("Dam", 6,"Progress")]] <- renderUI(list(
	  paste0("Progress for Dam", 6, ": "),
	  if( progress6() > upper_bound || progress6() < lower_bound)
	    tags$span(paste0(progress6(), " / 1.0"), class="not-complete")
	  else
	    tags$span("1.0 / 1.0", class="complete")
	))
	# Dam7
	output[[paste0("Dam", 7,"Progress")]] <- renderUI(list(
	  paste0("Progress for Dam", 7, ": "),
	  if( progress7() > upper_bound || progress7() < lower_bound)
	    tags$span(paste0(progress7(), " / 1.0"), class="not-complete")
	  else
	    tags$span("1.0 / 1.0", class="complete")
	))
	# Dam8
	output[[paste0("Dam", 8,"Progress")]] <- renderUI(list(
	  paste0("Progress for Dam", 8, ": "),
	  if( progress8() > upper_bound || progress8() < lower_bound)
	    tags$span(paste0(progress8(), " / 1.0"), class="not-complete")
	  else
	    tags$span("1.0 / 1.0", class="complete")
	))
	#--------------------------------------------------------------------------------
	# Alternative Update Event Listeners
	# these trigger the updates on button click
	#--------------------------------------------------------------------------------

	# West Enfield
	#----------------------------------------
	observeEvent(input$updateBtn1, {
		message("update button 1")
		if(progress1() > upper_bound || progress1() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress1())
			))
		}else{
			 updateDam1()
		}
	})

	# Medway
	#----------------------------------------
	observeEvent(input$updateBtn2, {
		if(progress2() > upper_bound || progress2() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress2())
			))
		}else{
			updateDam2()
		}
	})

	# Millinocket
	#----------------------------------------
	observeEvent(input$updateBtn3, {
		if(progress3() > upper_bound || progress3() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress3())
			))
		}else{
			 updateDam3()
		}
	})

	# East Millinocket
	#----------------------------------------
	observeEvent(input$updateBtn4, {
		if(progress4() > upper_bound || progress4() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress4())
			))
		}else{
			updateDam4()
		}
	})

	# North Twin
	#----------------------------------------
	observeEvent(input$updateBtn5, {
		if(progress5() > upper_bound || progress5() < lower_bound){
			showModal(modalDialog(
				title = "Not Finished!",
				paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress5())
			))
		}else{
			updateDam5()
		}
	})
	# Dolby
	#----------------------------------------
	observeEvent(input$updateBtn6, {
	  if(progress6() > upper_bound || progress6() < lower_bound){
	    showModal(modalDialog(
	      title = "Not Finished!",
	      paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress6())
	    ))
	  }else{
	    updateDam6()
	  }
	})
	# Millinocket Lake
	#----------------------------------------
	observeEvent(input$updateBtn7, {
	  if(progress7() > upper_bound || progress7() < lower_bound){
	    showModal(modalDialog(
	      title = "Not Finished!",
	      paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress7())
	    ))
	  }else{
	    updateDam7()
	  }
	})
	# Ripogenus Dam
	#----------------------------------------
	observeEvent(input$updateBtn8, {
	  if(progress8() > upper_bound || progress8() < lower_bound){
	    showModal(modalDialog(
	      title = "Not Finished!",
	      paste0('The sum of all sliders must be equal to 1.0! Currently the sum is: ', progress8())
	    ))
	  }else{
	    updateDam8()
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

	# on 'Dam1 Results > Generate' button event: fill matrix with user input values
	observeEvent(input$generateMatrix1, {
		 #TODO:
		 # as of right now all dam generation logic is in one function
		generateOutput()
	}) # end 'output' tab > on generate button event

	#TODO: remove as this is for fast debugging output results
	observeEvent(input$autoGenerateMatrix, {
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

	#TODO: remove as this is for fast debugging output results
	observeEvent(input$saveResultsToDjango, {

			raw_scores <- getRawScores()

			# assign values in new matrix
			RawCriteriaMatrix <- data.frame(
				matrix(raw_scores, nrow=length(available_dams), byrow=length(criteria_inputs))
			)

			# assign table row, column names
			row.names(RawCriteriaMatrix) <- dam_names
			colnames(RawCriteriaMatrix) <- criteria_names

			message("saveResultsToDjango RawCriteriaMatrix", RawCriteriaMatrix)
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
