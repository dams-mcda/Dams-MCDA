# WSM
#----------------------------------------
# generates the MDCA Output
# returns list(data.frame, data.frame) -> WSMScoreDF, WSMBarPlotData
#     WSMScoreDF: matrix for renderTable
#     WSMBarPlotData: matrix for rendering barplot
#
# Inputs:
#     CritImportance: #TODO: explain
#     RawCriteriaMatrix: raw score matrix
setwd("~/Beatrice2/R_ELF/R_NEST/MCDA_App_Shiny/MCDA_06262019/src/dams_mcda")

DamsData <- read.csv('DamsData.csv')
DamsData <- data.frame(DamsData)

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

# list of dams
available_dams <- seq(1:8)
# list of alternatives
available_alternatives <- seq(1:5)

matrix_cols <- length(criteria_inputs) # 14 default (output size, adds summedscore)
matrix_rows <- length(available_dams) # 8 default
matrix_levs <- length(available_alternatives)


WSM <- function(CritImportance, RawCriteriaMatrix, DamsData){

	# matrix setup
	matrix_cols <- length(criteria_inputs) # 14 default (output size, adds summedscore)
	matrix_rows <- length(available_dams) # 8 default
	matrix_levs <- length(available_alternatives)


	message("Preference cols", matrix_cols, " rows ", matrix_rows, "scenarios", matrix_levs )

	#----------------------------------------
	# Step A: Build Preference Matrix with blank levels, no normalization
	# score will be raw values from 0-1 based on user input
	#----------------------------------------
	WSMMatrix <- data.frame(matrix(data=NA, nrow = matrix_rows, ncol = matrix_cols)) #if this needs to be 3D add ",nlevels = matrix_levs" after ncol 

	message("Fill User Preference Matrix")
	# weight values of each raw score in matrix
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			x <- RawCriteriaMatrix[n,k]
			crit_imp <- CritImportance[n,k]

			WSMMatrix[n,k] <- tryCatch({
				#message("A", x, ', ', crit_imp)
				(x * crit_imp)
			}, error=function(e){
				(NA)
			})
		} #End alternative (rows) for loop.
	} #End criteria (columns) for loop.


	message("fill WSM Matrix")
	#----------------------------------------
	# Step B: Data Normalization using Min / Max Vectors
	### Sam's notes 7-10-19
	# outline for ranking procedure by range normalization, weighted sum
	# Retrieve user preference matrix (referred to as RawCriteriaMatrix), a 2D matrix [dams,criteria preference values]  DONE
	# Retrieve criteria scores for each dam (referred to as DamsDataMartrix), for each MCDA scenario (from server?) a 3D matrix [dams,criteria,alternatives] 
	
	# Normalization procedure:
	#  get maximum and minimum criteria score for each criteria, each dam, produces two 2D matrices [dams, max/min criteria]
	#  for positive scores: norm = (f - f_min) / (f_max - f_min)
	#  for negative scores (like cost): norm = 1 - (f - f_max) / (f_min - f_max)
	#  result is 3D matrix with dam-specific criteria scores normalized by min and max criteria sampled over all alternatives
	
	# Weighted sum procedure:
	#  multiply all normalized scores by prefrence weights
	#  sum normalized, weighted criteria scores for each dam
	#  sum this across all dams for each scenario
	
	# Rank:
	#  may need to reshape the array produced by the weighted sum procedure
	#  sort the array by descending order, highest score comes first, and record the indices of the top ranked scenario
	# Retrieve table, map of highest ranked scenario:
	#  use server url or whatever, searching for map name with the matching index number
	#  take the map image, table, and stick them in the webpage
	#----------------------------------------

	#retrieve DamsData to manipulate into DamsMatrix
	DamsDataMatrix <- array(data=NA, dim = length(3), dimnames(c(matrix_rows, matrix_cols, matrix_levs)))

	KeepMaintain <- as.array(c(DamsData$Cost_KeepMaintain, DamsData$AvgAnnualGen, DamsData$EmissionsReduc, DamsData$Damage, 
	                             DamsData$Properties, DamsData$ResStorage, DamsData$Aesthetics_KeepMaintain, DamsData$Community_KeepMaintain, 
	                             DamsData$Cuture_KeepMaintain, DamsData$History_KeepMaintain, DamsData$Health_KeepMaintain, DamsData$Justice_KeepMaintain))
	Improve_Hydro <- as.array(c(DamsData$Cost_ImproveHydro, DamsData$AvgAnnualGen_Add, DamsData$EmissionsReduc_Add, DamsData$Damage, 
	                              DamsData$Properties, DamsData$ResStorage, DamsData$Aesthetics_ImproveHydro, DamsData$Community_ImproveHydro, 
	                              DamsData$Cuture_ImproveHydro, DamsData$History_ImproveHydro, DamsData$Health_ImproveHydro, DamsData$Justice_ImproveHydro))
	Improve_Fish <- as.array(c(DamsData$Cost_ImproveFish, DamsData$AvgAnnualGen, DamsData$EmissionsReduc, DamsData$Damage, 
	                             DamsData$Properties, DamsData$ResStorage, DamsData$Aesthetics_ImproveFish, DamsData$Community_ImproveFish, 
	                             DamsData$Cuture_ImproveFish, DamsData$History_ImproveFish, DamsData$Health_ImproveFish, DamsData$Justice_ImproveFish))
	FishANDHydro <- as.array(c(DamsData$Cost_FishANDHydro, DamsData$AvgAnnualGen_Add, DamsData$EmissionsReduc_Add, DamsData$Damage, 
	                             DamsData$Properties, DamsData$ResStorage, DamsData$Aesthetics_FishANDHydro, DamsData$Community_FishANDHydro, 
	                             DamsData$Cuture_FishANDHydro, DamsData$History_FishANDHydro, DamsData$Health_FishANDHydro, DamsData$Justice_FishANDHydro))
	Remove <- as.array(c(DamsData$Cost_Remove, DamsData$AvgAnnualGen_Rem, DamsData$EmissionsReduc_Rem, DamsData$Damage, 
	                       DamsData$Properties, DamsData$ResStorage, DamsData$Aesthetics_Remove, DamsData$Community_Remove, 
	                       DamsData$Cuture_Remove, DamsData$History_Remove, DamsData$Health_Remove, DamsData$Justice_Remove))
	
	AlternativeLevels <- as.array(c(KeepMaintain, ImproveHydro, ImproveFish, FishANDHydro, Remove))
	
	
	
	#--------NORMALIZATION-------------------
	# iterate each criteria for min,max
	WSMMaxVector <- list("list", matrix_cols)
	for ( index in 1:matrix_cols ){
		WSMMaxVector[[index]] <- max(DamsDataMatrix[,index], na.rm=FALSE)
	}
	WSMMaxVector <- unlist(WSMMaxVector)

	WSMMinVector <- list("list", matrix_cols)
	for ( index in 1:matrix_cols ){
		WSMMinVector[[index]] <- min(DamsDataMatrix[,index], na.rm=FALSE)
	}
	WSMMinVector <- unlist(WSMMinVector)

	# debug
	message('min vector ', WSMMinVector)
	message('max vector ', WSMMaxVector)
	#message('critical matrix ', CritImportance)


	#----------------------------------------
	# PATH B: Build Score Matrix
	# score will be min/max normalized values from 0-1
	#----------------------------------------
	WSMScoreMatrix <- data.frame(matrix(data=NA, nrow = matrix_rows, ncol = matrix_cols, nlevels = matrix_levs))
	# array of rows that use alternative method
	alt_method_columns <- c(4, 6) ####THIS LIKELY NEEDS TO CHANGE

	message('length of critImportance', length(CritImportance)) ###THIS ALSO LIKELY NEEDS TO CHANGE
	# make normalized values of each value in matrix ---> check DOES THIS NEED TO BE 3-D array, too?
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			x <- RawCriteriaMatrix[n,k]
			min_x <- WSMMinVector[k]
			max_x <- WSMMaxVector[k]
			crit_imp <- CritImportance[n,k]


			WSMScoreMatrix[n,k] <- tryCatch({
				if (k %in% alt_method_columns){
					# alternative method
					# maximize normalization
					(((max_x - x) / (max_x - min_x)) * crit_imp)
				}else{
					# for debugging by cell WSM uncomment next line
					# message('cell n, k, x, crit, result', n, ', ', k, ', ', x, ', ', crit_imp, ', ', (((x - min_x) / (max_x - min_x)) * crit_imp) )

					# default method
					# minimize normilization
					(((x - min_x) / (max_x - min_x)) * crit_imp)
				}
			}, error=function(e){
				(NA)
			})

		} #End alternative (rows) for loop.
		message('Raw column ', RawCriteriaMatrix[k])
		message('WSM column ', WSMScoreMatrix[k])
	} #End criteria (columns) for loop.

	# debug
	#message('WSMScoreMatrix ', WSMScoreMatrix)

	#----------------------------------------
	# IntermediateMatrix
	# Note: adjust matrix to be rounded if using alternative path
	#----------------------------------------
	IntermediateMatrix <- data.frame(matrix(data=NA, nrow=matrix_rows, ncol=matrix_cols))
	IntermediateMatrix <- round(WSMMatrix,3) #adjust matrix name if using alternate path

	# debug
	#message('IntermediateMatrix', IntermediateMatrix)

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

	return(WSMResults)
}

