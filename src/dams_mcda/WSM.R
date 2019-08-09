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
#setwd("~/Beatrice2/R_ELF/R_NEST/MCDA_App_Shiny/MCDA_06262019/src/dams_mcda")


DamsData <- read.csv('DamsData.csv')
DamsData <- data.frame(DamsData)

TestData <- c(0,0.2,0.05,0.025,0.05,0.75, 0.2,0.1,0.1,0.2,0,0,0,0)

RawCriteriaMatrix <- matrix(TestData, 8, 14)

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


WSM <- function(RawCriteriaMatrix, DamsData){

	# matrix setup
	matrix_cols <- length(criteria_inputs) # 14 default (output size, adds summedscore)
	matrix_rows <- length(available_dams) # 8 default
	matrix_levs <- length(available_alternatives)


	message("Preference cols", matrix_cols, " rows ", matrix_rows, "scenarios", matrix_levs )

	#----------------------------------------
	# Step A (PREFS): Build Preference Matrix with blank levels, no normalization
	# score will be raw values from 0-1 based on user input
	#----------------------------------------
	PrefMatrix <- array(data = NA, c(8,14,5)) #if this needs to be 3D add ",nlevels = matrix_levs" after ncol 

	message("Fill User Preference Matrix")
	# weights in matrix
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			x <- RawCriteriaMatrix[n,k]

			PrefMatrix[n,k,p] <- tryCatch({
				#message("A", x, ', ', crit_imp)
				(x)
			}, error=function(e){
				(NA)
			})
		} #End dams (rows) for loop.
	} #End criteria (columns) for loop.


	message("fill Normalized Matrix")
	#----------------------------------------
	# Step B (DATA): Data Normalization using Min / Max Vectors
	### Sam's notes 7-10-19
	# outline for ranking procedure by range normalization, weighted sum
	# Retrieve user preference matrix (referred to as RawCriteriaMatrix), a 2D matrix [dams,criteria preference values]  DONE
	# Retrieve criteria scores for each dam (referred to as DamsDataMartrix), for each MCDA scenario (from server?) a 3D matrix [dams,criteria,alternatives] 
	
	# Normalization procedure:
	#  get maximum and minimum criteria score for each criterion, each dam, produces two 2D matrices [dams, max/min criteria]
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

	#retrieve DamsData to manipulate into DamsDataMatrix
	DamsDataMatrix <- array(data=NA, dim = c(8, 14, 5)) #creates empty 3d array in shape we want
	
	KeepMaintain <- cbind(DamsData$FishBiomass, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_KeepMaintain, DamsData$Damage, 
	                      DamsData$Properties, DamsData$AvgAnnualGen, DamsData$EmissionsReduc, 
	                      DamsData$Culture_KeepMaintain, DamsData$History_KeepMaintain, DamsData$Community_KeepMaintain, DamsData$Aesthetics_KeepMaintain, 
	                      DamsData$Health_KeepMaintain, DamsData$Justice_KeepMaintain)
	Improve_Hydro <- cbind(DamsData$FishBiomass, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_ImproveHydro, DamsData$Damage, 
	                       DamsData$Properties,DamsData$AvgAnnualGen_Add, DamsData$EmissionsReduc_Add, 
	                       DamsData$Culture_ImproveHydro, DamsData$History_ImproveHydro, DamsData$Community_ImproveHydro, DamsData$Aesthetics_ImproveHydro, 
	                       DamsData$Health_ImproveHydro, DamsData$Justice_ImproveHydro)
	Improve_Fish <- cbind(DamsData$FishBiomass, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_ImproveFish, DamsData$Damage, 
	                      DamsData$Properties,DamsData$AvgAnnualGen, DamsData$EmissionsReduc,  
	                      DamsData$Culture_ImproveFish, DamsData$History_ImproveFish, DamsData$Community_ImproveFish, DamsData$Aesthetics_ImproveFish, 
	                      DamsData$Health_ImproveFish, DamsData$Justice_ImproveFish)
	FishANDHydro <- cbind(DamsData$FishBiomass, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_FishANDHydro, DamsData$Damage, 
	                      DamsData$Properties, DamsData$AvgAnnualGen_Add, DamsData$EmissionsReduc_Add, 
	                      DamsData$Culture_FishANDHydro, DamsData$History_FishANDHydro, DamsData$Community_FishANDHydro, DamsData$Aesthetics_FishANDHydro,
	                      DamsData$Health_FishANDHydro, DamsData$Justice_FishANDHydro)
	Remove <- cbind(DamsData$FishBiomass, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_Remove, DamsData$Damage, 
	                DamsData$Properties, DamsData$AvgAnnualGen_Rem, DamsData$EmissionsReduc_Rem,  
	                DamsData$Culture_Remove, DamsData$History_Remove, DamsData$Community_Remove, DamsData$Aesthetics_Remove, 
	                DamsData$Health_Remove, DamsData$Justice_Remove)
	
	
	  
	#This abind creates our 3D matrix
	DamsDataMatrix <- abind(KeepMaintain, Improve_Hydro, Improve_Fish, FishANDHydro, Remove, along = 3, force.array=TRUE)
	

	
	#--------NORMALIZATION-------------------
	# iterate each criteria for min,max
  
	WSMMaxVector <- list("list", matrix_cols)
	  for ( k in 1:matrix_cols ){
		  WSMMaxVector[[k]] <- max(DamsDataMatrix[,k,], na.rm=FALSE)#specified index level 7/23/2019
	  }
	  WSMMaxVector <- unlist(WSMMaxVector)

	  WSMMinVector <- list("list", matrix_cols)
	  for ( k in 1:matrix_cols ){
		  WSMMinVector[[k]] <- min(DamsDataMatrix[,k,], na.rm=FALSE)#specified index level 7/23/2019
	  }
	  WSMMinVector <- unlist(WSMMinVector)

	# debug
	message('min vector ', WSMMinVector)
	message('max vector ', WSMMaxVector)
	


	#----------------------------------------
	# PATH B (DATA*PREFS): Build Score Matrix
	# score will be min/max normalized values from 0-1
	#----------------------------------------
	NormalizedMatrix <- array(data=NA, c(8,14,5))
	# array of rows that use alternative method
	min_crit_columns <- c(4,6) 

	# make normalized values of each value in matrix 
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			x <- DamsDataMatrix[n,k,p]
			min_x <- WSMMinVector[k]
			max_x <- WSMMaxVector[k]


			NormalizedMatrix[n,k,p] <- tryCatch({
				if (k %in% min_crit_columns){
					# alternative method
					# maximize normalization
					((max_x - x) / (max_x - min_x))
				}else{
					# for debugging by cell WSM uncomment next line
					# message('cell n, k, x, crit, result', n, ', ', k, ', ', x, ', ', ', ', (((x - min_x) / (max_x - min_x))) )

					# default method
					# minimize normilization
					((x - min_x) / (max_x - min_x))
				}
			}, error=function(e){
				(NA)
			})

		   #End alternative (levels) for loop.
		} #End dam (rows) for loop.
		message('Data column ', DamsDataMatrix[k])
		message('WSM column ', NormalizedMatrix[k])
	} #End criteria (columns) for loop.

	# debug
	#message('NormalizedMatrix ', NormalizedMatrix)

	#----------------------------------------
	# WeightedScoreMatrix
	#----------------------------------------
	
	WeightedScoreMatrix <- (NormalizedMatrix*PrefMatrix)

	WeightedScoreMatrix <- round(WeightedScoreMatrix,3) 

	# debug
	#message('IntermediateMatrix', IntermediateMatrix)

	#----------------------------------------
	# Score Sum
	#----------------------------------------
	# total score is last column of returned Data Table
	scoresum <- list("list", matrix_levs)

	for (i in 1:matrix_levs){
		scoresum[[i]] <- sum(as.numeric(WeightedScoreMatrix[i,,1:matrix_levs]))
	}

	scoresum <- unlist(scoresum)

	# warning adding things to list has side effects!
	WSMResults <- list(WeightedScoreMatrix, scoresum)

	return(WSMResults)
}

