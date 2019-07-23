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

	####FIX THISSS!!!! PUT IN REGULAR WS MATRIX!!!!!!!!! PRETEND LIKE THE SCENARIOS WONT HAPPEN
	#retrieve DamsData to manipulate into DamsMatrix
	DamsDataMatrix <- array(data=NA, dim = c(8, 14, 5))
	
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
	
	#This naming technique is wildly redundant and doesn't appear to work. 
	names(KeepMaintain) <- c(
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
	
	names(Improve_Hydro) <- c(
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
	
	names(Improve_Fish) <- c(
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
	
	names(FishANDHydro) <- c(
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
	
	names(Remove) <- c(
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
	  
	#This abind creates our 3D matrix
	DamsDataMatrix <- abind(KeepMaintain, Improve_Hydro, Improve_Fish, FishANDHydro, Remove, along = 3, force.array=TRUE)
	

	
	#--------NORMALIZATION-------------------
	# iterate each criteria for min,max
	WSMMaxVector <- list("list", matrix_cols)
	for ( index in 1:matrix_cols ){
		WSMMaxVector[[index]] <- max(DamsDataMatrix[,index,], na.rm=FALSE)#specified index level 7/23/2019
	}
	WSMMaxVector <- unlist(WSMMaxVector)

	WSMMinVector <- list("list", matrix_cols)
	for ( index in 1:matrix_cols ){
		WSMMinVector[[index]] <- min(DamsDataMatrix[,index,], na.rm=FALSE)#specified index level 7/23/2019
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
	alt_method_columns <- c(8, 14, 5) 

	message('length of critImportance', length(CritImportance)) 
	# make normalized values of each value in matrix ---> check DOES THIS NEED TO BE 3-D array, too?
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
		  for (p in 1:matrix_levs){#added levels 7/23/2019
			x <- DamsDataMatrix[n,k,p]
			min_x <- WSMMinVector[k]
			max_x <- WSMMaxVector[k]
			crit_imp <- CritImportance[n,k]


			WSMScoreMatrix[n,k,p] <- tryCatch({
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

		  } #End alternative (levels) for loop.
		} #End dam (rows) for loop.
		message('Raw column ', DamsDataMatrix[k])
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

