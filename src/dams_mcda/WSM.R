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

TestData <- c(0,0.2,0.05,0.025,0.05,0.75, 0.2,0.1,0.1,0.2,0,0,0,0)

RawCriteriaMatrix <- array(TestData, c(8, 14,5))

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

matrix_cols <- length(criteria_inputs) # 14 default (output size)
matrix_rows <- length(available_dams) # 8 default
matrix_levs <- length(available_alternatives)


WSM <- function(RawCriteriaMatrix, DamsData){

	# matrix setup
	matrix_cols <- length(criteria_inputs) # 14 default (output size)
	matrix_rows <- length(available_dams) # 8 default
	matrix_levs <- length(available_alternatives)# 5 default


	message("Decision Criteria", matrix_cols, "Dams", matrix_rows, "Decision Alternatives", matrix_levs )

	#----------------------------------------
	# Step A (PREFS): Build Preference Matrix with blank levels, no normalization
	# score will be raw values from 0-1 based on user input
	#----------------------------------------
	PrefMatrix <- array(data = NA, c(8,14,5)) #This is a 3-D blank matrix 
	
	message("Fill User Preference Matrix")
	# weights in matrix
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			x <- RawCriteriaMatrix[n,k,p]

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
	# (DATA): Data Normalization using Min / Max Vectors
	# Retrieve criteria scores for each dam (referred to as DamsDataMartrix), for each MCDA scenario (from server?) a 3D matrix [dams,criteria,alternatives] 
	
	# Normalization procedure:
	#  get maximum and minimum criteria score for each criterion, each dam, produces two 2D matrices [dams, max/min criteria]
	#  for positive scores: norm = (f - f_min) / (f_max - f_min)
	#  for negative scores (like cost): norm = 1 - (f - f_max) / (f_min - f_max)
	#  result is 3D matrix with dam-specific criteria scores normalized by min and max criteria sampled over all alternatives
	
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
  
	CritMaxVector <- list("list", matrix_cols)
	  for ( k in 1:matrix_cols ){
		  CritMaxVector[[k]] <- max(DamsDataMatrix[,k,], na.rm=FALSE)}
	  CritMaxVector <- unlist(CritMaxVector)

	  CritMinVector <- list("list", matrix_cols)
	  for ( k in 1:matrix_cols ){
		  CritMinVector[[k]] <- min(DamsDataMatrix[,k,], na.rm=FALSE)}
	  CritMinVector <- unlist(CritMinVector)

	# debug
	message('min vector ', CritMinVector)
	message('max vector ', CritMaxVector)
	
	
	#iterate each alternative for min, max
	AltMaxVector <- list("list", matrix_levs)
	for ( p in 1:matrix_levs ){
	  AltMaxVector[[p]] <- max(DamsDataMatrix[,,p], na.rm=FALSE)}
	AltMaxVector <- unlist(AltMaxVector)
	
	AltMinVector <- list("list", matrix_levs)
	for ( k in 1:matrix_levs ){
	  AltMinVector[[p]] <- min(DamsDataMatrix[,,p], na.rm=FALSE)}
	AltMinVector <- unlist(AltMinVector)
	
	# debug
	message('min vector ', AltMinVector)
	message('max vector ', AltMaxVector)	

	#----------------------------------------
	# (DATA*PREFS): Build Score Matrix
	# score will be min/max normalized values from 0-1
	
	#----------------------------------------
	NormalizedMatrix <- array(data=NA, dim = c(8,14,5))
	# array of rows that use minimization (cost or damage-related)
	min_crit_columns <- c(4, 5, 6) 

	# make normalized values of each value in matrix 
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
		  for (p in 1:matrix_levs){
			x <- DamsDataMatrix[n,k,p]
			crit_min_x <- CritMinVector[k]
			crit_max_x <- CritMaxVector[k]
			alt_min_x <- AltMinVector[p]
			alt_max_x <- AltMaxVector[p]

      for (k in NormalizedMatrix[n,k,p]){ 
			
				if (k %in% min_crit_columns){
					# alternative method
					# maximize normalization
					((crit_max_x - x) / (crit_max_x - crit_min_x))
				}else{
					# for debugging by cell WSM uncomment next line
					# message('cell n, k, x, crit, result', n, ', ', k, ', ', x, ', ', ', ', (((x - min_x) / (max_x - min_x))) )

					# default method
					# minimize normilization
					((x - crit_min_x) / (crit_max_x - crit_min_x))
				}
			}
			#, error=function(e){
				#(NA)
			for (p in NormalizedMatrix[n,k,p]){ 
			  
			  if (p %in% min_alt_columns){
			    # alternative method
			    # maximize normalization
			    ((alt_max_x - x) / (alt_max_x - alt_min_x))
			  }else{
			    # for debugging by cell WSM uncomment next line
			    # message('cell n, k, x, crit, result', n, ', ', k, ', ', x, ', ', ', ', (((x - min_x) / (max_x - min_x))) )
			    
			    # default method
			    # minimize normilization
			    ((x - alt_min_x) / (alt_max_x - alt_min_x))
			  }
			}
			
			}

		   #End alternative (levels) for loop.
		  } #End criteria (columns) for loop.
	
		} #End dam (rows)
		message('Data column ', DamsDataMatrix[k])
		message('Normalized column ', NormalizedMatrix[k])

		message('Data level ', DamsDataMatrix[p])
		message('Normalized level ', NormalizedMatrix[p])
	
	# debug
	#message('NormalizedMatrix ', NormalizedMatrix)
	

	#----------------------------------------
	# WeightedScoreMatrix
	
	#----------------------------------------
	
	WeightedScoreMatrix <- (NormalizedMatrix[n,k,]*PrefMatrix[n,k,])

	WeightedScoreMatrix <- round(WeightedScoreMatrix,3) 
	
	Dam1Results <- WeightedScoreMatrix[1,,]
	Dam1Score   <- for (j in 1:matrix_levs){
  	  scoresum[[j]] <- sum(as.numeric(Dam1Results[1:14,1:ncol]))
  }
	scoresum <- unlist(scoresum)
	
	Dam2Results <- WeightedScoreMatrix[2,,]
	Dam2Score   <- for (j in 1:matrix_levs){
	  scoresum[[j]] <- sum(as.numeric(Dam2Results[1:14,1:ncol]))
	}
	scoresum <- unlist(scoresum)
	
	Dam3Results <- WeightedScoreMatrix[3,,]
	Dam3Score   <- for (j in 1:matrix_levs){
	  scoresum[[j]] <- sum(as.numeric(Dam3Results[1:14,1:ncol]))
	}
	scoresum <- unlist(scoresum)
	
	Dam4Results <- WeightedScoreMatrix[4,,]
	Dam4Score   <- for (j in 1:matrix_levs){
	  scoresum[[j]] <- sum(as.numeric(Dam4Results[1:14,1:ncol]))
	}
	scoresum <- unlist(scoresum)
	
	Dam5Results <- WeightedScoreMatrix[5,,]
	Dam5Score   <- for (j in 1:matrix_levs){
	  scoresum[[j]] <- sum(as.numeric(Dam5Results[1:14,1:ncol]))
	}
	scoresum <- unlist(scoresum)
	
	Dam6Results  <- WeightedScoreMatrix[6,,]
	Dam6Score    <- for (j in 1:matrix_levs){
	  scoresum[[j]] <- sum(as.numeric(Dam6Results[1:14,1:ncol]))
	}
	scoresum <- unlist(scoresum)
	
	Dam7Results <- WeightedScoreMatrix[7,,]
	Dam7Score   <- for (j in 1:matrix_levs){
	  scoresum[[j]] <- sum(as.numeric(Dam7Results[1:14,1:ncol]))
	}
	scoresum <- unlist(scoresum)
	
	Dam8Results <- WeightedScoreMatrix[8,,]
	Dam8Score   <- for (j in 1:matrix_levs){
	  scoresum[[j]] <- sum(as.numeric(Dam8Results[1:14,1:ncol]))
	}
	scoresum <- unlist(scoresum)

	# debug
	#message('IntermediateMatrix', IntermediateMatrix)

	#----------------------------------------
	# Score Sum
	
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

