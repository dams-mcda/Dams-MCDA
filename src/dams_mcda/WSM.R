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

# set working directory only if it exists
has_wd <- tryCatch({
	workdir <- "~/Beatrice2/R_ELF/R_NEST/MCDA_App_Shiny/MCDA_06262019/src/dams_mcda"
	setwd(workdir)
	message("set Working Directory: ", workdir)
}, error=function(e){
	message("Working Directory does NOT exist.")
})


DamsData <- read.csv('DamsData.csv')
DamsData <- data.frame(DamsData)
source(file='f_raw.RData')
DamsDataMatrix <- as.array(f)
#source(file='Decisions.RData')
#Decisions <- as.array(Decisions)

TestData <- c(0, 0.2, 0.05, 0.025, 0.05, 0.75, 0.2, 0.1, 0.1, 0.2, 0, 0, 0, 0)

RawCriteriaMatrix <- array(TestData, c(8, 14,5))

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

# list of dams
available_dams <- seq(1:8)
# list of alternatives
available_alternatives <- seq(1:5)

# matrix setup
matrix_cols <- length(criteria_inputs) # 14 default (output size)
matrix_rows <- length(available_dams) # 8 default
matrix_levs_ind <- length(available_alternatives) # 5 default
matrix_levs <- length(1:995) # TODO: 995?


WSM <- function(RawCriteriaMatrix, DamsDataMatrix, Ind_DamsDataMatrix){
	message("Decision Criteria", matrix_cols, "Dams", matrix_rows, "Decision Alternatives", matrix_levs )

	#----------------------------------------
	# Step A1 (PREFS, SINGLE DAM PROCEDURE): Build Preference Matrix with blank levels, no normalization
	# score will be raw values from 0-1 based on user input
	#----------------------------------------
	Ind_PrefMatrix <- array(data = NA, c(8,14,5)) # blank 3-D matrix

	message("Fill User Preference Matrix")
	# weights in matrix
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
		  for (p in 1:matrix_levs_ind){
			x <- RawCriteriaMatrix[n,k,p]

			Ind_PrefMatrix[n,k,p] <- tryCatch({
				#message("A", x, ', ', crit_imp)
				(x)
			}, error=function(e){
				(NA)
			})
		  } #End dams (rows) for loop.
	  } #End criteria (columns) for loop.
  } #End alternatives (levels) forloop


	message("fill Ind Pref Matrix")
	#----------------------------------------
	# (DATA): Data Normalization using Min / Max Vectors
	# Retrieve criteria values for each dam (referred to as DamsDataMartrix), for each MCDA scenario (from server?) a 3D matrix [dams,criteria,alternatives] 
	#
	# Normalization procedure:
	#  get maximum and minimum criteria value for each criterion, each dam, produces two 2D matrices [dams, max/min criteria]
	#  for positive values: norm = (f - f_min) / (f_max - f_min)
	#  for negative values (like cost): norm = 1 - (f - f_max) / (f_min - f_max)
	#  result is 3D matrix with dam-specific criteria values normalized by min and max criteria sampled over all alternatives
	#----------------------------------------

	#retrieve DamsData to manipulate into DamsDataMatrix
	Ind_DamsDataMatrix <- array(data=NA, dim = c(8, 14, 5)) #creates empty 3d array in shape we want

	KeepMaintain <- cbind(DamsData$FishBiomass_KeepMaintain, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_KeepMaintain, DamsData$Damage, 
	                      DamsData$Properties, DamsData$AvgAnnualGen, DamsData$EmissionsReduc, 
	                      DamsData$Culture_KeepMaintain, DamsData$History_KeepMaintain, DamsData$Community_KeepMaintain, DamsData$Aesthetics_KeepMaintain, 
	                      DamsData$Health_KeepMaintain, DamsData$Justice_KeepMaintain)
	Improve_Hydro <- cbind(DamsData$FishBiomass_ImproveHydro, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_ImproveHydro, DamsData$Damage, 
	                       DamsData$Properties,DamsData$AvgAnnualGen_Add, DamsData$EmissionsReduc_Add, 
	                       DamsData$Culture_ImproveHydro, DamsData$History_ImproveHydro, DamsData$Community_ImproveHydro, DamsData$Aesthetics_ImproveHydro, 
	                       DamsData$Health_ImproveHydro, DamsData$Justice_ImproveHydro)
	Improve_Fish <- cbind(DamsData$FishBiomass_ImproveFish, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_ImproveFish, DamsData$Damage, 
	                      DamsData$Properties,DamsData$AvgAnnualGen, DamsData$EmissionsReduc,  
	                      DamsData$Culture_ImproveFish, DamsData$History_ImproveFish, DamsData$Community_ImproveFish, DamsData$Aesthetics_ImproveFish, 
	                      DamsData$Health_ImproveFish, DamsData$Justice_ImproveFish)
	FishANDHydro <- cbind(DamsData$FishBiomass_FishANDHydro, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_FishANDHydro, DamsData$Damage, 
	                      DamsData$Properties, DamsData$AvgAnnualGen_Add, DamsData$EmissionsReduc_Add, 
	                      DamsData$Culture_FishANDHydro, DamsData$History_FishANDHydro, DamsData$Community_FishANDHydro, DamsData$Aesthetics_FishANDHydro,
	                      DamsData$Health_FishANDHydro, DamsData$Justice_FishANDHydro)
	Remove <- cbind(DamsData$FishBiomass_Remove, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_Remove, DamsData$Damage, 
	                DamsData$Properties, DamsData$AvgAnnualGen_Rem, DamsData$EmissionsReduc_Rem,  
	                DamsData$Culture_Remove, DamsData$History_Remove, DamsData$Community_Remove, DamsData$Aesthetics_Remove, 
	                DamsData$Health_Remove, DamsData$Justice_Remove)

	# abind creates our 3D matrix, each third dimension frame the same
	Ind_DamsDataMatrix <- abind(KeepMaintain, Improve_Hydro, Improve_Fish, FishANDHydro, Remove, along = 3, force.array=TRUE)

	#--------NORMALIZATION FOR INDIVIDUAL DAMS RESULTS-------------------

	# iterate each criteria for min, max values

	# Max for Criteria
	CritMaxVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		CritMaxVector[[k]] <- max(Ind_DamsDataMatrix[,k,], na.rm=FALSE)}
	CritMaxVector <- unlist(CritMaxVector)

	# Min for Criteria
	CritMinVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		CritMinVector[[k]] <- min(Ind_DamsDataMatrix[,k,], na.rm=FALSE)}
	CritMinVector <- unlist(CritMinVector)

	# debug
	message('min vector ', CritMinVector)
	message('max vector ', CritMaxVector)


	#----------------------------------------
	# (DATA*PREFS): Build Score Matrix
	# score will be min/max normalized values from 0-1
	#----------------------------------------

	Ind_NormalizedMatrix <- array(data=NA, dim = c(8,14,5))
	# array of rows that use minimization (cost or damage-related)
	min_crit_columns <- c(4, 5, 6)

	# make normalized values of each value in matrix 
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
		  for (p in 1:matrix_levs_ind){
			x <- Ind_DamsDataMatrix[n,k,p]
			crit_min_x <- CritMinVector[k]
			crit_max_x <- CritMaxVector[k]

      Ind_NormalizedMatrix[n,k,p] <- tryCatch({ 
			
				if (k %in% min_crit_columns){
					# alternative method
					# maximize normalization
					((1-(x-crit_max_x)) / (crit_max_x - crit_min_x))
				}else{
					# for debugging by cell WSM uncomment next line
					# message('cell n, k, x, crit, result', n, ', ', k, ', ', x, ', ', ', ', (((x - crit_min_x) / (crit_max_x - crit_min_x))) )

					# default method
					# minimize normilization
					((x - crit_min_x) / (crit_max_x - crit_min_x))
				}
      }, error=function(e){
        (NA)
      })
		  }
		}
	}
	
	Ind_NormalizedMatrix[is.nan(Ind_NormalizedMatrix)] <-0
	
	message('Data column ', Ind_DamsDataMatrix[k])
	message('Normalized column ', Ind_NormalizedMatrix[k])

	# debug
	#message('NormalizedMatrix ', NormalizedMatrix)
	

	#----------------------------------------
	# WeightedScoreMatrix
	
	#----------------------------------------
	
	Ind_WeightedScoreMatrix <- (Ind_NormalizedMatrix*Ind_PrefMatrix)

	Ind_WeightedScoreMatrix <- round(Ind_WeightedScoreMatrix,3) 
	
	#------Dam 1--------------
	Dam1Results <- Ind_WeightedScoreMatrix[1,,] #Dam 1 Weighted Matrix
	scoresum1 <- list("list", matrix_levs_ind)
	
	for (j in 1:matrix_levs_ind){
  	  scoresum1[[j]] <- sum(as.numeric(Dam1Results[j, 1:matrix_levs_ind]))
  }
	scoresum1 <- unlist(scoresum1)
	
	#------Dam 2--------------
	Dam2Results <- Ind_WeightedScoreMatrix[2,,] #Dam 2 Weighted Matrix
	scoresum2 <- list("list", matrix_levs_ind)
	
	for (j in 1:matrix_levs_ind){
	  scoresum2[[j]] <- sum(as.numeric(Dam2Results[j, 1:matrix_levs_ind]))
	}
	scoresum2 <- unlist(scoresum2)
	
	#------Dam 3--------------
	Dam3Results <- Ind_WeightedScoreMatrix[3,,] #Dam 3 Weighted Matrix
	scoresum3 <- list("list", matrix_levs_ind)
	
  for (j in 1:matrix_levs_ind){
	  scoresum3[[j]] <- sum(as.numeric(Dam3Results[j, 1:matrix_levs_ind]))
	}
	scoresum3 <- unlist(scoresum3)
	
	#------Dam 4--------------
	Dam4Results <- Ind_WeightedScoreMatrix[4,,] #Dam 4 Weighted Matrix
	scoresum4 <- list("list", matrix_levs_ind)
	
	for (j in 1:matrix_levs_ind){
	  scoresum4[[j]] <- sum(as.numeric(Dam4Results[j, 1:matrix_levs_ind]))
	}
	scoresum4 <- unlist(scoresum4)
	
	#------Dam 5--------------
	Dam5Results <- Ind_WeightedScoreMatrix[5,,] #Dam 5 Weighted Matrix
	scoresum5 <- list("list", matrix_levs_ind)
	
	for (j in 1:matrix_levs_ind){
	  scoresum5[[j]] <- sum(as.numeric(Dam5Results[j, 1:matrix_levs_ind]))
	}
	scoresum5 <- unlist(scoresum5)
	
	#------Dam 6--------------
	Dam6Results  <- Ind_WeightedScoreMatrix[6,,] #Dam 6 Weighted Matrix
	scoresum6 <- list("list", matrix_levs_ind)
	
	for (j in 1:matrix_levs_ind){
	  scoresum6[[j]] <- sum(as.numeric(Dam6Results[j, 1:matrix_levs_ind]))
	}
	scoresum6 <- unlist(scoresum6)
	
	#------Dam 7--------------
	Dam7Results <- Ind_WeightedScoreMatrix[7,,] #Dam 7 Weighted Matrix
	scoresum7 <- list("list", matrix_levs_ind)
	
	for (j in 1:matrix_levs_ind){
	  scoresum7[[j]] <- sum(as.numeric(Dam7Results[j, 1:matrix_levs_ind]))
	}
	scoresum7 <- unlist(scoresum7)
	
	#------Dam 8--------------
	Dam8Results <- Ind_WeightedScoreMatrix[8,,] #Dam 8 Weighted Matrix
	scoresum8 <- list("list", matrix_levs_ind)
	
	for (j in 1:matrix_levs_ind){
	  scoresum8[[j]] <- sum(as.numeric(Dam8Results[j, 1:matrix_levs_ind]))
	}
	scoresum8 <- unlist(scoresum8)

	#------------------------------------------------------
	#  STEP A2: MULTI-DAM PROCEDURE FOR PREERENCES
	#------------------------------------------------------
	PrefMatrix <- array(data = NA, c(8,14,995)) # This is a 3-D blank matrix

	message("Fill User Preference Matrix")
	# weights in matrix
	for (k in 1:matrix_cols){
	  for (n in 1:matrix_rows){
	    for (p in 1:matrix_levs){
	      x <- RawCriteriaMatrix[n,k,p]

	      PrefMatrix[n,k,p] <- tryCatch({
	        #message("A", x, ', ', crit_imp)
	        (x)
	      }, error=function(e){
	        (NA)
	      })
	    } #End dams (rows) for loop.
	  } #End criteria (columns) for loop.
	} #End alternatives (levels) forloop

	message("fill multi-dam Pref Matrix")
	#--------NORMALIZATION FOR MULTI-DAMS RESULTS-------------------
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
	message('min criteria vector ', CritMinVector)
	message('max criteria vector ', CritMaxVector)


	# find alternative min, max values
	# Max for Alternative
	AltMaxVector <- list("list", matrix_levs)
	for ( p in 1:matrix_levs ){
	  AltMaxVector[[p]] <- max(DamsDataMatrix[,,p], na.rm=FALSE)
	}
	AltMaxVector <- unlist(AltMaxVector)

	# Min for Alternative
	AltMinVector <- list("list", matrix_levs)
	for ( p in 1:matrix_levs ){
	  AltMinVector[[p]] <- min(DamsDataMatrix[,,p], na.rm=FALSE)
	}
	AltMinVector <- unlist(AltMinVector)

	# debug
	message('min alternative vector ', AltMinVector)
	message('max alternative vector ', AltMaxVector)

	#----------------------------------------
	# (DATA*PREFS): Build Score Matrix
	# score will be min/max normalized values from 0-1
	#----------------------------------------

	NormalizedMatrix <- array(data=NA, dim = c(8, 14, 995))
	# array of rows that use minimization (cost or damage-related)
	min_crit_columns <- c(4, 5, 6)

	# make normalized values of each value in matrix
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			for (p in 1:matrix_levs){
				x <- DamsDataMatrix[n,k,p]
				crit_min_x <- CritMinVector[k]
				crit_max_x <- CritMaxVector[k]

				NormalizedMatrix[n,k,p] <- tryCatch({

					if (k %in% min_crit_columns){
						# alternative method
						# maximize normalization
						((1-(x-crit_max_x)) / (crit_max_x - crit_min_x))
					}else{
						# for debugging by cell WSM uncomment next line
						# message('cell n, k, x, crit, result', n, ', ', k, ', ', x, ', ', ', ', (((x - crit_min_x) / (crit_max_x - crit_min_x))) )

						# default method
						# minimize normilization
						((x - crit_min_x) / (crit_max_x - crit_min_x))
					}
				}, error=function(e){
					(NA)
				})
			}
		}
	}

	# set all NA (NaN) values to 0
	NormalizedMatrix[is.nan(NormalizedMatrix)] <- 0
	# debug
	#message('NormalizedMatrix ', NormalizedMatrix)

	message('Data column ', DamsDataMatrix[k])
	message('Normalized column ', NormalizedMatrix[k])

	#----------------------------------------
	# WeightedScoreMatrix
	#----------------------------------------

	WeightedScoreMatrix <- round((NormalizedMatrix*PrefMatrix), 3)

	#----------------------------------------
	# Score Sum
	#
	# Weighted sum procedure:
	#  multiply all normalized scores by prefrence weights
	#  sum normalized, weighted criteria scores for each dam
	#  sum this across all dams for each scenario
	#-----------------------------------------

	#declare a weighted sum variable
	scoresum_total <- rep(0,dim(NormalizedMatrix)[3])

	#multiply crit scores by user preferences
	for (i in 1:dim(NormalizedMatrix)[3]){
		WSMMatrix <- NormalizedMatrix[,,i] * PrefMatrix[,,i]
		scoresum_total[i] <- sum(WSMMatrix) #this sums everything in each scenario after they are preferenced. Should be fine as order doesn't matter at this point.
	}

	#-----------------------------------------
	# Rank:
	#  may need to reshape the array produced by the weighted sum procedure
	#  sort the array by descending order, highest score comes first, and record the indices of the top ranked scenario
	# Retrieve table, map of highest ranked scenario:
	#  use server url or whatever, searching for map name with the matching index number
	#  take the map image, table, and stick them in the webpage
	#----------------------------------------

	#order scenarios by rank: largest score first
	idxRank <- order(scoresum_total,decreasing=TRUE)

	#use scenario idxRank[1] to find corresponding map name
	fname <- sprintf('maps/Penobscot_MO_14_%d.png',idxRank[1])
	print(fname)

	# warning adding things to list has side effects!
	WSMResults <- list(WeightedScoreMatrix, scoresum_total, fname)

	return(WSMResults)
}


