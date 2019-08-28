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

source("plots.R")
library(plotly, warn.conflicts =  FALSE)
library(abind)

DamsData <- read.csv('DamsData.csv') # this is the dataset for the individual dams, where rows = dams and cols = criteria
DamsData <- data.frame(DamsData)
source(file='f_raw.RData') #these are the dams data from Sam's MOGA fitness function, where the'levels' data are for all 995 'scenarios' of 8 dams, 5 decision alts/dam
DamsDataMatrix <- as.array(f)
source(file='Decisions.RData') #this is 2 dimensions from f_raw: rows = 995 'scenarios' labeled with their decision alternative number, cols = 8 dams
Decisions <- as.array(Decisions)# we may not need this after all. 
TestData <- read.csv('TestData.csv')
RawCriteriaMatrix <- data.frame(TestData)#test preference data for 8 dams, 14 criteria each

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

WSM <- function(RawCriteriaMatrix, DamsDataMatrix, DamsData){
	# matrix setup
	matrix_cols <- length(criteria_inputs) # 14 default (output size)
	matrix_rows <- length(available_dams) # 8 default
	matrix_levs_ind <- length(available_alternatives)# 5 default
	matrix_levs <- length(1:995)
	message("Decision Criteria", matrix_cols, "Dams", matrix_rows, "Decision Alternatives", matrix_levs_ind, "Scenarios", matrix_levs)

	#----------------------------------------
	# Step A1 (PREFS, SINGLE DAM PROCEDURE): Build Preference Matrix with blank levels, no normalization
	# score will be raw values from 0-1 based on user input
	#----------------------------------------
	#Ind_* prefix indicates that we are dealing with individual or single dams, where the 5 decision alternatives are taken into account
	#locally and not as a part of the larger set of 8 dams.
	Ind_PrefMatrix <- array(data = NA, c(8,14)) #This is a 3-D blank matrix

	message("Fill User Preference Matrix")
	# weights in matrix
	for (n in 1:matrix_rows){
		for (k in 1:matrix_cols){
			x <- RawCriteriaMatrix[n,k]

			Ind_PrefMatrix[n,k] <- tryCatch({
				#message("A", x, ', ', crit_imp)
				(x)
			}, error=function(e){
				(NA)
			})
		} #End dams (rows) for loop.
	} #End criteria (columns) for loop.

	Ind_PrefMatrix <- array(rep(Ind_PrefMatrix,5), c(dim(Ind_PrefMatrix), 5))

	message("fill Ind Pref Matrix")

	#This subsets by dam (row) and transforms individual dam matrices

	#This subsets by dam (row)
	WestEnf_PrefMatrix <- subset(Ind_PrefMatrix[1,,])
	WestEnf_PrefMatrix <- data.frame(t(WestEnf_PrefMatrix))
	Med_PrefMatrix <- subset(Ind_PrefMatrix[2,,])
	Med_PrefMatrix <- data.frame(t(Med_PrefMatrix))
	Mill_PrefMatrix <- subset(Ind_PrefMatrix[3,,])
	Mill_PrefMatrix <- data.frame(t(Mill_PrefMatrix))
	EastMill_PrefMatrix <- subset(Ind_PrefMatrix[4,,])
	EastMill_PrefMatrix <- data.frame(t(EastMill_PrefMatrix))
	NorthTw_PrefMatrix <- subset(Ind_PrefMatrix[5,,])
	NorthTw_PrefMatrix <- data.frame(t(NorthTw_PrefMatrix))
	Dolby_PrefMatrix <- subset(Ind_PrefMatrix[6,,])
	Dolby_PrefMatrix <- data.frame(t(Dolby_PrefMatrix))
	MillLake_PrefMatrix <- subset(Ind_PrefMatrix[7,,])
	MillLake_PrefMatrix <- data.frame(t(MillLake_PrefMatrix))
	Rip_PrefMatrix <- subset(Ind_PrefMatrix[8,,])
	Rip_PrefMatrix <- data.frame(t(Rip_PrefMatrix))

	#----------------------------------------
	# (DATA): Data Normalization using Min / Max Vectors
	# Retrieve criteria values for each dam (referred to as DamsDataMartrix), for each MCDA scenario (from server?) a 3D matrix [dams,criteria,alternatives] 

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

	#This abind creates our 3D matrix
	Ind_DamsDataMatrix <- abind(KeepMaintain, Improve_Hydro, Improve_Fish, FishANDHydro, Remove, along = 3, force.array=TRUE)

	#This subsets by dam (row)
	WestEnf_DataMatrix <- subset(Ind_DamsDataMatrix[1,,])
	WestEnf_DataMatrix <- data.frame(t(WestEnf_DataMatrix))
	Med_DataMatrix <- subset(Ind_DamsDataMatrix[2,,])
	Med_DataMatrix <- data.frame(t(Med_DataMatrix))
	Mill_DataMatrix <- subset(Ind_DamsDataMatrix[3,,])
	Mill_DataMatrix <- data.frame(t(Mill_DataMatrix))
	EastMill_DataMatrix <- subset(Ind_DamsDataMatrix[4,,])
	EastMill_DataMatrix <- data.frame(t(EastMill_DataMatrix))
	NorthTw_DataMatrix <- subset(Ind_DamsDataMatrix[5,,])
	NorthTw_DataMatrix <- data.frame(t(NorthTw_DataMatrix))
	Dolby_DataMatrix <- subset(Ind_DamsDataMatrix[6,,])
	Dolby_DataMatrix <- data.frame(t(Dolby_DataMatrix))
	MillLake_DataMatrix <- subset(Ind_DamsDataMatrix[7,,])
	MillLake_DataMatrix <- data.frame(t(MillLake_DataMatrix))
	Rip_DataMatrix <- subset(Ind_DamsDataMatrix[8,,])
	Rip_DataMatrix <- data.frame(t(Rip_DataMatrix))

	#--------NORMALIZATION FOR INDIVIDUAL DAMS RESULTS-------------------
	# iterate each criteria for min,max

	WestEnf_MaxVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		WestEnf_MaxVector[[k]] <- max(WestEnf_DataMatrix[,k], na.rm=FALSE)}
	WestEnf_MaxVector <- unlist(WestEnf_MaxVector)

	WestEnf_MinVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		WestEnf_MinVector[[k]] <- min(WestEnf_DataMatrix[,k], na.rm=FALSE)}
	WestEnf_MinVector <- unlist(WestEnf_MinVector)

	# debug
	message('min vector ', WestEnf_MinVector)
	message('max vector ', WestEnf_MaxVector)
	#----------------------------------
	Med_MaxVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		Med_MaxVector[[k]] <- max(Med_DataMatrix[,k], na.rm=FALSE)}
	Med_MaxVector <- unlist(Med_MaxVector)

	Med_MinVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		Med_MinVector[[k]] <- min(Med_DataMatrix[,k], na.rm=FALSE)}
	Med_MinVector <- unlist(Med_MinVector)

	# debug
	message('min vector ', Med_MinVector)
	message('max vector ', Med_MaxVector)
	#----------------------------------
	Mill_MaxVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		Mill_MaxVector[[k]] <- max(Mill_DataMatrix[,k], na.rm=FALSE)}
	Mill_MaxVector <- unlist(Mill_MaxVector)

	Mill_MinVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		Mill_MinVector[[k]] <- min(Mill_DataMatrix[,k], na.rm=FALSE)}
	Mill_MinVector <- unlist(Mill_MinVector)

	# debug
	message('min vector ', Mill_MinVector)
	message('max vector ', Mill_MaxVector)
	#----------------------------------
	EastMill_MaxVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		EastMill_MaxVector[[k]] <- max(EastMill_DataMatrix[,k], na.rm=FALSE)}
	EastMill_MaxVector <- unlist(EastMill_MaxVector)

	EastMill_MinVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		EastMill_MinVector[[k]] <- min(EastMill_DataMatrix[,k], na.rm=FALSE)}
	EastMill_MinVector <- unlist(EastMill_MinVector)

	# debug
	message('min vector ', EastMill_MinVector)
	message('max vector ', EastMill_MaxVector)
	#----------------------------------
	NorthTw_MaxVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		NorthTw_MaxVector[[k]] <- max(NorthTw_DataMatrix[,k], na.rm=FALSE)}
	NorthTw_MaxVector <- unlist(NorthTw_MaxVector)

	NorthTw_MinVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		NorthTw_MinVector[[k]] <- min(NorthTw_DataMatrix[,k], na.rm=FALSE)}
	NorthTw_MinVector <- unlist(NorthTw_MinVector)

	# debug
	message('min vector ', NorthTw_MinVector)
	message('max vector ', NorthTw_MaxVector)
	#----------------------------------
	Dolby_MaxVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		Dolby_MaxVector[[k]] <- max(Dolby_DataMatrix[,k], na.rm=FALSE)}
	Dolby_MaxVector <- unlist(Dolby_MaxVector)

	Dolby_MinVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		Dolby_MinVector[[k]] <- min(Dolby_DataMatrix[,k], na.rm=FALSE)}
	Dolby_MinVector <- unlist(Dolby_MinVector)

	# debug
	message('min vector ', Dolby_MinVector)
	message('max vector ', Dolby_MaxVector)
	#----------------------------------
	MillLake_MaxVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		MillLake_MaxVector[[k]] <- max(MillLake_DataMatrix[,k], na.rm=FALSE)}
	MillLake_MaxVector <- unlist(MillLake_MaxVector)

	MillLake_MinVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		MillLake_MinVector[[k]] <- min(MillLake_DataMatrix[,k], na.rm=FALSE)}
	MillLake_MinVector <- unlist(MillLake_MinVector)

	# debug
	message('min vector ', MillLake_MinVector)
	message('max vector ', MillLake_MaxVector)
	#----------------------------------
	Rip_MaxVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		Rip_MaxVector[[k]] <- max(Rip_DataMatrix[,k], na.rm=FALSE)}
	Rip_MaxVector <- unlist(Rip_MaxVector)

	Rip_MinVector <- list("list", matrix_cols)
	for ( k in 1:matrix_cols ){
		Rip_MinVector[[k]] <- min(Rip_DataMatrix[,k], na.rm=FALSE)}
	Rip_MinVector <- unlist(Rip_MinVector)

	# debug
	message('min vector ', Rip_MinVector)
	message('max vector ', Rip_MaxVector)
	#----------------------------------------
	# (DATA*PREFS): Build Score Matrix for ind. dams
	# score will be min/max normalized values from 0-1
	# array of rows that use minimization (cost or damage-related)
	min_crit_columns <- c(4, 5, 6) 
	#----------------------------------------
	WestEnf_NormalizedMatrix <- array(data=NA, dim = c(5, 14))

	# make normalized values of each value in matrix 
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			x <- WestEnf_DataMatrix[n,k]
			crit_min_x <- WestEnf_MinVector[k]
			crit_max_x <- WestEnf_MaxVector[k]

			WestEnf_NormalizedMatrix[n,k] <- tryCatch({ 

				if (k %in% min_crit_columns){
					# alternative method
					# minimize normalization
					(1-(x-crit_min_x) / (crit_max_x - crit_min_x))
				}else{
					# for debugging by cell WSM uncomment next line
					# message('cell n, k, x, crit, result', n, ', ', k, ', ', x, ', ', ', ', (((x - crit_min_x) / (crit_max_x - crit_min_x))) )

					# default method
					# maximize normilization
					((x - crit_min_x) / (crit_max_x - crit_min_x))
				}
			}, error=function(e){
				(NA)
			})
		}
	}


	WestEnf_NormalizedMatrix[is.nan(WestEnf_NormalizedMatrix)] <-0

	message('Data column ', WestEnf_DataMatrix[k])
	message('Normalized column ', WestEnf_NormalizedMatrix[k])

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

	Ind_scoresum <- as.data.frame(rbind(scoresum1, scoresum2, scoresum3, scoresum4, scoresum5, scoresum6, scoresum7, scoresum8))

	#------------------------------------------------------
	#  STEP A2: MULTI-DAM PROCEDURE FOR PREERENCES
	#------------------------------------------------------
	PrefMatrix <- array(data = NA, c(8,14)) #This is a 3-D blank matrix 

	message("Fill User Preference Matrix")
	# weights in matrix
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			x <- RawCriteriaMatrix[n,k]

			PrefMatrix[n,k] <- tryCatch({
				#message("A", x, ', ', crit_imp)
				(x)
			}, error=function(e){
				(NA)
			})
		} #End dams (rows) for loop.
	} #End criteria (columns) for loop.

	PrefMatrix <- array(rep(PrefMatrix,995), c(dim(PrefMatrix), 995)) #will address this later


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
	message('min vector ', CritMinVector)
	message('max vector ', CritMaxVector)


	#iterate each alternative for min, max
	AltMaxVector <- list("list", matrix_levs)
	for ( p in 1:matrix_levs ){
		AltMaxVector[[p]] <- max(DamsDataMatrix[,,p], na.rm=FALSE)}
	AltMaxVector <- unlist(AltMaxVector)

	AltMinVector <- list("list", matrix_levs)
	for ( p in 1:matrix_levs ){
		AltMinVector[[p]] <- min(DamsDataMatrix[,,p], na.rm=FALSE)}
	AltMinVector <- unlist(AltMinVector)

	# debug
	message('min vector ', AltMinVector)
	message('max vector ', AltMaxVector)	

	#----------------------------------------
	# (DATA*PREFS): Build Score Matrix
	# score will be min/max normalized values from 0-1

	#----------------------------------------
	NormalizedMatrix <- array(data=NA, dim = c(8,14,995))
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
						(1-(x-crit_min_x) / (crit_max_x - crit_min_x))
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

	NormalizedMatrix[is.nan(NormalizedMatrix)] <-0

	message('Data column ', DamsDataMatrix[k])
	message('Normalized column ', NormalizedMatrix[k])

	# debug
	#message('NormalizedMatrix ', NormalizedMatrix)


	#----------------------------------------
	# WeightedScoreMatrix

	#----------------------------------------

	WeightedScoreMatrix <- (NormalizedMatrix*PrefMatrix)

	WeightedScoreMatrix <- round(WeightedScoreMatrix,3) 

	#----------------------------------------
	# Score Sum

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
	WSMResults <- list(Ind_WeightedScoreMatrix, Ind_scoresum, scoresum_total, fname)

	return(WSMResults)
} # end of WSM
message("wsm_graph_test loaded")


TableMatrix <- WSMResults[1]

Ind_MCDA_score <- WSMResults[2]

MCDA_score <- WSMResults[3]

map_name <- WSMResults[4]

#would really like to differentiate between levels somehow in this table (below)...this code doesn't work
WSMTableOutput <- as.array(TableMatrix, 
                        dim = c(8,14,5),
                        dimnames = list(c('WestEnfield','Medway','Millinocket','E.Millinocket','North Twin','Dolby','Millinocket Lake','Ripogenus'),
                                        c("FishBiomass", "RiverRec","Reservoir", "ProjectCost", "Safety", "NumProperties", "ElectricityGeneration",
                                          "AvoidEmissions", "IndigenousLifeways", "IndustrialHistory", "CommunityIdentity", "Aesthetics", "Health", "Justice"),
                                        c("KeepMaintain", "ImproveHydro", "ImproveFish", "ImproveFishANDHydro", "Remove")))
# would also like to add, between levels, a column for the Ind_scoresum
## this ones different because it has sum row

# -------------------------------END REWRITE BY DAM------------------------------#

#----------------------------------------
# Final Outputs
#----------------------------------------
# final output table commented out due to redundancy
#dam display names
dam_names <- as.list(c('WestEnfield','Medway','Millinocket','E.Millinocket','North Twin','Dolby','Millinocket Lake','Ripogenus'))

# alternative display names (for labeling tables and graphs)
alternative_names <- as.list(c(
  "Keep and Maintain Dam",
  "Improve Hydro",
  "Improve Fish Passage",
  "Improve Hydro AND Fish Passage",
  "Remove Dam"
))


## stacked bars data table for Individual Dam results
Score_compare <- as.matrix(Ind_scoresum)
colnames(Score_compare) <- alternative_names
rownames(Score_compare) <- dam_names

# Graph ALL DAM alternative scores with adjacent bars grouped by dam
WSMPlot <- barplot(t(Score_compare), main="Dam Decision Recommendation Comparison", ylab= "Decision Alternative Score",
        beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
       bty="n", fill=rainbow(5));
#-------------------------------------------------------
## stacked bars data table for West Enfield Dam results
Score1 <- as.matrix(scoresum1)
colnames(Score1) <- alternative_names

# Graph West Enfield alternative scores
WSMPlot1 <- barplot((scoresum1), main="West Enfield Dam Recommendation", ylab= "Decision Alternative Score",
                    names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
       bty="n", fill=rainbow(5));

#-------------------------------------------------------
## stacked bars data table for Medway Dam results
Score2 <- as.matrix(scoresum2)
colnames(Score2) <- alternative_names

# Graph  alternative scores
WSMPlot2 <- barplot((scoresum2), main="Medway Dam Recommendation", ylab= "Decision Alternative Score",
                    names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
       bty="n", fill=rainbow(5));
#-------------------------------------------------------
## stacked bars data table for Millinocket/Quakish Dam results
Score3 <- as.matrix(scoresum3)
colnames(Score3) <- alternative_names

# Graph alternative scores
WSMPlot3 <- barplot((scoresum3), main="Millinocket Dam Recommendation", ylab= "Decision Alternative Score",
                    names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
       bty="n", fill=rainbow(5));
#-------------------------------------------------------
## stacked bars data table for East Millinocket Dam results
Score4 <- as.matrix(scoresum4)
colnames(Score4) <- alternative_names

# Graph alternative scores
WSMPlot4 <- barplot((scoresum4), main="East Millinocket Dam Recommendation", ylab= "Decision Alternative Score",
                    names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
       bty="n", fill=rainbow(5));
#-------------------------------------------------------
## stacked bars data table for North Twin Dam results
Score5 <- as.matrix(scoresum5)
colnames(Score5) <- alternative_names

# Graph alternative scores
WSMPlot5 <- barplot((scoresum5), main="North Twin Dam Recommendation", ylab= "Decision Alternative Score",
                    names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
       bty="n", fill=rainbow(5));

#-------------------------------------------------------
## stacked bars data table for Dolby Dam results
Score6 <- as.matrix(scoresum6)
colnames(Score6) <- alternative_names

# Graph alternative scores
WSMPlot6 <- barplot((scoresum6), main="Dolby Dam Recommendation", ylab= "Decision Alternative Score",
                    names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
       bty="n", fill=rainbow(5));

#-------------------------------------------------------
## stacked bars data table for Millinocket Lake Dam results
Score7 <- as.matrix(scoresum7)
colnames(Score7) <- alternative_names

# Graph alternative scores
WSMPlot7 <- barplot((scoresum7), main="Millinocket Lake Dam Recommendation", ylab= "Decision Alternative Score",
                    names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
       bty="n", fill=rainbow(5));

#-------------------------------------------------------
## stacked bars data table for Ripogenus Dam results
Score8 <- as.matrix(scoresum8)
names(Score8) <- alternative_names

# Graph alternative scores
WSMPlot8 <- barplot((scoresum8), main="Ripogenus Dam Recommendation", ylab= "Decision Alternative Score",
                    names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
       bty="n", fill=rainbow(5));

#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt1 <- as.matrix(rbind(Dam1Results,scoresum1))
colnames(CritAlt1) <- alternative_names
rownames(CritAlt1) <- c(criteria_inputs, "sum")


# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
barplot((Dam1Results), main="WestEnfield", ylab="MCDA Score", col=rainbow(14),
        cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7) 

# Place the legend at (6,30) using rainbow colors
#legend(6, 30, names(criteria_inputs), cex=0.6, fill=rainbow(14));






