# WSM
#---------------------------------------- generates the MDCA Output
#
# Returns list (in order)
#     Ind_WeightedScoreMatrix #weighted scores for individual dams
#     Ind_scoresum #weighted sum (MCDA scores) for individual dams
#     scoresum_total #weighted sum (MCDA scores) for multi-dam
#     fname #map name
#     AllDataMatrix
#     Ind_NormalizedMatrix # normalized data for individual dams
#     idxRank #ranked scenarios (995)
#     WeightedScoreMatrix #weighted scores for multi-dam
#
# Required Inputs:
#     RawCriteriaMatrix: 2D raw score matrix
#     NormalizedMatrix: 3D normalized multi-dam (995 scenario) data from MOGA
#     DamsData: 2D criteria data (not normalized) for individual dams, including social/cultural pre-survey data
#     Decisions: 2D dams and scenarios numbered by outcome code, see below
#codes:
#0 = remove dam
#1 = keep as is
#2 = improve hydropower
#3 = improve fish passage
#4 = improve both

source("plots.R")
library(plotly, warn.conflicts =  FALSE)
library(abind)
library(data.table)

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
matrix_levs_ind <- length(available_alternatives)# 5 default
matrix_levs <- length(1:995)


WSM <- function(RawCriteriaMatrix, NormalizedMatrix, DamsData, Decisions){
	message("Decision Criteria ", matrix_cols, " Dams ", matrix_rows, " Decision Alternatives ", matrix_levs_ind, " Scenarios ", matrix_levs)

	colnames(Decisions) <- dam_names

	#----------------------------------------
	# SINGLE DAM PROCEDURE FOR PREFERENCES
	#
	# Build Preference Matrix with blank levels, no normalization
	# score will be raw values from 0-1 based on user input
	#----------------------------------------
	#Ind_* prefix indicates that we are dealing with individual or single dams, where the 5 decision alternatives are taken into account
	#locally and not as a part of the larger set of 8 dams.
	Ind_PrefMatrix <- array(data = NA, dim=c(8,14)) #This is a 2D blank matrix

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

	Ind_PrefMatrix <- array(rep(Ind_PrefMatrix,5), dim=c(dim(Ind_PrefMatrix), 5))

	#This subsets by dam (row) and transforms individual dam matrices
	WestEnf_PrefMatrix <- subset(Ind_PrefMatrix[1,,])
	WestEnf_PrefMatrix <- data.frame(t(WestEnf_PrefMatrix))
	Med_PrefMatrix <- subset(Ind_PrefMatrix[2,,])
	Med_PrefMatrix <- data.frame(t(Med_PrefMatrix))
	EastMill_PrefMatrix <- subset(Ind_PrefMatrix[3,,])
	EastMill_PrefMatrix <- data.frame(t(EastMill_PrefMatrix))
	Dolby_PrefMatrix <- subset(Ind_PrefMatrix[4,,])
	Dolby_PrefMatrix <- data.frame(t(Dolby_PrefMatrix))
	NorthTw_PrefMatrix <- subset(Ind_PrefMatrix[5,,])
	NorthTw_PrefMatrix <- data.frame(t(NorthTw_PrefMatrix))
	Mill_PrefMatrix <- subset(Ind_PrefMatrix[6,,])
	Mill_PrefMatrix <- data.frame(t(Mill_PrefMatrix))
	MillLake_PrefMatrix <- subset(Ind_PrefMatrix[7,,])
	MillLake_PrefMatrix <- data.frame(t(MillLake_PrefMatrix))
	Rip_PrefMatrix <- subset(Ind_PrefMatrix[8,,])
	Rip_PrefMatrix <- data.frame(t(Rip_PrefMatrix))

	#------------------------------------------------------
	#  MULTI-DAM PROCEDURE FOR PREERENCES
	#------------------------------------------------------
	PrefMatrix <- array(data = NA, dim=c(8,14)) #This is a 2D blank matrix

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

	PrefMatrix <- array(data=rep(PrefMatrix,995), dim=c(dim(PrefMatrix), 995)) #will address this later

	#----------------------------------------
	# SINGLE DAM DATA NORMALIATION PROCEDURE
	#
	# Data Normalization using Min / Max Vectors
	# Retrieve criteria values for each dam (referred to as Ind_DamsDataMartrix), for each MCDA scenario (from server?) a 3D matrix [dams,criteria,alternatives] 
	#
	# Normalization procedure:
	#  get maximum and minimum criteria value for each criterion, each dam, produces two 2D matrices [dams, max/min criteria]
	#  for positive values: norm = (f - f_min) / (f_max - f_min)
	#  for negative values (like cost): norm = 1 - (f - f_max) / (f_min - f_max)
	#  result is 3D matrix with dam-specific criteria values normalized by min and max criteria sampled over all alternatives
	#----------------------------------------

	#retrieve DamsData to manipulate into DamsDataMatrix
	Ind_DamsDataMatrix <- array(data=NA, dim = c(8, 14, 5)) #creates empty 3d array in shape we want

	Remove <- cbind(DamsData$FishBiomass_Remove, DamsData$RiverRec_Rem, DamsData$ResStorage_Rem, DamsData$Cost_Remove, DamsData$Damage_Rem,
	                DamsData$Properties_Rem, DamsData$AvgAnnualGen_Rem, DamsData$EmissionsReduc_Rem,
	                DamsData$Culture_Remove, DamsData$History_Remove, DamsData$Community_Remove, DamsData$Aesthetics_Remove,
	                DamsData$Health_Remove, DamsData$Justice_Remove)
	Improve_Fish <- cbind(DamsData$FishBiomass_ImproveFish, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_ImproveFish, DamsData$Damage,
						  DamsData$Properties,DamsData$AvgAnnualGen, DamsData$EmissionsReduc,
						  DamsData$Culture_ImproveFish, DamsData$History_ImproveFish, DamsData$Community_ImproveFish, DamsData$Aesthetics_ImproveFish,
						  DamsData$Health_ImproveFish, DamsData$Justice_ImproveFish)
	Improve_Hydro <- cbind(DamsData$FishBiomass_ImproveHydro, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_ImproveHydro, DamsData$Damage,
	                       DamsData$Properties,DamsData$AvgAnnualGen_Add, DamsData$EmissionsReduc_Add,
	                       DamsData$Culture_ImproveHydro, DamsData$History_ImproveHydro, DamsData$Community_ImproveHydro, DamsData$Aesthetics_ImproveHydro,
	                       DamsData$Health_ImproveHydro, DamsData$Justice_ImproveHydro)
	FishANDHydro <- cbind(DamsData$FishBiomass_FishANDHydro, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_FishANDHydro, DamsData$Damage,
						  DamsData$Properties, DamsData$AvgAnnualGen_Add, DamsData$EmissionsReduc_Add,
						  DamsData$Culture_FishANDHydro, DamsData$History_FishANDHydro, DamsData$Community_FishANDHydro, DamsData$Aesthetics_FishANDHydro,
						  DamsData$Health_FishANDHydro, DamsData$Justice_FishANDHydro)
	KeepMaintain <- cbind(DamsData$FishBiomass_KeepMaintain, DamsData$RiverRec, DamsData$ResStorage, DamsData$Cost_KeepMaintain, DamsData$Damage,
	                      DamsData$Properties, DamsData$AvgAnnualGen, DamsData$EmissionsReduc,
	                      DamsData$Culture_KeepMaintain, DamsData$History_KeepMaintain, DamsData$Community_KeepMaintain, DamsData$Aesthetics_KeepMaintain,
	                      DamsData$Health_KeepMaintain, DamsData$Justice_KeepMaintain)

	#This abind creates our 3D matrix
	Ind_DamsDataMatrix <- abind(Remove, Improve_Fish, Improve_Hydro, FishANDHydro, KeepMaintain, along = 3, force.array=TRUE)

	#------------------------SUBSET BY DAM (row)--------------------------
	WestEnf_DataMatrix <- subset(Ind_DamsDataMatrix[1,,])
	WestEnf_DataMatrix <- data.frame(t(WestEnf_DataMatrix))
	Med_DataMatrix <- subset(Ind_DamsDataMatrix[2,,])
	Med_DataMatrix <- data.frame(t(Med_DataMatrix))
	EastMill_DataMatrix <- subset(Ind_DamsDataMatrix[3,,])
	EastMill_DataMatrix <- data.frame(t(EastMill_DataMatrix))	
	Dolby_DataMatrix <- subset(Ind_DamsDataMatrix[4,,])
	Dolby_DataMatrix <- data.frame(t(Dolby_DataMatrix))
	NorthTw_DataMatrix <- subset(Ind_DamsDataMatrix[5,,])
	NorthTw_DataMatrix <- data.frame(t(NorthTw_DataMatrix))
	Mill_DataMatrix <- subset(Ind_DamsDataMatrix[6,,])
	Mill_DataMatrix <- data.frame(t(Mill_DataMatrix))
	MillLake_DataMatrix <- subset(Ind_DamsDataMatrix[7,,])
	MillLake_DataMatrix <- data.frame(t(MillLake_DataMatrix))
	Rip_DataMatrix <- subset(Ind_DamsDataMatrix[8,,])
	Rip_DataMatrix <- data.frame(t(Rip_DataMatrix))

	AllDataMatrix <- array(data=NA, dim=c(5,14,8))
	AllDataMatrix <- provideDimnames(AllDataMatrix, sep="_", base=list("alternative", "criterion", "dam"))

	AllDataMatrix[,,1] <- simplify2array(WestEnf_DataMatrix)
	AllDataMatrix[,,2] <- simplify2array(Med_DataMatrix)
	AllDataMatrix[,,3] <- simplify2array(EastMill_DataMatrix)
	AllDataMatrix[,,4] <- simplify2array(Dolby_DataMatrix)
	AllDataMatrix[,,5] <- simplify2array(NorthTw_DataMatrix)
	AllDataMatrix[,,6] <- simplify2array(Mill_DataMatrix)
	AllDataMatrix[,,7] <- simplify2array(MillLake_DataMatrix)
	AllDataMatrix[,,8] <- simplify2array(Rip_DataMatrix)

	#--------NORMALIZATION FOR INDIVIDUAL DAMS RESULTS-------------------

	# iterate each dam & criteria for min,max
	MaxVectors <- array(data=NA, dim=c(matrix_cols, matrix_rows))
	MinVectors <- array(data=NA, dim=c(matrix_cols, matrix_rows))

	for (p in 1:matrix_rows){
		min_vector_list <- list("list", matrix_cols)
		max_vector_list <- list("list", matrix_cols)
		for ( k in 1:matrix_cols ){
			if (p==1){
				#message("dam ", p, " column ", k, " vector ",  AllDataMatrix[,k,p])
			}
			min_vector_list[[k]] <- min(AllDataMatrix[,k,p], na.rm=FALSE)
			max_vector_list[[k]] <- max(AllDataMatrix[,k,p], na.rm=FALSE)
		}
		MaxVectors[,p] <- unlist(max_vector_list)
		MinVectors[,p] <- unlist(min_vector_list)
	}
	#message("min vector for dam 1 ", MinVectors[,1])
	#message("max vector for dam 1 ", MaxVectors[,1])


	#----------------------------------------
	# SINGLE DAM WEIGHTING PROCEDURE
	#
	# Build Weighting Matrix for ind. dams
	# score will be min/max normalized values from 0-1
	# array of rows that use minimization (cost or damage-related)
	min_crit_columns <- c(4, 5, 6)
	#----------------------------------------

	# make normalized values of each value in 3d matrix, [alt, crit, dam]
	Ind_NormalizedMatrix <- array(data=NA, dim = c(matrix_levs_ind,matrix_cols,matrix_rows))
	# array of rows that use minimization (cost or damage-related)
	min_crit_columns <- c(4, 5, 6)

	# make normalized values of each value in matrix
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			for (p in 1:matrix_levs_ind){
				x <- AllDataMatrix[p,k,n]
				crit_min_x <- MinVectors[k,n]
				crit_max_x <- MaxVectors[k,n]
				# debug Ind_NormalizedMatrix
				#if (n == 1){ message("NormalMatrx dam ", n, " criteria ", k, " alt ", p, ' min ', crit_min_x, ' max ', crit_max_x) }

				Ind_NormalizedMatrix[p,k,n] <- tryCatch({

					if (k %in% min_crit_columns){
						# alternative method
						# maximize normalization
						(1-(x-crit_min_x) / (crit_max_x - crit_min_x))
					}else{
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

	is.nan.data.frame <- function(a){
	  do.call(cbind, lapply(a, is.nan))
	}
	Ind_NormalizedMatrix[is.nan.data.frame(Ind_NormalizedMatrix)] <- 0

	Ind_NormalizedMatrix[2:5,6,3] <- 1#This replaces properties NaN at East Millinocket
	Ind_NormalizedMatrix[1,1,2] <- 1 #This replaces fish habitat NaN at Medway
	Ind_NormalizedMatrix[5,3,1:2] <- 1#This replaces the reservoir storage NaN at West Enfield, Medway
	Ind_NormalizedMatrix[5,3,3] <- 1 #This replaces the reservoir storage NaN at East Millinocket
	Ind_NormalizedMatrix[1,2,7] <- 1 #This replaces the river rec NaN at Millinocket Lake
	
	#message('Ind_Normalized column ', Ind_NormalizedMatrix[1,,1])

	#----------------------------------------
	# SINGLE DAM WEIGHTING PROCEDURE
	#----------------------------------------
	Dam1Results <- round(((Ind_NormalizedMatrix[,,1]*(WestEnf_PrefMatrix/max_slider_value))*max_slider_value),1)
	Dam2Results <- round(((Ind_NormalizedMatrix[,,2]*(Med_PrefMatrix/max_slider_value))*max_slider_value), 1)
	Dam3Results <- round(((Ind_NormalizedMatrix[,,3]*(EastMill_PrefMatrix/max_slider_value))*max_slider_value), 1)
	Dam4Results <- round(((Ind_NormalizedMatrix[,,4]*(Dolby_PrefMatrix/max_slider_value))*max_slider_value), 1)
	Dam5Results <- round(((Ind_NormalizedMatrix[,,5]*(NorthTw_PrefMatrix/max_slider_value))*max_slider_value), 1)
	Dam6Results <- round(((Ind_NormalizedMatrix[,,6]*(Mill_PrefMatrix/max_slider_value))*max_slider_value), 1)
	Dam7Results <- round(((Ind_NormalizedMatrix[,,7]*(MillLake_PrefMatrix/max_slider_value))*max_slider_value), 1)
	Dam8Results <- round(((Ind_NormalizedMatrix[,,8]*(Rip_PrefMatrix/max_slider_value))*max_slider_value), 1)

	# Ind WeightedScoreMatrix, this binds each dam to 5 rows, one for each alternative
	# old method
	#Ind_WeightedScoreMatrix <- as.data.frame(rbind(Dam1Results, Dam2Results, Dam3Results, Dam4Results, Dam5Results, Dam6Results, Dam7Results, Dam8Results))
	#colnames(Ind_WeightedScoreMatrix) <- criteria_inputs
	# new method
	Ind_WeightedScoreMatrix <- array(data=NA, dim=c(5,14,8))
	Ind_WeightedScoreMatrix[,,1] <- simplify2array(Dam1Results)
	Ind_WeightedScoreMatrix[,,2] <- simplify2array(Dam2Results)
	Ind_WeightedScoreMatrix[,,3] <- simplify2array(Dam3Results)
	Ind_WeightedScoreMatrix[,,4] <- simplify2array(Dam4Results)
	Ind_WeightedScoreMatrix[,,5] <- simplify2array(Dam5Results)
	Ind_WeightedScoreMatrix[,,6] <- simplify2array(Dam6Results)
	Ind_WeightedScoreMatrix[,,7] <- simplify2array(Dam7Results)
	Ind_WeightedScoreMatrix[,,8] <- simplify2array(Dam8Results)

	# store all results in one data structure
	WeightedResults <- array( data=NA, dim=c(matrix_levs_ind, matrix_cols, matrix_rows))
	WeightedResults[,,1] <- as.matrix(Dam1Results)
	WeightedResults[,,2] <- as.matrix(Dam2Results)
	WeightedResults[,,3] <- as.matrix(Dam3Results)
	WeightedResults[,,4] <- as.matrix(Dam4Results)
	WeightedResults[,,5] <- as.matrix(Dam5Results)
	WeightedResults[,,6] <- as.matrix(Dam6Results)
	WeightedResults[,,6] <- as.matrix(Dam6Results)
	WeightedResults[,,7] <- as.matrix(Dam7Results)
	WeightedResults[,,8] <- as.matrix(Dam8Results)
	WeightedResults <- round(WeightedResults, 1)

	# sum scores
	ScoreSums <- array(data=NA, dim=c(matrix_rows, matrix_levs_ind))
	for (damid in 1:matrix_rows){
		for (j in 1:matrix_levs_ind){
			# debug
			#if (damid==1){ message( "Scoresum dam: ", damid, " j ", j, " to sum ", WeightedResults[j,,damid], " orig_to_sum ", Dam1Results[j, 1:matrix_cols]) }
			ScoreSums[damid, j] <- sum(as.numeric(WeightedResults[j,,damid]))
		}
	}

	# Ind ScoreSum
	Ind_scoresum <- round(as.data.frame(ScoreSums, rownames=dam_names), 0)
	colnames(Ind_scoresum)<- alternative_names

	#----------------------------------------
	# MULTI-DAM PROCEDURE FOR WEIGHTED SCENARIOS
	#----------------------------------------

	WeightedScoreMatrix <- (NormalizedMatrix*PrefMatrix)
	WeightedScoreMatrix <- round(WeightedScoreMatrix,3)

	#----------------------------------------
	# MULTI-DAM WEIGHTED SUM SCORES
	#
	# Weighted sum procedure:
	#  multiply all normalized scores by prefrence weights
	#  sum normalized, weighted criteria scores for each dam
	#  sum this across all dams for each scenario
	#-----------------------------------------

	# declare a weighted sum variable
	scoresum_total <- rep(0,dim(NormalizedMatrix)[3])

	# multiply crit scores by user preferences
	for (i in 1:dim(NormalizedMatrix)[3]){
		WSMMatrix <- NormalizedMatrix[,,i] * PrefMatrix[,,i]
		scoresum_total[i] <- sum(WSMMatrix) #this sums everything in each scenario after they are preferenced. Should be fine as order doesn't matter at this point.
	}

	#-----------------------------------------
	# Rank:
	#  may need to reshape the array produced by the weighted sum procedure
	#  sort the array by descending order, highest score comes first, and record the indices of the top ranked scenario
	#
	# Retrieve table, map of highest ranked scenario:
	#  use server url or whatever, searching for map name with the matching index number
	#  take the map image, table, and stick them in the webpage
	#----------------------------------------

	# order scenarios by rank: largest score first
	idxScen <- c(0:994)
	scoresum_index <- data.frame(cbind(scoresum_total, Decisions, idxScen))
	idxRank <- setorder(scoresum_index, -scoresum_total)
	#message("idxRank ", idxRank, " dim ", dim(idxRank))
	message("idxRank dim size ", dim(idxRank))

	Dam1Scen <- t(WeightedScoreMatrix[1,,])
	Dam2Scen <- t(WeightedScoreMatrix[2,,])
	Dam3Scen <- t(WeightedScoreMatrix[3,,])
	Dam4Scen <- t(WeightedScoreMatrix[4,,])
	Dam5Scen <- t(WeightedScoreMatrix[5,,])
	Dam6Scen <- t(WeightedScoreMatrix[6,,])
	Dam7Scen <- t(WeightedScoreMatrix[7,,])
	Dam8Scen <- t(WeightedScoreMatrix[8,,])

	multiDamResult <- array(data = NA, dim = c(995, 8, 14))
	multiDamResult <- array(abind(Dam1Scen, Dam2Scen, Dam3Scen, Dam4Scen, Dam5Scen, Dam6Scen, Dam7Scen, Dam8Scen))

	# use scenario idxRank[1] to find corresponding map name
	fname <- sprintf('maps/Penobscot_MO_14_%d.png', as.integer(idxRank[1,10]))

	# debug
	#message("Ind_WSM: ", Ind_WeightedScoreMatrix, " DIMS ", dim(Ind_WeightedScoreMatrix))
	message("Ind_scoresum: ", Ind_scoresum, " DIMS ", dim(Ind_scoresum))

	# warning adding things to list has side effects!
	results <- list(Ind_WeightedScoreMatrix, Ind_scoresum, scoresum_total, fname, AllDataMatrix, Ind_NormalizedMatrix, idxRank, WeightedScoreMatrix)

} # end of WSM
