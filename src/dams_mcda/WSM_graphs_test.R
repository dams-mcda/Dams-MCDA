# calls WSM, for testing WSM interactively outside of server

# set working directory only if it exists
has_wd <- tryCatch({
	workdir <- "~/Beatrice2/R_ELF/R_NEST/MCDA_App_Shiny/MCDA_06262019/src/dams_mcda"
	setwd(workdir)
	message("set Working Directory: ", workdir)
}, error=function(e){
	message("Working Directory does NOT exist.")
})

source("WSM.R")

DamsData <- read.csv('DamsData.csv') # this is the dataset for the individual dams, where rows = dams and cols = criteria
DamsData <- data.frame(DamsData)
source(file = 'f_nrge_workshop.RData') #these are the NORMALIZED dams data from Sam's MOGA fitness function, where the'levels' data are for all 1885 'scenarios' of 8 dams, 5 decision alts/dam
NormalizedMatrix <- as.array(f_nrge_workshop)
source(file='Decisions_workshop.RData') #this is 2 dimensions from f_nrge: rows = 1885 'scenarios' with their decision alternative code for each dam, cols = 8 dams
Decisions <- as.array(Decisions_workshop)# need this for graphing
#codes:
#0 = remove dam
#1 = keep as is
#2 = improve hydropower
#3 = improve fish passage
#4 = improve both

TestData <- read.csv('FishPrefs_forLiveSite.csv', row.names = "DAM")
RawCriteriaMatrix <- data.frame(TestData)#test preference data for 8 dams, 14 criteria each

# criteria input identifiers
criteria_inputs <- c(
					 "FishBiomass",
					 "RiverRec",
					 "Reservoir",
					 "ProjectCost",
					 "BreachDamage",
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

#dam display names
dam_names <- as.list(c('WestEnfield','Medway','E.Millinocket', 'Dolby','North Twin','Millinocket','Millinocket Lake','Ripogenus'))

# alternative display names (for labeling tables and graphs)
alternative_names <- as.list(c(
  "Remove Dam",
  "Improve Fish Passage",
  "Improve Hydro",
  "Hydro And Fish",
  "Keep and Maintain Dam"
))

# list of dams
available_dams <- seq(1:8)
# list of alternatives
available_alternatives <- seq(1:5)

# matrix setup
matrix_cols <- length(criteria_inputs) # 14 default (output size)
matrix_rows <- length(available_dams) # 8 default
matrix_levs_ind <- length(available_alternatives)# 5 default


# MOGA Scenarios, how many are there?
num_scenarios <- 1885


message("Decision Criteria", matrix_cols, "Dams", matrix_rows, "Decision Alternatives", matrix_levs_ind, "Scenarios", num_scenarios)

#----------------------------------------
# SINGLE DAM PROCEDURE FOR PREFERENCES
#
# Build Preference Matrix with blank levels, no normalization
# score will be raw values from 0-1 based on user input
#----------------------------------------
#Ind_* prefix indicates that we are dealing with individual or single dams, where the 5 decision alternatives are taken into account
#locally and not as a part of the larger set of 8 dams.
Ind_PrefMatrix <- array(data = NA, dim=c(8,14)) #This is a 3-D blank matrix 

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

Ind_PrefMatrix <- array(rep(Ind_PrefMatrix,5), dim=c(dim(Ind_PrefMatrix), 5))

message("fill Ind Pref Matrix")


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
PrefMatrix <- array(data = NA, dim=c(8,14)) #This is a 3-D blank matrix 

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

PrefMatrix <- array(data=rep(PrefMatrix, num_scenarios), dim=c(dim(PrefMatrix), num_scenarios)) 


message("fill multi-dam Pref Matrix")

#----------------------------------------
# SINGLE DAM DATA NORMALIATION PROCEDURE

#Data Normalization using Min / Max Vectors
# Retrieve criteria values for each dam (referred to as Ind_DamsDataMartrix), for each MCDA scenario (from server?) a 3D matrix [dams,criteria,alternatives] 

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

#Build Weighting Matrix for ind. dams
# score will be min/max normalized values from 0-1
# array of rows that use minimization (cost, damage-related, properties impacted)
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

Ind_NormalizedMatrix[2:5,6,3] <- c(1,1,1,1)#This replaces properties NaN at East Millinocket
Ind_NormalizedMatrix[1,5,3] <- 1 #This replaces damage 0 value for Remove at East Millinocket
Ind_NormalizedMatrix[1,1,2] <- 1 #This  fish habitat NaN at Medway
Ind_NormalizedMatrix[5,3,1:3] <- 1#This replaces the reservoir storage NaN at West Enfield, Medway, East Millinocket
Ind_NormalizedMatrix[1,2,7] <- 1 #This replaces the river rec NaN at Millinocket Lake

#message('Ind_Normalized column ', Ind_NormalizedMatrix[1,,1])

#----------------------------------------
# SINGLE DAM WEIGHTING PROCEDURE
#----------------------------------------
Dam1Results <- (Ind_NormalizedMatrix[,,1]*(WestEnf_PrefMatrix))
Dam2Results <- (Ind_NormalizedMatrix[,,2]*(Med_PrefMatrix))
Dam3Results <- (Ind_NormalizedMatrix[,,3]*(EastMill_PrefMatrix))
Dam4Results <- (Ind_NormalizedMatrix[,,4]*(Dolby_PrefMatrix))
Dam5Results <- (Ind_NormalizedMatrix[,,5]*(NorthTw_PrefMatrix))
Dam6Results <- (Ind_NormalizedMatrix[,,6]*(Mill_PrefMatrix))
Dam7Results <- (Ind_NormalizedMatrix[,,7]*(MillLake_PrefMatrix))
Dam8Results <- (Ind_NormalizedMatrix[,,8]*(Rip_PrefMatrix))

# store all results in one data structure
WeightedResults <- array( data=NA, dim=c(matrix_levs_ind,matrix_cols,matrix_rows))
WeightedResults[,,1] <- as.matrix(Dam1Results)
WeightedResults[,,2] <- as.matrix(Dam2Results)
WeightedResults[,,3] <- as.matrix(Dam3Results)
WeightedResults[,,4] <- as.matrix(Dam4Results)
WeightedResults[,,5] <- as.matrix(Dam5Results)
WeightedResults[,,6] <- as.matrix(Dam6Results)
WeightedResults[,,6] <- as.matrix(Dam6Results)
WeightedResults[,,7] <- as.matrix(Dam7Results)
WeightedResults[,,8] <- as.matrix(Dam8Results)
WeightedResults <- round(WeightedResults, 0)


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
Ind_scoresum <- round(as.data.frame(ScoreSums, rownames = dam_names),0)
message("Ind_scoresum ", Ind_scoresum)
rownames(Ind_scoresum) <- dam_names
colnames(Ind_scoresum) <- alternative_names

# Ind WeightedScoreMatrix
Ind_WeightedScoreMatrix <- as.data.frame(rbind(Dam1Results, Dam2Results, Dam3Results, Dam4Results, Dam5Results, Dam6Results, Dam7Results, Dam8Results))
colnames(Ind_WeightedScoreMatrix)<- criteria_inputs

#----------------------------------------
# MULTI-DAM PROCEDURE FOR WEIGHTED SCENARIOS
##!!!!!!!!!!NOTE: this Normalized matrix hard-code for NaNs is no longer in WSM.R!!!!!!!!!!
#----------------------------------------
# reorganize dams in NormalizedMatrix
# Normalized is Dolby Milli Quak NorthTwin Ripo EastMilli Medway WestEnf
# target is WestEnf Medway EMill Dolby NorthTwin Quak Milli Ripo 
# Normalized is 4 7 6 5 8 3 2 1
# target is     1 2 3 4 5 6 7 8
NormalizedMatrix <- as.array(f_nrge)

TestMatrix <- NormalizedMatrix
TestMatrix[1,,] <- NormalizedMatrix[8,,]
TestMatrix[2,,] <- NormalizedMatrix[7,,]
TestMatrix[3,,] <- NormalizedMatrix[6,,]
TestMatrix[4,,] <- NormalizedMatrix[1,,]
TestMatrix[5,,] <- NormalizedMatrix[4,,]
TestMatrix[6,,] <- NormalizedMatrix[3,,]
TestMatrix[7,,] <- NormalizedMatrix[2,,]
TestMatrix[8,,] <- NormalizedMatrix[5,,]
NormMatrix <- TestMatrix
message("size of NormalizedMatrix", dim(TestMatrix))

WeightedScoreMatrix <- (NormMatrix*PrefMatrix)
WeightedScoreMatrix <- round(WeightedScoreMatrix,3) 

#----------------------------------------
# MULTI-DAM WEIGHTED SUM SCORES

# Weighted sum procedure:
#  multiply all normalized scores by prefrence weights
#  sum normalized, weighted criteria scores for each dam
#  sum this across all dams for each scenario
#-----------------------------------------

#declare a weighted sum variable
scoresum_total <- rep(0,dim(NormMatrix)[3])

#multiply crit scores by user preferences
for (i in 1:dim(NormMatrix)[3]){
	WSMMatrix <- NormMatrix[,,i] * PrefMatrix[,,i]
	scoresum_total[i] <- sum(WSMMatrix) #this sums everything in each scenario after they are preferenced. Should be fine as order doesn't matter at this point.
}

colnames(Decisions) <- dam_names #need to check with Sam about the order of dams here
idxScen <- c(0:994)
scoresum_index <- data.frame(cbind(idxScen, scoresum_total, Decisions))
#-----------------------------------------
# Rank:
#  may need to reshape the array produced by the weighted sum procedure
#  sort the array by descending order, highest score comes first, and record the indices of the top ranked scenario
# Retrieve table, map of highest ranked scenario:
#  use server url or whatever, searching for map name with the matching index number
#  take the map image, table, and stick them in the webpage
#----------------------------------------

#order scenarios by rank: largest score first
idxRank <- data.frame(setorder(scoresum_index,-scoresum_total))

#use scenario idxRank[1] to find corresponding map name
fname <- sprintf('maps/Penobscot_MO_14_%d.png',idxRank[1,1])


# call WSM
results <- WSM(RawCriteriaMatrix, NormMatrix, DamsData, Decisions)
# results are list(Ind_WeightedScoreMatrix, Ind_scoresum, scoresum_total, fname)
#message("WSM Results", results)

TableMatrix <- results[1]

Ind_MCDA_score <- results[2]

MCDA_score <- results[3]

map_name <- results[4]

#----------------------------------------
# Final Outputs: TABLES
#----------------------------------------

#dam display names
dam_names <- as.list(c('WestEnfield','Medway','E.Millinocket','Dolby','North Twin','Millinocket','Millinocket Lake','Ripogenus'))
# alternative display names (for labeling tables and graphs)
alternative_names <- as.list(c("Remove Dam", "Improve Fish Passage", "Improve Hydro", "Improve Hydro AND Fish", "Keep and Maintain Dam"))

# West Enfield/Dam 1 output table(s)

Dam1RawTable <- setDT(WestEnf_DataMatrix)
row.names(Dam1RawTable) <- alternative_names
colnames(Dam1RawTable) <- criteria_inputs

Dam1NormTable <- setDT(data.frame(round(Ind_NormalizedMatrix[,,1], 1)*100))
row.names(Dam1NormTable) <- alternative_names
colnames(Dam1NormTable) <- criteria_inputs

Dam1ScoreTable <- setDT(round(Dam1Results, 1)*100)
row.names(Dam1ScoreTable) <- alternative_names
colnames(Dam1ScoreTable) <- criteria_inputs

# Medway/Dam 2 output table(s)

Dam2RawTable <- setDT(Med_DataMatrix)
rownames(Dam2RawTable) <- alternative_names
colnames(Dam2RawTable) <- criteria_inputs

Dam2NormTable <- setDT(data.frame(Ind_NormalizedMatrix[,,2]))
row.names(Dam2NormTable) <- alternative_names
colnames(Dam2NormTable) <- criteria_inputs

Dam2ScoreTable <- setDT(Dam2Results)
row.names(Dam2ScoreTable) <- alternative_names
colnames(Dam2ScoreTable) <- criteria_inputs

# Millinocket/Dam 3 output table(s)

Dam3RawTable <- setDT(EastMill_DataMatrix)
rownames(Dam3RawTable) <- alternative_names
colnames(Dam3RawTable) <- criteria_inputs

Dam3NormTable <- setDT(data.frame(Ind_NormalizedMatrix[,,3]))
row.names(Dam3NormTable) <- alternative_names
colnames(Dam3NormTable) <- criteria_inputs

Dam3ScoreTable <- setDT(Dam3Results)
row.names(Dam3ScoreTable) <- alternative_names
colnames(Dam3ScoreTable) <- criteria_inputs

# East Millinocket/Dam 4 output table(s)

Dam4RawTable <- setDT(Dolby_DataMatrix)
rownames(Dam4RawTable) <- alternative_names
colnames(Dam4RawTable) <- criteria_inputs

Dam4NormTable <- setDT(data.frame(Ind_NormalizedMatrix[,,4]))
row.names(Dam4NormTable) <- alternative_names
colnames(Dam4NormTable) <- criteria_inputs

Dam4ScoreTable <- setDT(Dam4Results)
row.names(Dam4ScoreTable) <- alternative_names
colnames(Dam4ScoreTable) <- criteria_inputs

# North Twin/Dam 5 output table(s)

Dam5RawTable <- setDT(NorthTw_DataMatrix)
rownames(Dam5RawTable) <- alternative_names
colnames(Dam5RawTable) <- criteria_inputs

Dam5NormTable <- setDT(data.frame(Ind_NormalizedMatrix[,,5]))
row.names(Dam5NormTable) <- alternative_names
colnames(Dam5NormTable) <- criteria_inputs

Dam5ScoreTable <- setDT(Dam5Results)
row.names(Dam5ScoreTable) <- alternative_names
colnames(Dam5ScoreTable) <- criteria_inputs

# Dolby/Dam 6 output table(s)

Dam6RawTable <- setDT(Mill_DataMatrix)
rownames(Dam6RawTable) <- alternative_names
colnames(Dam6RawTable) <- criteria_inputs

Dam6NormTable <- setDT(data.frame(Ind_NormalizedMatrix[,,6]))
row.names(Dam6NormTable) <- alternative_names
colnames(Dam6NormTable) <- criteria_inputs

Dam6ScoreTable <- setDT(Dam6Results)
row.names(Dam6ScoreTable) <- alternative_names
colnames(Dam6ScoreTable) <- criteria_inputs

# Millinocket Lake /Dam 7 output table(s)

Dam7RawTable <- setDT(MillLake_DataMatrix)
rownames(Dam7RawTable) <- alternative_names
colnames(Dam7RawTable) <- criteria_inputs

Dam7NormTable <- setDT(data.frame(Ind_NormalizedMatrix[,,7]))
row.names(Dam7NormTable) <- alternative_names
colnames(Dam7NormTable) <- criteria_inputs

Dam7ScoreTable <- setDT(Dam7Results)
row.names(Dam7ScoreTable) <- alternative_names
colnames(Dam4ScoreTable) <- criteria_inputs

# Ripogenus/Dam 8 output table(s)

Dam8RawTable <- setDT(Rip_DataMatrix)
rownames(Dam8RawTable) <- alternative_names
colnames(Dam8RawTable) <- criteria_inputs

Dam8NormTable <- setDT(data.frame(Ind_NormalizedMatrix[,,8]))
row.names(Dam8NormTable) <- alternative_names
colnames(Dam8NormTable) <- criteria_inputs

Dam8ScoreTable <- setDT(Dam8Results)
row.names(Dam8ScoreTable) <- alternative_names
colnames(Dam8ScoreTable) <- criteria_inputs

#-------------------------------------------------------

## bars for ALL Dam MCDA score results
Score_compare <- as.matrix(Ind_scoresum)
colnames(Score_compare) <- alternative_names
rownames(Score_compare) <- dam_names

# Graph ALL DAM alternative scores with adjacent bars grouped by dam
WSMPlota <- barplot(t(Score_compare), ylim= c(0,100), main="Dam Decision Recommendation Comparison", ylab= "MCDA Score",
                    beside=TRUE, col=rainbow(5), cex.axis=0.8, names.arg= dam_names, cex=0.7)

# Place the legend at the top-left corner with no frame
# using rainbow colors
legend("topleft", c("RemoveDam", "ImproveFish","ImproveHydro","Improve FishANDHydro","KeepMaintain"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#-----------------------------------------------
# stacked bars for ALL dam MCDA scores (broken down by criteria)
CritAlt <- as.matrix(Ind_WeightedScoreMatrix)
colnames(CritAlt) <- criteria_inputs


# put 10% of the space between each bar, and make labels
# smaller with horizontal y-axis labels
WSMPlotb <- barplot(t(CritAlt), ylim= c(0,100), main="Dam Decision Alternative Comparison", ylab="MCDA Score", 
                    col=rainbow(14), cex.axis=0.8, las=1, names.arg= c(alternative_names, alternative_names, alternative_names, alternative_names,
                                                                       alternative_names, alternative_names, alternative_names, alternative_names), cex=0.7) 

# Place the legend at the top-left corner with no frame
# using rainbow colors

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#--------------------------------------------------------
## stacked bars data table for West Enfield Dam results
Score1 <- as.matrix(Ind_scoresum[1,])
rownames(Score1) <- alternative_names

# Graph West Enfield alternative scores
WSMPlot1a <- barplot((Score1), ylim= c(0,100), main="West Enfield Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame
# using rainbow colors
legend("topleft", c("RemoveDam", "ImproveFish","ImproveHydro","Improve FishANDHydro","KeepMaintain"), cex=0.6, 
	   bty="n", fill=rainbow(5));

#-------------------------------------------------------
# Graph alternatives (broken down by criteria) for West Enfield
CritAlt1 <- as.matrix(Dam1Results)
colnames(CritAlt1) <- criteria_inputs
rownames(CritAlt1) <- alternative_names


# put 10% of the space between each bar, and make labels
# smaller with horizontal y-axis labels
WSMPlot1b <- barplot(t(CritAlt1), ylim= c(0,100), main="West Enfield Dam", ylab="MCDA Score", col=rainbow(14),
					 cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7)

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#--------------------------------------------------------
## stacked bars data table for Medway Dam results
Score2 <- as.matrix(Ind_scoresum[2,])
rownames(Score2) <- alternative_names

# Graph  alternative scores
WSMPlot2a <- barplot((Score2), ylim= c(0,100), main="Medway Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame
# using rainbow colors
legend("topleft", c("RemoveDam", "ImproveFish","ImproveHydro","Improve FishANDHydro","KeepMaintain"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for Medway dam
CritAlt2 <- as.matrix(Dam2Results)
colnames(CritAlt2) <- criteria_inputs
rownames(CritAlt2) <- alternative_names


# put 10% of the space between each bar, and make labels
# smaller with horizontal y-axis labels
WSMPlot2b <- barplot(t(CritAlt2), ylim= c(0,100), main="Medway Dam", ylab="MCDA Score", col=rainbow(14),
					 cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7)

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for Millinocket/Quakish Dam results
Score3 <- as.matrix(Ind_scoresum[3,])
rownames(Score3) <- alternative_names

# Graph alternative scores
WSMPlot3a <- barplot((Score3), ylim= c(0,100), main="East Millinocket Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame
# using rainbow colors
legend("topleft", c("RemoveDam", "ImproveFish","ImproveHydro","Improve FishANDHydro","KeepMaintain"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt3 <- as.matrix(Dam3Results)
colnames(CritAlt3) <- criteria_inputs
rownames(CritAlt3) <- alternative_names


# put 10% of the space between each bar, and make labels
# smaller with horizontal y-axis labels
WSMPlot3b <- barplot(t(CritAlt3), ylim= c(0,100), main="East Millnocket Dam", ylab="MCDA Score", col=rainbow(14),
					 cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7)

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for East Millinocket Dam results
Score4 <- as.matrix(Ind_scoresum[4,])
rownames(Score4) <- alternative_names

# Graph alternative scores
WSMPlot4a <- barplot((Score4), ylim= c(0,100), main="Dolby Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame
# using rainbow colors
legend("topleft", c("RemoveDam", "ImproveFish","ImproveHydro","Improve FishANDHydro","KeepMaintain"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt4 <- as.matrix(Dam4Results)
colnames(CritAlt4) <- criteria_inputs
rownames(CritAlt4) <- alternative_names


# put 10% of the space between each bar, and make labels
# smaller with horizontal y-axis labels
WSMPlot4b <- barplot(t(CritAlt4), ylim= c(0,100), main="Dolby Dam", ylab="MCDA Score", col=rainbow(14),
					 cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7)

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for North Twin Dam results
Score5 <- as.matrix(Ind_scoresum[5,])
rownames(Score5) <- alternative_names

# Graph alternative scores
WSMPlot5a <- barplot((Score5), ylim= c(0,100), main="North Twin Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame
# using rainbow colors
legend("topleft", c("RemoveDam", "ImproveFish","ImproveHydro","Improve FishANDHydro","KeepMaintain"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt5 <- as.matrix(Dam5Results)
colnames(CritAlt5) <- criteria_inputs
rownames(CritAlt5) <- alternative_names


# put 10% of the space between each bar, and make labels
# smaller with horizontal y-axis labels
WSMPlot5b <- barplot(t(CritAlt5), ylim= c(0,100), main="North Twin Dam", ylab="MCDA Score", col=rainbow(14),
					 cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7)

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for Dolby Dam results
Score6 <- as.matrix(Ind_scoresum[6,])
rownames(Score6) <- alternative_names

# Graph alternative scores
WSMPlot6a <- barplot((Score6), ylim= c(0,100), main="Millinocket/Quakish Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame
# using rainbow colors
legend("topleft", c("RemoveDam", "ImproveFish","ImproveHydro","Improve FishANDHydro","KeepMaintain"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt6 <- as.matrix(Dam6Results)
colnames(CritAlt6) <- criteria_inputs
rownames(CritAlt6) <- alternative_names


# put 10% of the space between each bar, and make labels
# smaller with horizontal y-axis labels
WSMPlot6b <- barplot(t(CritAlt6), ylim= c(0,100), main="Millinocket/Quakish Dam", ylab="MCDA Score", col=rainbow(14),
					 cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7)

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for Millinocket Lake Dam results
Score7 <- as.matrix(Ind_scoresum[7,])
rownames(Score7) <- alternative_names

# Graph alternative scores
WSMPlot7a <- barplot((Score7), ylim= c(0,100), main="Millinocket Lake Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame
# using rainbow colors
legend("topleft", c("RemoveDam", "ImproveFish","ImproveHydro","Improve FishANDHydro","KeepMaintain"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt7 <- as.matrix(Dam7Results)
colnames(CritAlt7) <- criteria_inputs
rownames(CritAlt7) <- alternative_names


# put 10% of the space between each bar, and make labels
# smaller with horizontal y-axis labels
WSMPlot7b <- barplot(t(CritAlt7), ylim= c(0,100), main="Millinocket Lake Dam", ylab="MCDA Score", col=rainbow(14),
					 cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7)

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for Ripogenus Dam results
Score8 <- as.matrix(Ind_scoresum[8,])
rownames(Score8) <- alternative_names

# Graph alternative scores
WSMPlot8a <- barplot((Score8), ylim= c(0,100), main="Ripogenus Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame
# using rainbow colors
legend("topleft", c("RemoveDam", "ImproveFish","ImproveHydro","Improve FishANDHydro","KeepMaintain"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt8 <- as.matrix(Dam8Results)
colnames(CritAlt8) <- criteria_inputs
rownames(CritAlt8) <- alternative_names


# put 10% of the space between each bar, and make labels
# smaller with horizontal y-axis labels
WSMPlot8b <- barplot(t(CritAlt8), ylim= c(0,100), main="Ripogenus Dam", ylab="MCDA Score", col=rainbow(14),
					 cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7)

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14))
#--------------------------------------------------------
#Graph for Top Scenario for ALL dams across ALL scenarios

Dam1Scen <- t(WeightedScoreMatrix[1,,])
Dam2Scen <- t(WeightedScoreMatrix[2,,])
Dam3Scen <- t(WeightedScoreMatrix[3,,])
Dam4Scen <- t(WeightedScoreMatrix[4,,])
Dam5Scen <- t(WeightedScoreMatrix[5,,])
Dam6Scen <- t(WeightedScoreMatrix[6,,])
Dam7Scen <- t(WeightedScoreMatrix[7,,])
Dam8Scen <- t(WeightedScoreMatrix[8,,])

MCDASum <- data.frame(cbind(Dam1Scen, Dam2Scen, Dam3Scen, Dam4Scen, Dam5Scen, Dam6Scen, Dam7Scen, Dam8Scen, idxScen, scoresum_total))
MCDASum <- data.frame(setorder(MCDASum,-scoresum_total))
MCDASum_forGraph <- t(MCDASum[1,])

DamsTopScenGraph <- data.frame(cbind(MCDASum_forGraph[1:14], MCDASum_forGraph[15:28], MCDASum_forGraph[29:42],
                                     MCDASum_forGraph[43:56], MCDASum_forGraph[57:70], MCDASum_forGraph[71:84], 
                                     MCDASum_forGraph[85:98], MCDASum_forGraph[99:112]))
DamsTopScenGraph <- as.matrix(DamsTopScenGraph)
colnames(DamsTopScenGraph) <- dam_names
rownames(DamsTopScenGraph) <- criteria_inputs

# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
WSMPlot9 <- barplot((DamsTopScenGraph), ylim= c(0,100), main="Top Dam Scenario", ylab="MCDA Score", col=rainbow(14),
                     cex.axis=0.8, las=1, names.arg= dam_names, cex=0.7) 

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14))
