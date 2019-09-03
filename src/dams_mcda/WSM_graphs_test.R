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
library(data.table)

DamsData <- read.csv('DamsData.csv') # this is the dataset for the individual dams, where rows = dams and cols = criteria
DamsData <- data.frame(DamsData)
source(file = 'f_nrge2.RData') #these are the NORMALIZED dams data from Sam's MOGA fitness function, where the'levels' data are for all 995 'scenarios' of 8 dams, 5 decision alts/dam
NormalizedMatrix <- as.array(f_nrge)
source(file='Decisions.RData') #this is 2 dimensions from f_nrge: rows = 995 'scenarios' with their decision alternative code for each dam, cols = 8 dams
Decisions <- as.array(Decisions)# need this for graphing
#codes:
#0 = remove dam
#1 = keep as is
#2 = improve hydropower
#3 = improve fish passage
#4 = improve both

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

# matrix setup
matrix_cols <- length(criteria_inputs) # 14 default (output size)
matrix_rows <- length(available_dams) # 8 default
matrix_levs_ind <- length(available_alternatives)# 5 default
matrix_levs <- length(1:995)


message("Decision Criteria", matrix_cols, "Dams", matrix_rows, "Decision Alternatives", matrix_levs_ind, "Scenarios", matrix_levs)

#----------------------------------------
# SINGLE DAM PROCEDURE FOR PREFERENCES
#
# Build Preference Matrix with blank levels, no normalization
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
#------------------------------------------------------
#  MULTI-DAM PROCEDURE FOR PREERENCES
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
				DamsData$Properties_Rem, DamsData$AvgAnnualGen_Rem, DamsData$EmissionsReduc_Rem,  
				DamsData$Culture_Remove, DamsData$History_Remove, DamsData$Community_Remove, DamsData$Aesthetics_Remove, 
				DamsData$Health_Remove, DamsData$Justice_Remove)

#This abind creates our 3D matrix
Ind_DamsDataMatrix <- abind(KeepMaintain, Improve_Hydro, Improve_Fish, FishANDHydro, Remove, along = 3, force.array=TRUE)

#------------------------SUBSET BY DAM (row)--------------------------
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


AllDataMatrix <- array(data=NA, dim=c(5,14,8))
AllDataMatrix <- provideDimnames(AllDataMatrix, sep="_", base=list("critieria", "alternative", "dam"))

AllDataMatrix[,,1] <- simplify2array(WestEnf_DataMatrix)
AllDataMatrix[,,2] <- simplify2array(Med_DataMatrix)
AllDataMatrix[,,3] <- simplify2array(Mill_DataMatrix)
AllDataMatrix[,,4] <- simplify2array(EastMill_DataMatrix)
AllDataMatrix[,,5] <- simplify2array(NorthTw_DataMatrix)
AllDataMatrix[,,6] <- simplify2array(Dolby_DataMatrix)
AllDataMatrix[,,7] <- simplify2array(MillLake_DataMatrix)
AllDataMatrix[,,8] <- simplify2array(Rip_DataMatrix)

#--------NORMALIZATION FOR INDIVIDUAL DAMS RESULTS-------------------
# iterate each criteria for min,max
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
message("min/max vectors done")
#message("simplified min vector1 ", MinVectors[,1])
#message("simplified max vector1 ", MaxVectors[,1])


#----------------------------------------
# SINGLE DAM WEIGHTING PROCEDURE

#Build Weighting Matrix for ind. dams
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
			if (n == 1){
				message("NormalMatrx dam ", n, " criteria ", k, " alt ", p, ' min ', crit_min_x, ' max ', crit_max_x)
			}

			Ind_NormalizedMatrix[p,k,n] <- tryCatch({

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
message("Normalized Matrix Done")

is.nan.data.frame <- function(a){
  do.call(cbind, lapply(a, is.nan))
}
Ind_NormalizedMatrix[is.nan.data.frame(Ind_NormalizedMatrix)] <- 0

#message('Normalized column ', Ind_NormalizedMatrix[1,,1])


########################################
### FOR COMPARING
########################################
WestEnf_NormalizedMatrix <- data.frame(array(data=NA, dim=c(5, 14)))

WestEnf_MaxVector <- list("list", matrix_cols)
for ( k in 1:matrix_cols ){
	WestEnf_MaxVector[[k]] <- max(WestEnf_DataMatrix[,k], na.rm=FALSE)}
WestEnf_MaxVector <- unlist(WestEnf_MaxVector)

WestEnf_MinVector <- list("list", matrix_cols)
for ( k in 1:matrix_cols ){
	WestEnf_MinVector[[k]] <- min(WestEnf_DataMatrix[,k], na.rm=FALSE)}
WestEnf_MinVector <- unlist(WestEnf_MinVector)

# make normalized values of each value in matrix
for (k in 1:matrix_cols){
  for (n in 1:matrix_levs_ind){
    x <- WestEnf_DataMatrix[n,k]
    crit_min_x <- WestEnf_MinVector[k]
    crit_max_x <- WestEnf_MaxVector[k]
	message("OLD NormalMatrx criteria ", k, " alt ", n, ' min ', crit_min_x, ' max ', crit_max_x)

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

is.nan.data.frame <- function(a){
  do.call(cbind, lapply(a, is.nan))
}
WestEnf_NormalizedMatrix[is.nan.data.frame(WestEnf_NormalizedMatrix)] <- 0

message('old method: ', WestEnf_NormalizedMatrix)
message('old method col: ', WestEnf_NormalizedMatrix[1])
message('new method: ', Ind_NormalizedMatrix[,,1])
message('new method col: ', Ind_NormalizedMatrix[,1,1])
#message('Normalized column ', NormalizedMatrix[1,,1])
########################################
### END FOR COMPARING
########################################

#----------------------------------------
# SINGLE DAM WEIGHTING PROCEDURE
#----------------------------------------
Dam1Results <- (Ind_NormalizedMatrix[,,1]*WestEnf_PrefMatrix)
Dam2Results <- (Ind_NormalizedMatrix[,,2]*Med_PrefMatrix)
Dam3Results <- (Ind_NormalizedMatrix[,,3]*Mill_PrefMatrix)
Dam4Results <- (Ind_NormalizedMatrix[,,4]*EastMill_PrefMatrix)
Dam5Results <- (Ind_NormalizedMatrix[,,5]*NorthTw_PrefMatrix)
Dam6Results <- (Ind_NormalizedMatrix[,,6]*Dolby_PrefMatrix)
Dam7Results <- (Ind_NormalizedMatrix[,,7]*MillLake_PrefMatrix)
Dam8Results <- (Ind_NormalizedMatrix[,,8]*Rip_PrefMatrix)

#------Dam 1--------------
Dam1Results <- round(Dam1Results, 3)#Dam 1 Weighted Matrix
scoresum1 <- list("list", matrix_levs_ind)

for (j in 1:matrix_levs_ind){
	scoresum1[[j]] <- sum(as.numeric(Dam1Results[j, 1:matrix_cols]))
}
scoresum1 <- unlist(scoresum1)

#------Dam 2--------------
Dam2Results <- round(Dam2Results, 3) #Dam 2 Weighted Matrix
scoresum2 <- list("list", matrix_levs_ind)

for (j in 1:matrix_levs_ind){
	scoresum2[[j]] <- sum(as.numeric(Dam2Results[j, 1:matrix_cols]))
}
scoresum2 <- unlist(scoresum2)

#------Dam 3--------------
Dam3Results <- round(Dam3Results, 3) #Dam 3 Weighted Matrix
scoresum3 <- list("list", matrix_levs_ind)

for (j in 1:matrix_levs_ind){
	scoresum3[[j]] <- sum(as.numeric(Dam3Results[j, 1:matrix_cols]))
}
scoresum3 <- unlist(scoresum3)

#------Dam 4--------------
Dam4Results <- round(Dam4Results, 3) #Dam 4 Weighted Matrix
scoresum4 <- list("list", matrix_levs_ind)

for (j in 1:matrix_levs_ind){
	scoresum4[[j]] <- sum(as.numeric(Dam4Results[j, 1:matrix_cols]))
}
scoresum4 <- unlist(scoresum4)

#------Dam 5--------------
Dam5Results <- round(Dam5Results, 3) #Dam 5 Weighted Matrix
scoresum5 <- list("list", matrix_levs_ind)

for (j in 1:matrix_levs_ind){
	scoresum5[[j]] <- sum(as.numeric(Dam5Results[j, 1:matrix_cols]))
}
scoresum5 <- unlist(scoresum5)

#------Dam 6--------------
Dam6Results  <- round(Dam6Results, 3) #Dam 6 Weighted Matrix
scoresum6 <- list("list", matrix_levs_ind)

for (j in 1:matrix_levs_ind){
	scoresum6[[j]] <- sum(as.numeric(Dam6Results[j, 1:matrix_cols]))
}
scoresum6 <- unlist(scoresum6)

#------Dam 7--------------
Dam7Results <- round(Dam7Results, 3) #Dam 7 Weighted Matrix
scoresum7 <- list("list", matrix_levs_ind)

for (j in 1:matrix_levs_ind){
	scoresum7[[j]] <- sum(as.numeric(Dam7Results[j, 1:matrix_cols]))
}
scoresum7 <- unlist(scoresum7)

#------Dam 8--------------
Dam8Results <- round(Dam8Results, 3) #Dam 8 Weighted Matrix
scoresum8 <- list("list", matrix_levs_ind)

for (j in 1:matrix_levs_ind){
	scoresum8[[j]] <- sum(as.numeric(Dam8Results[j, 1:matrix_cols]))
}
scoresum8 <- unlist(scoresum8)

Ind_scoresum <- as.data.frame(cbind(scoresum1, scoresum2, scoresum3, scoresum4, scoresum5, scoresum6, scoresum7, scoresum8))
colnames(Ind_scoresum)<- dam_names
Ind_WeightedScoreMatrix <- as.data.frame(rbind(Dam1Results, Dam2Results, Dam3Results, Dam4Results, Dam5Results, Dam6Results, Dam7Results, Dam8Results))
colnames(Ind_WeightedScoreMatrix)<- criteria_inputs

#----------------------------------------
# MULTI-DAM PROCEDURE FOR WEIGHTED SCENARIOS

#----------------------------------------

#WeightedScoreMatrix <- (Ind_NormalizedMatrix*PrefMatrix)
WeightedScoreMatrix <- round(WeightedScoreMatrix,3) 

#----------------------------------------
# MULTI-DAM WEIGHTED SUM SCORES

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

colnames(Decisions) <- dam_names
idxScen <- c(1:995)
scoresum_index <- data.frame(cbind(scoresum_total, Decisions, idxScen))
#-----------------------------------------
# Rank:
#  may need to reshape the array produced by the weighted sum procedure
#  sort the array by descending order, highest score comes first, and record the indices of the top ranked scenario
# Retrieve table, map of highest ranked scenario:
#  use server url or whatever, searching for map name with the matching index number
#  take the map image, table, and stick them in the webpage
#----------------------------------------

#order scenarios by rank: largest score first
idxRank <- setorder(scoresum_index,-scoresum_total)

Dam1Scen <- t(WeightedScoreMatrix[1,,])
Dam2Scen <- t(WeightedScoreMatrix[2,,])
Dam3Scen <- t(WeightedScoreMatrix[3,,])
Dam4Scen <- t(WeightedScoreMatrix[4,,])
Dam5Scen <- t(WeightedScoreMatrix[5,,])
Dam6Scen <- t(WeightedScoreMatrix[6,,])
Dam7Scen <- t(WeightedScoreMatrix[7,,])
Dam8Scen <- t(WeightedScoreMatrix[8,,])

multiDamResult <- array(data = NA, dim = c(995,8, 14))
multiDamResult <- array(abind(Dam1Scen, Dam2Scen, Dam3Scen, Dam4Scen, Dam5Scen, Dam6Scen, Dam7Scen, Dam8Scen))

#use scenario idxRank[1] to find corresponding map name
fname <- sprintf('maps/Penobscot_MO_14_%d.png',idxRank[[1]])
print(fname[1])

# warning adding things to list has side effects!
WSMResults <- list(Ind_WeightedScoreMatrix, Ind_scoresum, scoresum_total, fname)

# end of WSM
message("wsm_graph_test loaded")


TableMatrix <- results[1]

Ind_MCDA_score <- results[2]

MCDA_score <- results[3]

map_name <- results[4]

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
							   "Hydro And Fish",
							   "Remove Dam"
							   ))

#-------------------------------------------------------
## bars for ALL Dam MCDA score results
Score_compare <- as.matrix(Ind_scoresum)
colnames(Score_compare) <- dam_names
rownames(Score_compare) <- alternative_names

# Graph ALL DAM alternative scores with adjacent bars grouped by dam
WSMPlota <- barplot((Score_compare), ylim= c(0,1.0), main="Dam Decision Recommendation Comparison", ylab= "MCDA Score",
                    beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
       bty="n", fill=rainbow(5));
#-----------------------------------------------
# stacked bars for ALL dam MCDA scores (broken down by criteria)
CritAlt <- as.matrix(Ind_WeightedScoreMatrix)
colnames(CritAlt) <- criteria_inputs
rownames(CritAlt) <- alternative_names


# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
WSMPlotb <- barplot(t(CritAlt), ylim= c(0,1.0), main="Dam Decision Alternative Comparison", ylab="MCDA Score", 
                    col=rainbow(14), cex.axis=0.8, las=1, cex=0.7) 

# Place the legend at the top-left corner with no frame  
# using rainbow colors

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#--------------------------------------------------------
## stacked bars data table for West Enfield Dam results
Score1 <- as.matrix(scoresum1)
rownames(Score1) <- alternative_names

# Graph West Enfield alternative scores
WSMPlot1a <- barplot((scoresum1), ylim= c(0,1.0), main="West Enfield Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
	   bty="n", fill=rainbow(5));

#-------------------------------------------------------
# Graph alternatives (broken down by criteria) for West Enfield
CritAlt1 <- as.matrix(Dam1Results)
colnames(CritAlt1) <- criteria_inputs
rownames(CritAlt1) <- alternative_names


# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
WSMPlot1b <- barplot(t(CritAlt1), ylim= c(0,1.0), main="West Enfield Dam", ylab="MCDA Score", col=rainbow(14),
                     cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7) 

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#--------------------------------------------------------
## stacked bars data table for Medway Dam results
Score2 <- as.matrix(scoresum2)
rownames(Score2) <- alternative_names

# Graph  alternative scores
WSMPlot2a <- barplot((scoresum2), ylim= c(0,1.0), main="Medway Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for Medway dam
CritAlt2 <- as.matrix(Dam2Results)
colnames(CritAlt2) <- criteria_inputs
rownames(CritAlt2) <- alternative_names


# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
WSMPlot2b <- barplot(t(CritAlt2), ylim= c(0,1.0), main="Medway Dam", ylab="MCDA Score", col=rainbow(14),
                     cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7) 

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for Millinocket/Quakish Dam results
Score3 <- as.matrix(scoresum3)
rownames(Score3) <- alternative_names

# Graph alternative scores
WSMPlot3a <- barplot((scoresum3), ylim= c(0,1.0), main="Millinocket Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt3 <- as.matrix(Dam3Results)
colnames(CritAlt3) <- criteria_inputs
rownames(CritAlt3) <- alternative_names


# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
WSMPlot3b <- barplot(t(CritAlt3), ylim= c(0,1.0), main="Millnocket Dam", ylab="MCDA Score", col=rainbow(14),
                     cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7) 

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for East Millinocket Dam results
Score4 <- as.matrix(scoresum4)
rownames(Score4) <- alternative_names

# Graph alternative scores
WSMPlot4a <- barplot((scoresum4), ylim= c(0,1.0), main="East Millinocket Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt4 <- as.matrix(Dam4Results)
colnames(CritAlt4) <- criteria_inputs
rownames(CritAlt4) <- alternative_names


# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
WSMPlot4b <- barplot(t(CritAlt4), ylim= c(0,1.0), main="East Millinocket Dam", ylab="MCDA Score", col=rainbow(14),
                     cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7) 

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for North Twin Dam results
Score5 <- as.matrix(scoresum5)
rownames(Score5) <- alternative_names

# Graph alternative scores
WSMPlot5a <- barplot((scoresum5), ylim= c(0,1.0), main="North Twin Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt5 <- as.matrix(Dam5Results)
colnames(CritAlt5) <- criteria_inputs
rownames(CritAlt5) <- alternative_names


# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
WSMPlot5b <- barplot(t(CritAlt5), ylim= c(0,1.0), main="North Twin Dam", ylab="MCDA Score", col=rainbow(14),
                     cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7) 

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for Dolby Dam results
Score6 <- as.matrix(scoresum6)
rownames(Score6) <- alternative_names

# Graph alternative scores
WSMPlot6a <- barplot((scoresum6), ylim= c(0,1.0), main="Dolby Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt6 <- as.matrix(Dam6Results)
colnames(CritAlt6) <- criteria_inputs
rownames(CritAlt6) <- alternative_names


# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
WSMPlot6b <- barplot(t(CritAlt6), ylim= c(0,1.0), main="Dolby Dam", ylab="MCDA Score", col=rainbow(14),
                     cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7) 

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for Millinocket Lake Dam results
Score7 <- as.matrix(scoresum7)
rownames(Score7) <- alternative_names

# Graph alternative scores
WSMPlot7a <- barplot((scoresum7), ylim= c(0,1.0), main="Millinocket Lake Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt7 <- as.matrix(Dam7Results)
colnames(CritAlt7) <- criteria_inputs
rownames(CritAlt7) <- alternative_names


# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
WSMPlot7b <- barplot(t(CritAlt7), ylim= c(0,1.0), main="Millinocket Lake Dam", ylab="MCDA Score", col=rainbow(14),
                     cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7) 

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14));
#-------------------------------------------------------
## stacked bars data table for Ripogenus Dam results
Score8 <- as.matrix(scoresum8)
rownames(Score8) <- alternative_names

# Graph alternative scores
WSMPlot8a <- barplot((scoresum8), ylim= c(0,1.0), main="Ripogenus Dam Recommendation", ylab= "Decision Alternative Score",
					 names.arg= alternative_names, beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("KeepMaintain","ImproveHydro","ImproveFish","Improve FishANDHydro","Remove"), cex=0.6, 
	   bty="n", fill=rainbow(5));
#--------------------------------------------------------
# Graph alternatives (broken down by criteria) for individual dams
CritAlt8 <- as.matrix(Dam8Results)
colnames(CritAlt8) <- criteria_inputs
rownames(CritAlt8) <- alternative_names


# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
WSMPlot8b <- barplot(t(CritAlt8), ylim= c(0,1.0), main="Ripogenus Dam", ylab="MCDA Score", col=rainbow(14),
					 cex.axis=0.8, las=1, names.arg= alternative_names, cex=0.7) 

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14))

