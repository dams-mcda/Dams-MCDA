#base_dir <- "~/Beatrice2/R_ELF/R_NEST/MCDA_App_Shiny/"
#working_dir <- paste(base_dir, "MCDA_11132018/WSM_Tool", sep="")
#setwd(working_dir)

base_dir <- "/srv/shiny-server/dams_mcda/"
setwd(base_dir)

library(shiny)
library(ggplot2)
library(dplyr)

####SAVE THIS EXAMPLE CODE TO TROUBLESHOOT PLOT DESIGN LATER########

#RawCriteriaMatrix            <- data.frame(matrix(NA), nrow=6, ncol=7)

#Fish <- c(3, 3, 3, 3, 3, 1)
#Rec <-c(3, 3, 3, 2, 3, 3)
#Res <- c(4, 3, 5, 3, 1, 3)
#Cost <- c(3, 3, 3, 5, 3, 3)
#Safe <- c(3, 3, 5, 3, 3, 3)
#Houses <- c(3, 3, 2, 2, 3, 3)
#Power <- c(3, 3, 2, 2, 3, 3)

#RawCriteriaMatrix <- data.frame(cbind(Fish, Rec, Res, Cost, Safe, Houses, Power))

#row.names(RawCriteriaMatrix) <- paste(c("Dam Removal", "Fish Improve", "Turbine Improve", "Turbine Add or Expand", "Dam Refurbish or Maintain", "Keep Dam"), sep = " ")
#colnames(RawCriteriaMatrix) <- paste(c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Number of Properties Impacted", "Dam Safety", "Hydropower Capacity"), sep = " ")

#CritImportance    <- c(Fish, Rec, Res, Cost, Houses, Safe, Power)/sum(Fish, Rec, Res, Cost, Houses, Safe, Power)
#CritImportance
#sum(CritImportance)
#RawCriteriaMatrix

####SAVE THIS EXAMPLE CODE TO TROUBLESHOOT PLOT DESIGN LATER########

####################TEST###########################################

colors <- c("darkblue", "purple", "green", "red", "yellow", "orange", "pink")

#--------------------------------------------------------------------------------
# Garrett's version of MCDA fn, modified for DAM DECISION purposes
#--------------------------------------------------------------------------------
WSM <- function(CritImportance, RawCriteriaMatrix){
	message('--------------------------------------------------------------------------------')
	message('WSM init')
	message('--------------------------------------------------------------------------------')

	#Build empty min/max vectors
	WSMMaxVector <- cbind(c(NA, NA, NA, NA, NA, NA))
	WSMMinVector <- cbind(c(NA, NA, NA, NA, NA, NA))

	#Fill min/max vectors
	#for (k in 1:7){
	WSMMaxVector <- c(max(RawCriteriaMatrix[,1], na.rm = FALSE), max(RawCriteriaMatrix[,2], na.rm = FALSE), max(RawCriteriaMatrix[,3], na.rm = FALSE),
					   max(RawCriteriaMatrix[,4], na.rm = FALSE), max(RawCriteriaMatrix[,5], na.rm = FALSE), max(RawCriteriaMatrix[,6], na.rm = FALSE),
					   max(RawCriteriaMatrix[,7], na.rm = FALSE))

	WSMMinVector <- c(min(RawCriteriaMatrix[,1], na.rm = FALSE), min(RawCriteriaMatrix[,2], na.rm = FALSE), min(RawCriteriaMatrix[,3], na.rm = FALSE),
					  min(RawCriteriaMatrix[,4], na.rm = FALSE), min(RawCriteriaMatrix[,5], na.rm = FALSE), min(RawCriteriaMatrix[,6], na.rm = FALSE),
					  min(RawCriteriaMatrix[,7], na.rm = FALSE))

	message('min vector', WSMMinVector)
	message('max vector', WSMMaxVector)
	message('critical matrix', CritImportance)

	#Build Score Matrix
	WSMScoreMatrix <- data.frame(matrix(data=NA, nrow = 6, ncol = 7))

	message('WSM score matrix start')

	# array of rows that used alternative method
	alt_method_rows <- c(4, 6)

	# make normalized values of each value in matrix
	for (k in 1:7){
		for (n in 1:6){
			x <- RawCriteriaMatrix[n,k]
			min_x <- WSMMinVector[k]
			max_x <- WSMMaxVector[k]
			crit_imp <- CritImportance[k]

			WSMScoreMatrix[n,k] <- tryCatch({
				if (k %in% alt_method_rows){
					# alternative? method
					(((max_x - x) / (max_x - min_x)) * crit_imp)
				}else{
					# default min/max normilization
					(((x - min_x) / (max_x - min_x)) * crit_imp)
				}
			}, error=function(e){
				return(NA)
			})

		} #End alternative (rows) for loop.
	} #End criteria (columns) for loop.

	message('WSMScoreMatrix', WSMScoreMatrix)

	IntermediateMatrix <- data.frame(matrix(data=NA, nrow=6, ncol=7))
	IntermediateMatrix[1:6, 1:7] <- round(WSMScoreMatrix,3)

	message('IntermediateMatrix', IntermediateMatrix)

	# total score is last column of returned Data Table
	scoresum <- c(NA, NA, NA, NA, NA, NA)
	scoresum <- rbind(c(sum(as.numeric(IntermediateMatrix[1:6,1]))),
					  c(sum(as.numeric(IntermediateMatrix[1:6,2]))),
					  c(sum(as.numeric(IntermediateMatrix[1:6,3]))),
					  c(sum(as.numeric(IntermediateMatrix[1:6,4]))),
					  c(sum(as.numeric(IntermediateMatrix[1:6,5]))),
					  c(sum(as.numeric(IntermediateMatrix[1:6,6]))))
	message('WSM score sum done', scoresum)

	WSMScoreDF <- data.frame(cbind(IntermediateMatrix, scoresum))
	t_IntermediateMatrix <- t(IntermediateMatrix)

	# column/row names of WSMTable
	row.names(WSMScoreDF) <- c("Dam Removal", "Fish Improve", "Turbine Improve", "Turbine Add or Expand", "Dam Refurbish or Maintain", "Keep Dam")
	colnames(WSMScoreDF) <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Number of Properties Impacted", "Dam Safety", "Hydropower Capacity", "Summed Score")

	# Assign data to be used for BarPlot
	WSMBarPlotData <- t_IntermediateMatrix

	WSMResults <- list(WSMScoreDF, WSMBarPlotData)

	return(WSMResults)
}
