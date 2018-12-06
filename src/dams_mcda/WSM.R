
colors <- c("darkblue", "purple", "green", "red", "yellow", "orange", "pink") # output graph colors
# output column names
table_colnames <- c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Number of Properties Impacted", "Dam Safety", "Hydropower Capacity", "Summed Score")
# output row names
table_rownames <- c("Dam Removal", "Fish Improve", "Turbine Improve", "Turbine Add or Expand", "Dam Refurbish or Maintain", "Keep Dam")

# WSM
#----------------------------------------
# generates the MDCA Output
#
# Garrett's version of MCDA fn, modified for DAM DECISION purposes
#
# returns list(data.frame, data.frame) -> WSMScoreDF, WSMBarPlotData
#     WSMScoreDF: matrix for renderTable
#     WSMBarPlotData: matrix for rendering barplot
#
# Inputs:
#     CritImportance: #TODO: explain
#     RawCriteriaMatrix: raw score matrix
WSM <- function(CritImportance, RawCriteriaMatrix){
	#----------------------------------------
	# Min / Max Vectors
	#----------------------------------------
	WSMMaxVector <- c(max(RawCriteriaMatrix[,1], na.rm = FALSE), max(RawCriteriaMatrix[,2], na.rm = FALSE), max(RawCriteriaMatrix[,3], na.rm = FALSE),
					   max(RawCriteriaMatrix[,4], na.rm = FALSE), max(RawCriteriaMatrix[,5], na.rm = FALSE), max(RawCriteriaMatrix[,6], na.rm = FALSE),
					   max(RawCriteriaMatrix[,7], na.rm = FALSE))

	WSMMinVector <- c(min(RawCriteriaMatrix[,1], na.rm = FALSE), min(RawCriteriaMatrix[,2], na.rm = FALSE), min(RawCriteriaMatrix[,3], na.rm = FALSE),
					  min(RawCriteriaMatrix[,4], na.rm = FALSE), min(RawCriteriaMatrix[,5], na.rm = FALSE), min(RawCriteriaMatrix[,6], na.rm = FALSE),
					  min(RawCriteriaMatrix[,7], na.rm = FALSE))

	# debug
	#message('min vector ', WSMMinVector)
	#message('max vector ', WSMMaxVector)
	#message('critical matrix ', CritImportance)

	#----------------------------------------
	# Build Score Matrix
	# score will be min/max normalized values from 0-1
	#----------------------------------------
	WSMScoreMatrix <- data.frame(matrix(data=NA, nrow = 6, ncol = 7))
	# array of rows that use alternative method
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
					# alternative method
					# maximize normalization
					(((max_x - x) / (max_x - min_x)) * crit_imp)
				}else{
					# default method
					# minimize normilization
					(((x - min_x) / (max_x - min_x)) * crit_imp)
				}
			}, error=function(e){
				return(NA)
			})

		} #End alternative (rows) for loop.
	} #End criteria (columns) for loop.

	# debug
	#message('WSMScoreMatrix', WSMScoreMatrix)

	#----------------------------------------
	# IntermediateMatrix
	#----------------------------------------
	IntermediateMatrix <- data.frame(matrix(data=NA, nrow=6, ncol=7))
	IntermediateMatrix <- round(WSMScoreMatrix,3)

	# debug
	#message('IntermediateMatrix', IntermediateMatrix)

	#----------------------------------------
	# Score Sum
	#----------------------------------------
	# total score is last column of returned Data Table
	scoresum <- c(NA, NA, NA, NA, NA, NA)
	scoresum <- rbind(sum(as.numeric(IntermediateMatrix[1:6,1])),
					  sum(as.numeric(IntermediateMatrix[1:6,2])),
					  sum(as.numeric(IntermediateMatrix[1:6,3])),
					  sum(as.numeric(IntermediateMatrix[1:6,4])),
					  sum(as.numeric(IntermediateMatrix[1:6,5])),
					  sum(as.numeric(IntermediateMatrix[1:6,6])))
	message('WSM score sum done', scoresum)


	# warning adding things to list has side effects!
	WSMResults <- list(IntermediateMatrix, scoresum)

	return(WSMResults)
}

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
