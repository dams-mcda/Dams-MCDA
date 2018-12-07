
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

	# matrix setup
	matrix_cols <- length(criterion_inputs) # 7 default (output size, adds summedscore)
	matrix_rows <- length(available_alternatives) # 5 default

	#----------------------------------------
	# Min / Max Vectors
	#----------------------------------------
	# iterate each criteria for min,max
	WSMMaxVector <- list("list", matrix_cols)
	for ( index in 1:matrix_cols ){
		WSMMaxVector[[index]] <- max(RawCriteriaMatrix[,index], na.rm=FALSE)
	}
	WSMMaxVector <- unlist(WSMMaxVector)

	WSMMinVector <- list("list", matrix_cols)
	for ( index in 1:matrix_cols ){
		WSMMinVector[[index]] <- min(RawCriteriaMatrix[,index], na.rm=FALSE)
	}
	WSMMinVector <- unlist(WSMMinVector)

	# debug
	#message('min vector ', WSMMinVector)
	#message('max vector ', WSMMaxVector)
	#message('critical matrix ', CritImportance)


	#----------------------------------------
	# Build Score Matrix
	# score will be min/max normalized values from 0-1
	#----------------------------------------
	WSMScoreMatrix <- data.frame(matrix(data=NA, nrow = matrix_rows, ncol = matrix_cols))
	# array of rows that use alternative method
	alt_method_columns <- c(4, 6)

	# make normalized values of each value in matrix
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			x <- RawCriteriaMatrix[n,k]
			min_x <- WSMMinVector[k]
			max_x <- WSMMaxVector[k]
			crit_imp <- CritImportance[k]

			WSMScoreMatrix[n,k] <- tryCatch({
				if (k %in% alt_method_columns){
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
	IntermediateMatrix <- data.frame(matrix(data=NA, nrow=matrix_rows, ncol=matrix_cols))
	IntermediateMatrix <- round(WSMScoreMatrix,3)

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
