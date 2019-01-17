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
	matrix_cols <- length(criteria_inputs) # 7 default (output size, adds summedscore)
	matrix_rows <- length(available_alternatives) # 5 default


	message("Path A cols ", matrix_cols, " rows ", matrix_rows )

	#----------------------------------------
	# PATH A: Build Score Matrix, no normalization
	# score will be raw values from 0-1 based on user input
	#----------------------------------------
	WSMMatrix <- data.frame(matrix(data=NA, nrow = matrix_rows, ncol = matrix_cols))

	message("Fill WSM Matrix")
	# weight values of each raw score in matrix
	for (k in 1:matrix_cols){
		for (n in 1:matrix_rows){
			x <- RawCriteriaMatrix[n,k]
			crit_imp <- CritImportance[k]

			WSMMatrix[n,k] <- tryCatch({
				(x * crit_imp)
			}, error=function(e){
				(NA)
			})
		} #End alternative (rows) for loop.
	} #End criteria (columns) for loop.


	message("Path B")
	#----------------------------------------
	# PATH B: Normalization using Min / Max Vectors
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
	message('min vector ', WSMMinVector)
	message('max vector ', WSMMaxVector)
	#message('critical matrix ', CritImportance)


	#----------------------------------------
	# PATH B: Build Score Matrix
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
				(NA)
			})
		} #End alternative (rows) for loop.
	} #End criteria (columns) for loop.

	# debug
	message('WSMScoreMatrix', WSMScoreMatrix)

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

