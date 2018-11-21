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

#Garrett's version of MCDA fn, modified for DAM DECISION purposes
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

	message('min vector')
	message(WSMMinVector)
	message('max vector')
	message(WSMMaxVector)

	#Build Score Matrix
	WSMScoreMatrix <- data.frame(matrix(data=NA, nrow = 6, ncol = 7))

	message('WSM score matrix start')
	for (k in 1:7){
		for (n in 1:6){
			if (k == 1){
				WSMScoreMatrix[n,k] <- ((RawCriteriaMatrix[n,k]-WSMMinVector[k])/(WSMMaxVector[k]-WSMMinVector[k]))*CritImportance[k]
			}else if (k == 2){
				WSMScoreMatrix[n,k] <- ((RawCriteriaMatrix[n,k]-WSMMinVector[k])/(WSMMaxVector[k]-WSMMinVector[k]))*CritImportance[k]
			}else if (k == 3){
				WSMScoreMatrix[n,k] <- ((RawCriteriaMatrix[n,k]-WSMMinVector[k])/(WSMMaxVector[k]-WSMMinVector[k]))*CritImportance[k]
			}else if (k == 4){
				WSMScoreMatrix[n,k] <- ((WSMMaxVector[k]-RawCriteriaMatrix[n,k])/(WSMMaxVector[k]-WSMMinVector[k]))*CritImportance[k]
			}else if (k == 5){
				WSMScoreMatrix[n,k] <- ((RawCriteriaMatrix[n,k]-WSMMinVector[k])/(WSMMaxVector[k]-WSMMinVector[k]))*CritImportance[k]
			}else if (k == 6){
				WSMScoreMatrix[n,k] <- ((WSMMaxVector[k]-RawCriteriaMatrix[n,k])/(WSMMaxVector[k]-WSMMinVector[k]))*CritImportance[k]
			}else if (k == 7){
				WSMScoreMatrix[n,k] <- ((RawCriteriaMatrix[n,k]-WSMMinVector[k])/(WSMMaxVector[k]-WSMMinVector[k]))*CritImportance[k]
			}
			message('raw value')
			message(RawCriteriaMatrix[n,k])
			message('new value')
			message(WSMScoreMatrix[n,k])

		} #End alternative (rows) for loop.
	} #End criteria (columns) for loop.
	message('WSMScoreMatrix')
	message(WSMScoreMatrix)

	IntermediateMatrix <- data.frame(matrix(data=NA, nrow=6, ncol=7))
	IntermediateMatrix[1:6, 1:7] <- round(WSMScoreMatrix,3)

	message('IntermediateMatrix')
	message(IntermediateMatrix)

	message('WSM score sum')
	#for (n in 1:6){
	scoresum <- c(NA, NA, NA, NA, NA, NA)
	scoresum <- rbind(c(sum(as.numeric(IntermediateMatrix[1:6,1]))),
					  c(sum(as.numeric(IntermediateMatrix[1:6,2]))),
					  c(sum(as.numeric(IntermediateMatrix[1:6,3]))),
					  c(sum(as.numeric(IntermediateMatrix[1:6,4]))),
					  c(sum(as.numeric(IntermediateMatrix[1:6,5]))),
					  c(sum(as.numeric(IntermediateMatrix[1:6,6]))))
	#}
	message('WSM score sum done')

	WSMScoreDF <- as.matrix(data.frame(cbind(IntermediateMatrix, scoresum)))
	t_IntermediateMatrix <- t(IntermediateMatrix)

	row.names(WSMScoreDF) <- paste(c("Dam Removal", "Fish Improve", "Turbine Improve", "Turbine Add or Expand", "Dam Refurbish or Maintain", "Keep Dam"), sep = " ")
	colnames(WSMScoreDF) <- paste(c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Project Costs", "Number of Properties Impacted", "Dam Safety", "Hydropower Capacity", "Summed Score"), sep = " ")
	message('WSM names done')

	#Make Bar Chart
	WSMBar <- barplot(t_IntermediateMatrix, main="WSM Ranked Alternatives",
					  xlab=c("Dam Removal", "Fish Improve", "Turbine Improve", "Turbine Add or Expand", "Dam Refurbish or Maintain", "Keep Dam"),
					  col=colors,
					  legend = c("Fish Biomass", "River Recreation", "Reservoir Storage", "One-Time Costs", "Safety", "Number of Houses", "Hydropower Capacity"))

	WSMResults <- list(WSMScoreDF, WSMBar, IntermediateMatrix)
	message('WSM results done')
	return(WSMResults)
}

####SAVE THIS EXAMPLE CODE TO TROUBLESHOOT PLOT DESIGN LATER########

#WSMResults <- WSM(CritImportance=CritImportance, RawCriteriaMatrix=RawCriteriaMatrix)
#WSMResults[1]
#WSMResults[2]

####SAVE THIS EXAMPLE CODE TO TROUBLESHOOT PLOT DESIGN LATER########
