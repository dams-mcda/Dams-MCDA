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

# call WSM
results <- WSM(RawCriteriaMatrix, NormalizedMatrix, DamsData, Decisions)
# results are list(Ind_WeightedScoreMatrix, Ind_scoresum, scoresum_total, fname)
#message("WSM Results", results)

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
alternative_names <- as.list(c( "Keep and Maintain Dam", "Improve Hydro", "Improve Fish Passage", "Hydro And Fish", "Remove Dam"))

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

