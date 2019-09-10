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

TestData <- read.csv('TestData2.csv')
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

# West Enfield/Dam 1 output table(s)

Dam1RawTable <- setDT(WestEnf_DataMatrix)
row.names(Dam1RawTable) <- alternative_names
colnames(Dam1RawTable) <- criteria_inputs

Dam1NormTable <- setDT(data.frame(Ind_NormalizedMatrix[,,1]))
row.names(Dam1NormTable) <- alternative_names
colnames(Dam1NormTable) <- criteria_inputs

Dam1ScoreTable <- setDT(Dam1Results)
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

Dam3RawTable <- setDT(Mill_DataMatrix)
rownames(Dam3RawTable) <- alternative_names
colnames(Dam3RawTable) <- criteria_inputs

Dam3NormTable <- setDT(data.frame(Ind_NormalizedMatrix[,,3]))
row.names(Dam3NormTable) <- alternative_names
colnames(Dam3NormTable) <- criteria_inputs

Dam3ScoreTable <- setDT(Dam3Results)
row.names(Dam3ScoreTable) <- alternative_names
colnames(Dam3ScoreTable) <- criteria_inputs

# East Millinocket/Dam 4 output table(s)

Dam4RawTable <- setDT(EastMill_DataMatrix)
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

Dam6RawTable <- setDT(Dolby_DataMatrix)
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
colnames(Score_compare) <- criteria_inputs
rownames(Score_compare) <- alternative_names

# Graph ALL DAM alternative scores with adjacent bars grouped by dam
WSMPlota <- barplot(t(Score_compare), ylim= c(0,1.0), main="Dam Decision Recommendation Comparison", ylab= "MCDA Score",
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
Score1 <- as.matrix(Ind_scoresum[1,])
rownames(Score1) <- alternative_names

# Graph West Enfield alternative scores
WSMPlot1a <- barplot((Score1), ylim= c(0,1.0), main="West Enfield Dam Recommendation", ylab= "Decision Alternative Score",
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
Score2 <- as.matrix(Ind_scoresum[2,])
rownames(Score2) <- alternative_names

# Graph  alternative scores
WSMPlot2a <- barplot((Score2), ylim= c(0,1.0), main="Medway Dam Recommendation", ylab= "Decision Alternative Score",
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
Score3 <- as.matrix(Ind_scoresum[3,])
rownames(Score3) <- alternative_names

# Graph alternative scores
WSMPlot3a <- barplot((Score3), ylim= c(0,1.0), main="Millinocket Dam Recommendation", ylab= "Decision Alternative Score",
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
Score4 <- as.matrix(Ind_scoresum[4,])
rownames(Score4) <- alternative_names

# Graph alternative scores
WSMPlot4a <- barplot((Score4), ylim= c(0,1.0), main="East Millinocket Dam Recommendation", ylab= "Decision Alternative Score",
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
Score5 <- as.matrix(Ind_scoresum[5,])
rownames(Score5) <- alternative_names

# Graph alternative scores
WSMPlot5a <- barplot((Score5), ylim= c(0,1.0), main="North Twin Dam Recommendation", ylab= "Decision Alternative Score",
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
Score6 <- as.matrix(Ind_scoresum[6,])
rownames(Score6) <- alternative_names

# Graph alternative scores
WSMPlot6a <- barplot((Score6), ylim= c(0,1.0), main="Dolby Dam Recommendation", ylab= "Decision Alternative Score",
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
Score7 <- as.matrix(Ind_scoresum[7,])
rownames(Score7) <- alternative_names

# Graph alternative scores
WSMPlot7a <- barplot((Score7), ylim= c(0,1.0), main="Millinocket Lake Dam Recommendation", ylab= "Decision Alternative Score",
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
Score8 <- as.matrix(Ind_scoresum[8,])
rownames(Score8) <- alternative_names

# Graph alternative scores
WSMPlot8a <- barplot((Score8), ylim= c(0,1.0), main="Ripogenus Dam Recommendation", ylab= "Decision Alternative Score",
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
WSMPlot9 <- barplot((DamsTopScenGraph), ylim= c(0,1.0), main="Top Dam Scenario", ylab="MCDA Score", col=rainbow(14),
                     cex.axis=0.8, las=1, names.arg= dam_names, cex=0.7) 

legend("topleft", criteria_inputs, cex=0.6, bty="n", fill=rainbow(14))
