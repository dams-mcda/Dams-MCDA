#WSM: weighted sum code for dam decisions
#SRoy 8-6-19
#given a database of criteria scores, separated by criteria, dam, and decision scenario,
#and user preferences split by criteria and dam,
#take their product, summ resulting scores per scenario, and rank scenarios based on total score,
#then find the map that matches the highest ranked scenario
#NB: no system arguments passed but can be modified to take user preferences
#NB: currently only loading up pre-normalized criteria scores, norm'd by min/max range

#load libraries
#no libraries used right now

#load criteria scores normalized by range
load(file='./f_nrge.RData')

#get preference data from users...
#testing with synthetic data but eventually replace with real
#load(file='./prefs.RData') #or whatever?
prefs <- rep(1/14, 14*8)
prefs <- array(prefs, c(8,14))

#declare a weighted sum variable
wesum <- rep(0,dim(f_nrge)[3])

#multiply crit scores by user preferences
for (i in 1:dim(f_nrge)[3]){
  f_np <- f_nrge[,,i] * prefs
  wesum[i] <- sum(f_np) #this sums everything in each scenario after they are preferenced. Should be fine as order doesn't matter at this point.
}

#order scenarios by rank: largest score first
idxRank <- order(wesum,decreasing=TRUE)


#use scenario idxRank[1] to find corresponding map name
fname <- sprintf('./maps/Penobscot_MO_14_%d.png',idxRank[1])

#print stuff: discared for final version, just here to debug
#print('top ten scenarios:')
#print(idxRank[1:10])
#print('raw scores:')
#print(wesum[idxRank[1:10]])
#print('name of map:')
#print(fname)

