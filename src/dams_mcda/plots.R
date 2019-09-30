# plots.R
# all wrappers for plot rendering
library(reshape2)
library(viridis)
library(stringr)

# renderBarPlot
#----------------------------------------
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
renderBarPlot <- function(df, title, x_names, x_label, y_label, colors, x_limit, y_limit) {
	# debug data
	#message('------------------')
	#message(
	#	'BarPlot title: ', title,
	#	'\ndata: ', df,
	#	"\n#(values in data): ", length(df),
	#	"\nclasstype: ", class(df),
	#	"\ndatatype: ", typeof(df),
	#	#"\nnames:", x_names,
	#	"\n#(x names): ", length(x_names)
	#)
	#message('------------------')

	# new graph (ggplot2) requires a data frame not vectors
	if (is.vector(df)){
		message('BarPlot convert vector data to frame')
		df <- data.frame(criteria=x_names, score=df)
	}
	else if (is.matrix(df)){
		message('BarPlot convert matrix data to frame')
		df <- data.frame(criteria=x_names, score=df)
	}

	plot <- ggplot(
		data=df,
		mapping = aes(x=criteria, y=score, fill=criteria)
		# if data frame column mappings conflict with environment we may need to specify an execution environment
		#environment = environment(),
	)

	result <-  renderPlot(
		plot
		+ geom_bar(stat="identity")
		+ geom_text(data=subset(df, score != 0), aes(label=score), position = position_stack(vjust=0.5), size=6)
		+ coord_flip()
		+ theme_minimal()
		+ theme(legend.position="none", text=element_text(size=20))
		+ scale_x_discrete(limits=rev(x_names))
		+ ylab(y_label)
		+ xlab(x_label)
		+ scale_fill_viridis(discrete=TRUE)
	)
	return(result)
}


# renderCombinedBarPlot
#----------------------------------------
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
renderCombinedBarPlot <- function(df, title, x_names, x_label, y_label, colors, x_limit, y_limit) {
	# this method plots by criteria
	# debug data
	#message(
	#	'------------------\n',
	#	'CombinedBarPlot title: ', title,
	#	# '\ndata: ', df,
	#	"\n#(values in data): ", length(df),
	#	"\n#(dim of data): ", dim(df),
	#	"\nclasstype: ", class(df),
	#	"\ndatatype: ", typeof(df),
	#	"\n#(x names): ", length(x_names),
	#	'\n------------------'
	#)

	Dam <- c(rep(dam_names, times=length(x_names)))
	Criteria <- c(rep(x_names, each=length(dam_names)))
	Score <- unlist(as.data.frame(df))
	df <- data.frame(Criteria, Dam, Score)

	# ordering by order of criteria appearance
	df$Criteria <- factor(df$Criteria, levels=unique(df$Criteria))

	#message("Dams ", Dam, " dim ", dim(Dam))
	#message("Crits ", Criteria, " dim ", dim(Criteria))
	#message("Score ", Score, " dim ", dim(Score))

	plot <- ggplot(
		data=df,
		# if data frame column mappings conflict with environment we may need to specify an execution environment
		#environment = environment(),
		mapping = aes(x=Criteria, y=Score, fill=Dam, label=Score)
	)

	result <-  renderPlot(
		plot
		# inclue empty values
		+ geom_bar(stat="identity")
		# ignore empty values (uncomment)
		#+ geom_bar(data=subset(df, Score != 0), stat="identity") # ignore empty values
		#+ coord_flip() # sometimes helpful for better fitting graph on screen
		+ geom_text(data=subset(df, Score != 0), size=4, position = position_stack(vjust = 0.5))
		+ theme_minimal()
		+ theme(
			text=element_text(size=16),
			legend.position="bottom",
			axis.text.y = element_text(angle = 0, hjust = 1),
			axis.text.x = element_text(angle = 45, hjust = 1)
		)
		+ ylab(y_label)
		+ xlab(x_label)
		+ scale_fill_viridis(discrete=TRUE)
	)
	return(result)
}


# renderCombinedBarPlot2
#----------------------------------------
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
renderCombinedBarPlot2 <- function(df, title, x_names, x_label, y_label, colors, x_limit, y_limit) {
	# this method plots by dam 
	# debug data
	message(
		'------------------\n',
		'CombinedBarPlot title: ', title,
		# '\ndata: ', df,
		"\n#(values in data): ", length(df),
		"\n#(dim of data): ", dim(df),
		"\nclasstype: ", class(df),
		"\ndatatype: ", typeof(df),
		"\n#(x names): ", length(x_names),
		'\n------------------'
	)

	Dam <- c(rep(dam_names, times=length(x_names)))
	Criteria <- c(rep(x_names, each=length(dam_names)))
	Score <- unlist(as.data.frame(df))
	df <- data.frame(Criteria, Dam, Score)

	# ordering by order of dam appearance
	df$Dam <- factor(df$Dam, levels=unique(df$Dam))

	#message("Dams ", Dam, " dim ", dim(Dam))
	#message("Crits ", Criteria, " dim ", dim(Criteria))
	#message("Score ", Score, " dim ", dim(Score))

	plot <- ggplot(
		data=df,
		# if data frame column mappings conflict with environment we may need to specify an execution environment
		#environment = environment(),
		mapping = aes(x=Dam, y=Score, fill=str_wrap(Criteria, 24), label=Score, order=Criteria)
	)

	result <-  renderPlot(
		plot
		# inclue empty values
		+ geom_bar(stat="identity")
		# ignore empty values (uncomment)
		#+ geom_bar(data=subset(df, Score != 0), stat="identity") # ignore empty values
		#+ coord_flip() # sometimes helpful for better fitting graph on screen
		+ geom_text(data=subset(df, Score != 0), size=4, position = position_stack(vjust = 0.5))
		+ theme_minimal()
		+ theme(
			text=element_text(size=16),
			legend.position="bottom",
			legend.key.height=unit(1.5,"cm"),
			axis.text.y = element_text(angle = 0, hjust = 1),
			axis.text.x = element_text(angle = 45, hjust = 1)
		)
		+ guides(fill=guide_legend(title=x_label))
		+ ylab(y_label)
		+ xlab(x_label)
		# Criteria labels are long
		+ scale_fill_viridis(discrete=TRUE)
	)
	return(result)
}


# renderPlot2D
#----------------------------------------
# for 2D data
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
# NOTE: DOES NOT RENDER PLOT
renderPlot2D <- function(df, title, x_names, y_names, x_label, y_label, legend_label, colors, x_limit, y_limit) {
	#message(
	#	'------------------\n',
	#	'Plot2D title: ', title,
	#	# '\ndata: ', df,
	#	"\n#(values in data): ", length(df),
	#	"\n#(dim of data): ", dim(df),
	#	"\nclasstype: ", class(df),
	#	"\ndatatype: ", typeof(df),
	#	"\n#(x names): ", length(x_names),
	#	"\n#(y names): ", length(y_names),
	#	'\n------------------'
	#)

	Y <- c(rep(str_wrap(y_names, 24), times=length(x_names)))
	X <- c(rep(str_wrap(x_names, 24), each=length(y_names)))
	Score <- unlist(as.data.frame(df))
	Score[Score==0] <- NA # dont show zero values on plot
	df <- data.frame(X=X, Y=Y, Score=Score)

	# ordering by order of appearance
	df$X <- factor(df$X, levels=unique(df$X))
	#message("non zero values of score: ", subset(df$Score, df$Score != 0))

	result <- (
		ggplot(data=df, mapping = aes(x=df$X, y=df$Score, fill=df$Y, label=df$Score))
		# ingnore empty values
		#+ geom_bar(data=subset(df, Score != 0), stat="identity") # ignore empty values
		+ geom_bar(stat="identity") # ignore empty values
		+ geom_text(size=4, position = position_stack(vjust = 0.5))
		#+ coord_flip() # sometimes helpful for better fitting graph on screen
		+ theme_minimal()
		+ theme(
			text=element_text(size=16),
			legend.position="bottom",
			axis.text.y = element_text(angle = 0, hjust = 1),
			axis.text.x = element_text(angle = 45, hjust = 1),
			plot.margin=unit(c(0,1,0,1.5), "cm")
		)
		+ guides(fill=guide_legend(title=legend_label))
		+ ylab(y_label)
		+ xlab(x_label)
		+ scale_fill_viridis(discrete=TRUE)
	)
	return(result)
}


# renderPlot2DCluster
#----------------------------------------
# for 2D data
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
# NOTE: DOES NOT RENDER PLOT
renderPlot2DCluster <- function(df, title, x_names, y_names, x_label, y_label, legend_label, colors, x_limit, y_limit) {
	#message(
	#	'------------------\n',
	#	'Plot2DCluster title: ', title,
	#	# '\ndata: ', df,
	#	"\n#(values in data): ", length(df),
	#	"\n#(dim of data): ", dim(df),
	#	"\nclasstype: ", class(df),
	#	"\ndatatype: ", typeof(df),
	#	"\n#(x names): ", length(x_names),
	#	"\n#(y names): ", length(y_names),
	#	'\n------------------'
	#)

	Y <- c(rep(str_wrap(y_names, 24), times=length(x_names)))
	X <- c(rep(str_wrap(x_names, 24), each=length(y_names)))
	Score <- unlist(as.data.frame(df))
	Score[Score==0] <- NA # dont show zero values on plot
	df <- data.frame(X=X, Y=Y, Score=Score)

	# ordering by order of appearance
	df$X <- factor(df$X, levels=unique(df$X))
	#message("non zero values of score: ", subset(df$Score, df$Score != 0))

	result <- (
		ggplot(data=df, mapping = aes(x=df$X, y=df$Score, fill=df$Y, label=df$Score))
		# ingnore empty values
		#+ geom_bar(data=subset(df, Score != 0), stat="identity") # ignore empty values
		+ geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.7)) # ignore empty values
		+ geom_text(size=3, position = position_dodge(width=0.7), angle=90, hjust=-0.2) #, position = position_stack(vjust = 0.5))
		+ coord_cartesian(clip="off")
		#+ coord_flip() # sometimes helpful for better fitting graph on screen
		+ theme_classic()
		+ theme(
			text=element_text(size=16),
			legend.position="bottom",
			axis.text.y = element_text(angle = 0, hjust = 1),
			axis.text.x = element_text(angle = 45, hjust = 1),
			plot.margin=unit(c(1,1,1,1.5), "cm")
		)
		+ guides(fill=guide_legend(title=legend_label))
		+ ylab(y_label)
		+ xlab(x_label)
		+ scale_fill_viridis(discrete=TRUE)
	)
	return(result)
}


# renderPlot2DDamAlts
#----------------------------------------
# for 2D data
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
# NOTE: DOES NOT RENDER PLOT
renderPlot2DDamAlts <- function(df, title, x_names, y_names, alt_names, x_label, y_label, legend_label, colors, x_limit, y_limit) {
	#message(
	#	'------------------\n',
	#	'Plot2D title: ', title,
	#	# '\ndata: ', df,
	#	"\n#(values in data): ", length(df),
	#	"\n#(dim of data): ", dim(df),
	#	"\nclasstype: ", class(df),
	#	"\ndatatype: ", typeof(df),
	#	"\n#(x names): ", length(x_names),
	#	"\n#(y names): ", length(y_names),
	#	'\n------------------'
	#)

	Y <- c(rep(str_wrap(y_names, 24), times=length(x_names)))
	X <- c(rep(str_wrap(x_names, 24), each=length(y_names)))
	for (xid in 1:length(alt_names)){
		Y[xid] <- paste(Y[xid], alt_names[xid], sep="-")
	}
	Score <- unlist(as.data.frame(df))
	Score[Score==0] <- NA # dont show zero values on plot
	df <- data.frame(X=X, Y=Y, Score=Score)

	# ordering by order of appearance
	df$X <- factor(df$X, levels=unique(df$X))
	#message("non zero values of score: ", subset(df$Score, df$Score != 0))

	result <- (
		ggplot(data=df, mapping = aes(x=df$X, y=df$Score, fill=df$Y, label=df$Score))
		# ingnore empty values
		#+ geom_bar(data=subset(df, Score != 0), stat="identity") # ignore empty values
		+ geom_bar(stat="identity") # ignore empty values
		+ geom_text(size=4, position = position_stack(vjust = 0.5))
		#+ coord_flip() # sometimes helpful for better fitting graph on screen
		+ theme_minimal()
		+ theme(
			text=element_text(size=16),
			legend.position="bottom",
			axis.text.y = element_text(angle = 0, hjust = 1),
			axis.text.x = element_text(angle = 45, hjust = 1),
			plot.margin=unit(c(0,1,0,1.5), "cm"),
			legend.text=element_text(size=rel(0.7))
		)
		+ guides(fill=guide_legend(title=legend_label))
		+ ylab(y_label)
		+ xlab(x_label)
		+ scale_fill_viridis(discrete=TRUE)
		+ stat_summary(fun.y = sum, aes(label = ..y.., group = df$X), geom = "text", vjust=-.2) # dont know what ..y.. is but it works
	)
	return(result)
}

# renderPlot1D
#----------------------------------------
# for 1D data
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
renderPlot1D <- function(df, title, x_names, x_label, y_label, colors, x_limit, y_limit) {
	#message(
	#	'------------------\n',
	#	'Plot1D title: ', title,
	#	# '\ndata: ', df,
	#	"\n#(values in data): ", length(df),
	#	"\n#(dim of data): ", dim(df),
	#	"\nclasstype: ", class(df),
	#	"\ndatatype: ", typeof(df),
	#	"\n#(x names): ", length(x_names),
	#	'\n------------------'
	#)

	X <- c(rep(str_wrap(x_names, 24), each=length(1)))
	Score <- unlist(as.data.frame(df))
	df <- data.frame(X=X, Score=Score)

	# ordering by order of appearance
	df$X <- factor(df$X, levels=unique(df$X))

	result <- (
		ggplot(data=df, mapping = aes(x=df$X, y=df$Score, label=df$Score))
		# inclue empty values
		+ geom_bar(stat="identity")
		# ignore empty values (uncomment)
		#+ geom_bar(data=subset(df, Score != 0), stat="identity") # ignore empty values
		#+ coord_flip() # sometimes helpful for better fitting graph on screen
		#+ geom_text(data=subset(df, Score != 0), size=4, position = position_stack(vjust = 0.5))
		+ theme_minimal()
		+ theme(
			text=element_text(size=16),
			legend.position="bottom",
			axis.text.y = element_text(angle = 0, hjust = 1),
			axis.text.x = element_text(angle = 45, hjust = 1)
		)
		+ ylab(y_label)
		+ xlab(x_label)
		+ scale_fill_viridis(discrete=TRUE)
	)
	return(result)
}



# renderPlot2DScaled100
#----------------------------------------
# for 2D data
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
renderPlot2DScaled100 <- function(df, title, x_names, y_names, x_label, y_label, legend_label, colors, x_limit) {
	#message(
	#	'------------------\n',
	#	'Plot2DScaled100 title: ', title,
	#	# '\ndata: ', df,
	#	"\n#(values in data): ", length(df),
	#	"\n#(dim of data): ", dim(df),
	#	"\nclasstype: ", class(df),
	#	"\ndatatype: ", typeof(df),
	#	"\n#(x names): ", length(x_names),
	#	"\n#(x names): ", length(y_names),
	#	'\n------------------'
	#)

	Y <- c(rep(str_wrap(y_names, 24), times=length(x_names)))
	X <- c(rep(str_wrap(x_names, 24), each=length(y_names)))
	Score <- unlist(as.data.frame(prop.table(df, 2)))
	df <- data.frame(X=X, Y=Y, Score=Score)

	# ordering by order of appearance
	df$X <- factor(df$X, levels=unique(df$X))

	result <- (
		ggplot(data=df,
		   mapping = aes(x=df$X, y=df$Score, fill=df$Y, label=df$Score)
	    )
		# inclue empty values
		+ geom_bar(stat="identity")
		# ignore empty values (uncomment)
		#+ geom_bar(data=subset(df, Score != 0), stat="identity") # ignore empty values
		#+ coord_flip() # sometimes helpful for better fitting graph on screen
		#+ geom_text(data=subset(df, Score != 0), size=4, position = position_stack(vjust = 0.5))
		+ theme_minimal()
		+ theme(
			text=element_text(size=16),
			legend.position="bottom",
			axis.text.y = element_text(angle = 0, hjust = 1),
			axis.text.x = element_text(angle = 45, hjust = 1)
		)
		+ guides(fill=guide_legend(title=legend_label))
		+ ylab(y_label)
		+ xlab(x_label)
		+ scale_fill_viridis(discrete=TRUE)
		+ scale_y_continuous(limits=c(0,1), labels = scales::percent_format())
	)
	return(result)
}



# renderBarErrorPlot
#----------------------------------------
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
#
# differs from renderBarPlot by having error bars
#
# could add ability for low/high error bars
renderBarErrorPlot <- function(df, title, x_names, x_label, y_label, colors, x_limit, y_limit, error_amount, error_bar_width, error_bar_color) {
	# debug data
	message('------------------')
	message(
		'BarErrorPlot title:', title,
		'\ndata:', df,
		"\n#(values in data):", length(df),
		"\nclasstype: ", class(df),
		"\ndatatype: ", typeof(df),
		#"\nnames:", x_names,
		"\n#(x names):", length(x_names)
	)
	message('------------------')

	# new graph (ggplot2) requires a data frame not vectors
	if (is.vector(df)){
		message('BarErrorPlot convert vector data to frame')
		df <- data.frame(criteria=x_names, score=df)
	}

	plot <- ggplot(
		data=df,
		mapping=aes(x=criteria, y=score, fill=criteria)
		# if data frame column mappings conflict with environment we may need to specify an execution environment
		#environment=environment(),
	)

	result <-  renderPlot(
		plot
		+ geom_bar(stat="identity")
		+ geom_errorbar(
			aes(ymin=score-error_amount, ymax=score+error_amount),
			width=error_bar_width,
			color=error_bar_color,
			position=position_dodge(1)
		)
		+ geom_text(data=subset(df, score != 0), aes(label=score), position = position_stack(vjust=0.5), size=6)
		+ coord_flip()
		+ theme_minimal()
		+ theme(legend.position="none", text=element_text(size=20))
		+ scale_x_discrete(limits=rev(x_names))
		+ ylab(y_label)
		+ xlab(x_label)
	)
	return(result)
}

