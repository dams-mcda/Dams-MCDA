
# renderBarPlot
#----------------------------------------
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
renderBarPlot <- function(data, title, x_names, x_label, y_label, colors, x_limit, y_limit) {
	# debug data
	message('------------------')
	message('BarPlot title:', title, '\ndata:', data, "\n#(values):", length(data), "\nclasstype: ", class(data), "\ndatatype: ", typeof(data), "\nnames:", x_names, "\n#(names):", length(x_names))
	message('------------------')
	# new graph (ggplot2) requires a data frame not vectors
	if (is.vector(data)){
		df <- data.frame(Criteria=x_names, Score=data)
	}

	result <-  renderPlot(
		ggplot(
		  df,
		  aes(x=Criteria, y=Score, fill=Criteria)
		)
		+ geom_bar(stat="identity")
		+ geom_text(data=subset(df, Score != 0), aes(label=Score), color="white", hjust=1, vjust=0.4, size=6)
		+ geom_text(data=subset(df, Score == 0), aes(label=Score), color="black", hjust=-1, vjust=0.4, size=6)
		+ coord_flip()
		+ theme_minimal()
		+ theme(legend.position="none", text=element_text(size=20), )
		+ scale_x_discrete(limits=rev(x_names))
		+ ylab(y_label)
		+ xlab(x_label)
	)
	return(result)
}


# renderStackedBarPlot
#----------------------------------------
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
renderStackedBarPlot <- function(data, title, x_names, x_label, y_label, colors, x_limit, y_limit) {
	# debug data
	message('------------------')
	message('StackedBarPlot title:', title, '\ndata:', data, "\n#(values):", length(data), "\nclasstype: ", class(data), "\ndatatype: ", typeof(data), "\nnames:", x_names, "\n#(names):", length(x_names))
	message('------------------')
	# new graph (ggplot2) requires a data frame not vectors
	if (is.vector(data)){
		df <- data.frame(Criteria=x_names, Score=data)
	}

	result <-  renderPlot(
		ggplot(
		  df,
		  aes(x=Criteria, y=Score, fill=Criteria)
		)
		+ geom_bar(stat="identity")
		+ geom_text(data=subset(df, Score != 0), aes(label=Score), color="white", hjust=1, vjust=0.4, size=6)
		+ geom_text(data=subset(df, Score == 0), aes(label=Score), color="black", hjust=-1, vjust=0.4, size=6)
		+ coord_flip()
		+ theme_minimal()
		+ theme(legend.position="none", text=element_text(size=20), )
		+ scale_x_discrete(limits=rev(x_names))
		+ ylab(y_label)
		+ xlab(x_label)
	)
	return(result)
}
