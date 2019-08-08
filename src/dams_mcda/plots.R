# plots.R
# all wrappers for plot rendering

# renderBarPlot
#----------------------------------------
# wrapper for barplot with a debug message
# when no value is needed pass NULL for a field
# x_limit and y_limit are arrays when not NULL
# xpd == False disables bars being drawn outsize graph canvas
renderBarPlot <- function(df, title, x_names, x_label, y_label, colors, x_limit, y_limit) {
	# debug data
	message('------------------')
	message('BarPlot title:', title, '\ndata:', data, "\n#(values):", length(data), "\nclasstype: ", class(data), "\ndatatype: ", typeof(data), "\nnames:", x_names, "\n#(names):", length(x_names))
	message('------------------')
	# new graph (ggplot2) requires a data frame not vectors
	if (is.vector(df)){
		message('BarPlot convert vector data to frame')
		df <- data.frame(criteria=x_names, score=df)
	}
  
	plot <- ggplot(data=df, mapping =aes(x=criteria, y=score, fill=criteria)
	               #if data frame column mappings conflict with environment we may need to specify an execution environment
	               #environment = environment(),
	               )
	
	result <-  renderPlot(
		plot(
		  df,
		  aes(x=Criteria, y=Score, fill=Criteria)
		)
		+ geom_bar(stat="identity")
		+ geom_text(data=subset(df, score != 0), 
		            aes(label=score), position = position_stack(vjust=0.5), size=6)
		+ coord_flip()
		+ theme_minimal()
		+ theme(legend.position="none", text=element_text(size=20), )
		+ scale_x_discrete(limits=rev(x_names))
		+ ylab(y_label)
		+ xlab(x_label)
	)
	return(result)
}

