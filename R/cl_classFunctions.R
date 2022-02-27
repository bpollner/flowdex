
#### regarding gating set ####

show_GatingSet_flowWorkspace <- function(object) {
	saNames <- flowWorkspace::sampleNames(object)
	nr <- 1: length(saNames)
	isnDF <- data.frame(nr, saNames)
	colnames(isnDF) <- c("Index", "SampleNames")
	#
	colNames <- flowWorkspace::colnames(object)
	#
	txtA <- paste0("A GatingSet of class 'GatingSet' with ", length(saNames), " samples:\n\n")
	txtB <- paste0("The following ", length(colNames), " channels are available:\n", paste(colNames, collapse="    "))
	#
	cat(txtA)
	print(isnDF)
	cat("\n\n")
	cat(txtB)
} # EOF





show_GatingSet_fd <- function(object) {
	saNames <- flowWorkspace::sampleNames(object)
	nr <- 1: length(saNames)
	isnDF <- data.frame(nr, saNames)
	colnames(isnDF) <- c("Index", "SampleNames")
	#
	colNames <- flowWorkspace::colnames(object)
	#
	txtA <- paste0("A GatingSet of class 'GatingSet_fd' with ", length(saNames), " samples:\n\n")
	txtB <- paste0("The following ", length(colNames), " channels are available:\n", paste(colNames, collapse="    "))
	txtC <- paste0("The following gating strategy derived from file '", object@gateStrat@filename, "' has been applied:\n")
	#
	cat(txtA)
	print(isnDF)
	cat("\n\n")
	cat(txtB)
	cat("\n\n")
	cat(txtC)
	print(object@gateStrat)
} # EOF
