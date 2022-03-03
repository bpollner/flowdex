
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

show_fdmat <- function(object) {
	nr <- nrow(object@.Data)
	nc <- ncol(object@.Data)
	txt1 <- paste0("An object of class 'fdmat' containing data from ", nr, " samples over ", nc, " fluorescence intensities.\n\n")
	txt2 <- "(showing only the first and last columns and rows)\n"
	#
	cat(txt1)
	matShow <- object@.Data[c(1,2,nr-1, nr), c(1:3, nc-2, nc-1, nc)]
	print(matShow)
	cat(txt2)
	#
	cat("\n\nSlot 'metadata':\n")
	print(object@metadata)

	cat("\n\nSlot 'pData':\n")
	print(object@pData)

	cat("\n\nSlot 'eventsPerVol':\n")
	print(object@eventsPerVol)

	cat("\n\nSlot 'cyTags':\n")
	print(object@cyTags)
	
	cat("\n\nSlot 'gateStrat':\n")
	print(object@gateStrat)
} # EOF

plot_gateStructure <- function(x, y, ...) {
	flowWorkspace::plot(x)
} # EOF
