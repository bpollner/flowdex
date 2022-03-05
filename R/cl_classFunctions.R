
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
	#
	return(invisible(NULL))
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
	#
	return(invisible(NULL))
} # EOF

show_fdmat <- function(object) {
	#
	txtGaAdd <- ""
	#
	reToVol <- all(unlist(lapply(object, function(x) nrow(x@eventsPerVol)))) > 0
	if (reToVol) {
		volUnit <- object[[1]]@eventsPerVol@volumeUnit
		volTxt <- paste0("Fluorescence distribution has been re-calculated to events per ", volUnit, ".\n")
	} else {
		volTxt <- "Fluorescence distribution has *NOT* been re-calculated to events per volume unit.\n"
	} # end else
	
	
	nrSamples <- nrow(object@pData)
	nrGates <- nrow(object@metadata)
	if (nrGates > 1) {txtGaAdd <- "s"}
	txt0 <- "An object of class 'fdmat' [package 'flowdex'] with 6 slots\n"
	txt1 <- paste0("containing data from ", nrSamples, " samples in ", nrGates, " gate", txtGaAdd, ".\n") 
	txt2 <- paste0("(use 'object[[1]]' to inspect the first list element containing the first fluorescence distribution)\n")
	#
	cat(txt0)
	cat(txt1)
	cat(volTxt)
	cat(txt2)
#	matShow <- object@.Data[c(1,2,nr-1, nr), c(1:3, nc-2, nc-1, nc)]
#	print(matShow)
#	cat(txt2)
	#
	cat("\n\nSlot 'metadata':\n")
	print(object@metadata)

	cat("\n\nSlot 'cyTags':\n")
	print(object@cyTags)

#	cat("\n\nSlot 'eventsPerVol':\n")
#	print(object@eventsPerVol)
	
	cat("\n\nSlot 'gateStrat':\n")
	print(object@gateStrat)
	
	cat("\n\nSlot 'pData':\n")
	print(object@pData)
	
	cat("\n\nSlot 'note':\n")
	print(object@note)
	#
	return(invisible(NULL))
} # EOF

#	plot_gateStructure <- function(x, y, ...) {  
#		flowWorkspace::plot(x)
#	} # EOF


show_fdmat_single <- function(object) {

	nC <- ncol(object)
	nSa <- nrow(object)
	cns <- colnames(object)
	from <- cns[1]
	to <- cns[length(cns)]
	#
	txt0 <- "An object of class 'fdmat_single'\n"
	txt1 <- paste0("containing data from ", nSa, " samples in ", nC, " fluorescence intensities from ", from, " to ", to, "\n")
	txt11 <- paste0("derived from gate '", object@gateName, "'.\n")
	txt2 <- "(showing only the first and last columns and rows)\n"
	txtOveralEVPV <- "Overall data for events per volume unit:\n"
	txtNote <- paste0(object@note, " \n")
	if (nrow(object) < 5) {
		matShow <- object[, c(1,2,3, nC-2, nC-1,nC)] 
	} else {
		matShow <- object[c(1,2, nSa-1, nSa), c(1,2,3, nC-2, nC-1,nC)]
	}
	cat(txt0)
	cat(txt1)
	cat(txt11)
	cat(txtNote)	
	print(matShow)
	cat(txt2)
	cat("\n")
	cat(txtOveralEVPV)
	print(object@eventsPerVol)	
	return(invisible(NULL))
} # EOF


