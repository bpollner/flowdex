
makeCyTags_inner <- function(gs,  dictionary, stn) { # returns FALSE if the dictionary or the sampleId column in the phenoData does not exist
	# sampleIDs can be present in the tube in any order, can have different lengths; in the dictionary there can be any number of translations, but only no duplicates in the first column
	#
	# foN_dict, nameDict, dictType,
	elmSep <- stn$dD_elementSep # dividing the key-value pair
	grSep <- stn$dD_groupSep # dividing the groups
	#
	clVarPref <- stn$dD_classVarPrefix
	yVarPref <- stn$dD_numericVarPrefix
	cnSampleId <- "sampleId"
	#
	#
	pdExists <- cnSampleId %in% colnames(flowWorkspace::pData(gs)) 	# check for existence of sampleId column in the pData of the gating set
	if (!pdExists) {
		stop(paste0("Sorry, the required column 'sampleId' is not available in the present flow frames. \nMaybe there was a problem when reading in the fcs files?\nYou could retry calling 'makeGatingSet'."), call.=FALSE)
	} # end if !pdExists
	#

	
	## now everything should be good to go
	cyt <- as.character(flowWorkspace::pData(gs)[, cnSampleId])
	groupedList <- strsplit(cyt, grSep)
#		if (!all(diff(unlist(lapply(grl, length)))==0)) { # is TRUE when all elements in the list have the same length
#			stop(paste0("Sorry, it seems that not all sample IDs have the same length."), call.=FALSE)
#		}
	maxInd <- which.max(unlist(lapply(groupedList, length))) # if not all sampleIDs have the same length, get the longest
	aa <- groupedList[[maxInd]] # just take the first one, they have to have all the same length
	cnShort <- trimws(unlist(lapply(strsplit(aa, ":"), function(x) x[1]))) 	#  get the column names coded in short
	cydf <- as.data.frame(matrix(NA, nrow=(length(gs)), ncol=length(cnShort)))
	colnames(cydf) <- cnShort
	for (i in 1: length(groupedList)) {
		siTube <- groupedList[[i]]
		for (k in 1: length(siTube)) {
			elm <- unlist(strsplit(siTube[k], elmSep))
			cnS <- trimws(elm[1])
			value <- trimws(elm[2])
			cydf[i,cnS] <- value # insert the value into the data frame
		} # end for k
	} # end for i
	# now we have the class and y-variables (still as character) in a data frame; go and translate the column names using the dictionary
	dict <- dictionary # returns a data frame with the short names in the first column and the translation in the second
	cnLong <- character(length(cnShort))
	for (i in 1: length(cnShort)) {
		ind <- which(dict[,1] == cnShort[i])
		if (length(ind) == 0) {
			stop(paste0("Sorry, it seems that the designator `", cnShort[i], "` is not present in the dictionary."), call.=FALSE)
		}
		if (length(ind) > 1) {
			stop(paste0("Sorry, it seems that there is more than one translation for the designator `", cnShort[i], "`."), call.=FALSE)
		}
		cnLong[i] <- dict[ind,2] # assign the corresponding long name from the dictionary
	} # end for i
	colnames(cydf) <- cnLong
	rownames(cydf) <- rownames(flowWorkspace::pData(gs))
	# turns numbers into real numbers
	yInd <- which(grepl(paste0("^", yVarPref), cnLong)) # using a regular expression with the "^" saying that we look at the start of the string
	if (length(yInd > 0)) {
		for (i in 1: length(yInd)) {
			cydf[,yInd[i]] <- as.numeric(cydf[,yInd[i]])
		} # end for i
	} # end if
	# check for all rows NAs -- should not happen, but could if one parameter given twice in a symple id
	naInd <- which(apply(cydf, 2, function(x) all(is.na(x))))
	if (length(naInd) > 0) {
		cydf <- cydf[, -naInd]
	}
	return(cydf)
	 #
} # EOF

makeCyTags <- function(gs, dictionary, stn) {
	clVarPref <- "C_"
	gateNameChar <- "gate"
	#
	cyt <- makeCyTags_inner(gs, dictionary, stn)
	gsdf <- gs@gateStrat # the data frame with the gating strategy
	gateNames <- gsdf[,"GateName"]
	gateNameDF <- data.frame(rep(gateNames, each=nrow(cyt)))
	colnames(gateNameDF) <- paste0(clVarPref, gateNameChar)
	cytMult <- NULL
	origRowNames <- rownames(cyt)
	for (i in 1: nrow(gsdf)) {
		gateName <- gsdf[i, "GateName"]
		rNames <- paste0(origRowNames, "|", gateName)
		rownames(cyt) <- rNames
		cytMult <- rbind(cytMult, cyt) # is multiplying the cy-tags as often as we have rows in the gating strategy dataframe
	}
	out <- cbind(gateNameDF, cytMult)
	keepGates <- gsdf[,"GateName"][gsdf[,"keepData"]] # returns a character vector with all the GateNames we want to keep
	ind <- which(out[,1] %in% keepGates)
	out <- out[ind,]
	return(out)
} # EOF

################

getEventsPerVolume_single <- function(gs, gateName="DNA+", chName="FITC.A", volUnit="ml", apc=TRUE, coV=125) {
	volUnitTxt <- paste0("events_", volUnit)
	#
	vols <- as.numeric(as.character(flowWorkspace::pData(gs)[,"volume"])) # read the acquired volumes from the pheno data of the gating set
	fls <- flowWorkspace::gs_pop_get_data(gs, gateName) # function was "getData"
	cnsFls <- names(flowCore::markernames(fls)) # strangely, simply using 'colnames' does not work any more. Hmm.
	#
	if (! chName %in% cnsFls) {
		stop(paste0("Sorry, the channel '", chName, "' seems not to exist in the provided data."), call.=FALSE)
	}
	fluorList <- flowCore::fsApply(fls, function(x) x[,chName], use.exprs = TRUE, simplify = FALSE) # extract a single channel
	nrEvRaw <- as.numeric(unlist(lapply(fluorList, length)))
	means <- round(as.numeric(unlist(lapply(fluorList, mean))),0)
	evml <- evmlCorr <- round((nrEvRaw / vols) * 1000 * 1000, 0) ##### here calculation ######
	filtVec <- rep("FALSE", length(evml))
	gateNameVec <- rep(gateName, length(evml))
	out <- data.frame(gateNameVec, evml, means)
	primCns <- c("gate", volUnitTxt, "mean")
	colnames(out) <- primCns
	rownames(out) <- paste0(flowWorkspace::sampleNames(gs), "|", gateName)
	if (apc) {
		aa <- which(evml <= coV)
		evmlCorr[aa] <- 0
		filtVec[aa] <- "TRUE"
		out <- cbind(out, data.frame(evmlCorr), data.frame(filtVec))
		colnames(out) <- c(primCns, "events_ml.filt", "is_filtered")
	} # end if
	return(out)
} # EOF


#' @title Get Events per Volume Unit
#' @description From the data contained in the provided gating set, produce a 
#' data frame containing the events per volume unit for every gate in every 
#' single flowframe (.e. sample tube).
#' @param gs A gating set as produced by \code{\link{makeAddGatingSet}}.
#' @return A data frame.
#' @family Statistic functions
#' @export
getEventsPerVolume <- function(gs) {
	stn <- autoUpS()
	#
	volUnit <- stn$dV_volumeUnit
	apc <- stn$dV_cutoff_apply
	coV <- stn$dV_cutoff_Vol
	#
	out <- NULL
	gsdf <- gs@gateStrat
	for (i in 1: nrow(gsdf)) {
		gateName <- gsdf[i,"GateName"]
		chName <- gsdf[i,"extractOn"]
		if (gsdf[i, "keepData"]) {
			siEvml <- getEventsPerVolume_single(gs, gateName, chName, volUnit, apc, coV)
			out <- rbind(out, siEvml)
		} # end if
	} # end for i
	return(out)
} # EFO

#################
