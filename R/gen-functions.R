#' @title Perform settings setup
#' @description Perform the setup of the setting-system provided by package 
#' 'uniset'.
#' @details Has to be done only once.
#' @param path Character length one, holding the path to the location where the 
#' folder containing the settings.R file should be located. Defaults to 'NULL'. 
#' If left at the default 'NULL', the location should be selectable interactively.
#' @return No return value, is called for its side effects, i.e. to set up 
#' the settings-file system as provided by package 'uniset'.
#' @examples
#' \dontrun{
#' setupSettings(path="~/foo/bar")
#' }
#' @seealso \code{\link{fd_updateSettings}}
#' @export
setupSettings <- function(path=NULL) {
	uniset::uniset_setup(where=path, get("uniset_env_name"))
	return(invisible(NULL))
} # EOF

#' @title Manually Update the Settings 
#' @description Source the settings-list as defined in the settings.R file and 
#' create the object 'stn' in its environment. The settings-file system is 
#' based on functionality from package \code{\link[uniset]{uniset}}.
#' @param silent Logical 
#' @seealso \code{\link{setupSettings}}
#' @export
fd_updateSettings <- function(silent=FALSE) {
	return(updateSettings(silent))
} # EOF

checkPath <- function(path) {
	if (!dir.exists(path)) {
		stop(paste0("Sorry, the folder '", path, "' does not seem to exist."), call.=FALSE)
	} else {
		return(TRUE)
	}
} # EOF

createSingleFolder <- function(where, fn) {
	path <- paste0(where, "/", fn)
	if (!dir.exists(path)) {
		ok <- dir.create(path)
	} else {
		return(FALSE)
	}
	return(ok)
} # EOF

createFolders <- function(where, stn) {
	foN_gating <- stn$foN_gating
	foN_fcsFiles <- stn$foN_fcsFiles
	foN_rawData <- stn$foN_rawData
	foN_templ <- stn$foN_templates
	foN_dict <- stn$foN_dictionary
	foN_plots <- stn$foN_plots
	#
	aa <- createSingleFolder(where, foN_gating)
	bb <- createSingleFolder(where, foN_fcsFiles)
	cc <- createSingleFolder(where, foN_rawData)
	dd <- createSingleFolder(where, foN_templ)
	ee <- createSingleFolder(where, foN_dict)
	ff <- createSingleFolder(where, foN_plots)
	#
	return(aa & bb & cc & dd & ee & ff)
} # EOF

copyAllTemplates <- function(home, stn) {
	ptp <- path.package("flowdex")
	if (dir.exists(paste0(ptp, "/inst"))) {
		fromFolder <- paste0(ptp, "/inst/templates") # needed for testing
 	} else {
 		fromFolder <- paste0(ptp, "/templates")
 	} # end else
	#
	foN_templ <- stn$foN_templates
	to <- paste0(home, "/", foN_templ)
	dicFile <- paste0(fromFolder, "/dictionary.xlsx")
	gaStraF <- paste0(fromFolder, "/gateStrat.xlsx")
	#
	aa <- file.copy(dicFile, to, overwrite=TRUE)
	bb <- file.copy(gaStraF, to, overwrite=TRUE)
	#
	return(aa & bb)	
} # EOF

#' @title Generate Folder Structure
#' @description Generate the required folder structure, and possibly copy the 
#' available templates (gate definitions, gating strategy, dictionary).
#' @param copyTemplates Logical, if available templates should be copied into 
#' the folder 'templates'.
#' @param where Character length one, holding a valid path. Defaults to the 
#' current working directory.
#' @return No return value, called for its side effects, i.e. the creation of 
#' the required folder structure.
#' @examples
#' \dontrun{
#' home <- paste0(tempdir(), "/home")
#' dir.create(home)
#' genfs(home)
#' }
#' @family  Helper Functions
#' @export
genfs <- function(where=getwd(), copyTemplates=TRUE) {
	autoUpS()
	stn <- getstn()
	#
	checkPath(where)
	createFolders(where, stn)
	#
	if (copyTemplates) {
		copyAllTemplates(where, stn)
	}
	return(invisible(NULL))
} # EOF

checkConsolidateFcsFiles <- function(folderName, igTeOff=FALSE,  verbose=TRUE) {
	# Error: 'HEADER and the TEXT segment define different starting point'
	#  ignore.text.offset and using 'flowWorkspace::load_cytoframe_from_fcs' had to be introduced in FEB 2022.
	# Before that, when I was developing the code (~2017/2018), everything was good with the simple 'read.FCS' resp. 'flowCore::read.flowSet'
	# flowCore and flowWorkspace did have some updates since 2017 - so probably something was changed that made it necessary to re-save the fcs-files with the text-offset ignored.
	####
	namesOut <- NULL
	#	
	fcsNames <- list.files(folderName)
	for (i in 1: length(fcsNames)) {
		ptf <- paste0(folderName, "/", fcsNames[i])
		bana <- basename(ptf)
		siCF <- try(flowWorkspace::load_cytoframe_from_fcs(ptf, ignore.text.offset = FALSE), silent=TRUE) # this will fail if the "The HEADER and the TEXT segment define different starting point ... to read the data" is present
		if (class(siCF) == "try-error") {
			if (!igTeOff) {
 				msg1 <- paste0("Trying to read in the fcs-file '", bana, "' gives the error-message: \n", simpleMessage(siCF), "")
 				msg2 <- paste0("Consider setting the argument 'ignore.text.offset' to TRUE. \nThis will be passed on to the function 'flowWorkspace::load_cytoframe_from_fcs', the fcs-file will then be read in again with \n'The value in TEXT being ignored', \nand and the fcs-file will be re-written to disc.\n\nCAVE: With 'ignore.text.offset' set to TRUE, afflicted files in the folder \n'", folderName,"' \nwill be overwritten without further warning.")
				stop(paste0(msg1, msg2), call.=FALSE)
			} # end if !doCons
			siCF <- try(flowWorkspace::load_cytoframe_from_fcs(ptf, ignore.text.offset = TRUE), silent=TRUE)
			if (class(siCF) == "try-error") {
				stop(paste0("Sorry, reading the fcs-file '", bana, "' still did not work."), call.=FALSE)
			}
			siFF <- flowWorkspace::cytoframe_to_flowFrame(siCF)
			flowCore::write.FCS(siFF, ptf)
			namesOut <- c(namesOut, bana)
		} # end if
	} # end for i
	if (verbose & length(namesOut) != 0) {
		cat(paste0("\n\nThe following files have been re-written to disc:\n", paste(namesOut, collapse=", \n"), "\n"))
	} # end verbose
	return(NULL)
} # EOF

readInFlowSet <- function(folderName=NULL, patt=NULL, colPat=NULL, volCheck=TRUE, igTeOff=FALSE, verbose=TRUE) {
	checkConsolidateFcsFiles(folderName, igTeOff, verbose)
	#
	rawdata <- try(flowCore::read.flowSet(path = folderName, pattern=patt, column.pattern=colPat, alter.names = TRUE, name.keyword="$FIL"), silent=FALSE)
	if (class(rawdata) == "try-error") {
		stop("Sorry, an error while trying to read in the flowSet occured.", call.=TRUE)
	} # end try error
	#
	# now transform into flowCore "space" so that repairing volume and sample ID still work
	rawdata <- flowWorkspace::cytoset_to_flowSet(rawdata) # now we are back in the flowSet as produced by package "flowCore"
	#
	pdkw=list(volume="VOL", btim="$BTIM", sampleId="$SMNO") # define what to use in the pheno-data
	kw <- flowCore::keyword(rawdata, pdkw)  # "keyword" is a function in package "flowCore"; as seen on 21.04.2021 on https://support.bioconductor.org/p/p132747/#p132763
	flowWorkspace::pData(rawdata) <- as.data.frame(kw) # pData is a function in package "flowWorkspace"
	# now check for missing volume values and, if a value for val is provided, replace with that
	pDat <- flowWorkspace::pData(rawdata)
	indNA <- which(as.character(pDat[,"volume"]) == "NA")
	if (length(indNA) > 0 & volCheck) {
			if (length(indNA) == 1) {add=""} else {add="s"}
			stop(paste0("Sorry, there appear to be missing volume values in the sample", add, "\n", paste(pDat[indNA,"name"], collapse=", "), ".\nPlease use the function `repairVolumes` to repair the affected FCS files."), call.=FALSE)
	} # end length(indNA)
	return(rawdata)
} # EOF

#' @title Repair Volume Values in FCS files
#' @description Read in all or a subset of FCS files, replace missing volume
#' values in the description of the single flowFrames with a provided default
#' value and write the FCS-files back to disc.
#' @details This function is intended to be used when, for known or unknown
#' reasons, there is no volume information encoded in the original fcs file.
#' @inheritParams flowdexit
#' @param includeAll Logical. If left at the default \code{FALSE}, only those
#' FCS files with *missing* volume information are read in and the value provided
#' in \code{vol} will be assigned to them. If changed to \code{TRUE}, also FCS
#' files with *present* volume information will be read in and the value provided
#' in \code{vol} will be assigned to them.
#' @param vol Numeric length one. The value that should be written into the
#' \code{$VOL} slot in the description of the single flowFrame.
#' @param confirm Logical. If the user should be asked for additional confirmation
#' before the rewriting of the fcs files is performed. Defaults to TRUE.
#' @return Nothing, resp. the modified fcs files written back into the same folder
#' and replacing the original ones.
#' @family Repair functions
#' @export
repairVolumes <- function(patt=NULL, vol=NULL, fn=".", includeAll=FALSE, confirm=TRUE, ignore.text.offset=FALSE, verbose=TRUE) {
	stn <- autoUpS()	
	#
	folderName <- checkDefToSetVal(fn, "foN_fcsFiles", "fn", stn, checkFor="char")
	#
	if (includeAll) {
		pomText <- "present or missing"
	} else {
		pomText <- "missing"
	}
	if (is.null(vol)) {
		stop(paste0("Please provide a value for `vol` to use this for all ", pomText, " volume values in the FCS files matching the provided pattern."), call.=FALSE)
	}
	if (is.null(patt)) {
		pattAdd <- paste0("all fcs files in the folder `", folderName, "`...")
	} else {
		pattAdd <- paste0("fcsFiles in folder `", folderName, "` with pattern matching `", patt, "`...")
	}
	if (verbose) {cat(paste0("Reading in ", pattAdd))}
	fs <- readInFlowSet(folderName=folderName, patt=patt, volCheck=FALSE, igTeOff=ignore.text.offset, verbose=verbose)
	if (verbose) {cat(" ok.\n")}
	pDat <- flowWorkspace::pData(fs)
	if  (includeAll) {
		indUse <- 1: nrow(pDat) # as we want to include all files in the flowframe
	} else {
		indUse <- which(as.character(pDat[,"volume"]) == "NA") # only those with missing volume information
	}
	namesUse <- ls(fs@frames)[indUse] # the frames are in an environment
	if (length(indUse) == 0) { # so there are no missing volume values
		if (verbose) {cat("All volume values are present - no re-writing of fcs files will be performed.")}
		return(invisible(NULL))
	} else {
		if (confirm) {
			cat(paste0(length(indUse), " volume values are ", pomText, " and will be replaced with the value `", vol, "` in the following files:\n", paste(namesUse, collapse=", "), "\n\nPress enter to continue or escape to abort:"))
		scan(file = "", n = 1, quiet = TRUE)
		} # end if confirm
	} # end else
	if (verbose) { cat(paste0("Re-writing volume data of ", length(namesUse), " FCS files, using `", vol, "` to replace ", pomText, " values.\n")) }
	for (i in 1: length(namesUse)) {
		flowFile <- paste0("fs@frames$", namesUse[i])
		txt <- paste0(flowFile, "@description$VOL <- ", vol)
		eval(parse(text=txt)) # here write the provided volume into the description of a single flowFrame
#		ffn <- paste0(folderName,"/",namesUse[i], ".fcs") # the name of folder and file
		ffn <- paste0(folderName,"/",namesUse[i], "") # the name of folder and file
		options(warn=-1)
		txt <- paste0("flowCore::write.FCS(", flowFile, ", \"", ffn, "\")")
		eval(parse(text=txt)) # write the single flowFrame with corrected Volume back to file
		options(warn=0)
		cat(".")
	} # end for i
	cat(" ok.\n")
	return(invisible(NULL))
} # EOF

#' @title Repair a single Sample ID.
#' @description Replace a faulty sample ID with a new one and write the single
#' fcs file back to disc.
#' @details To first obtain the flowSet, leave the parameter `fs`at its default
#' NULL. By providing a pattern to `patt`, subgroups of fcs files can be read in.
#' Provide the so obtained flowSet to the parameter `fs`, and specify the
#' name and the new sample ID in order to re-write the specified file with its
#' new Sample ID. 'object@phenoData@data' can be used to inspect and verify names 
#' of FCS files and the sample IDs therein -- see examples.
#' @section Note: The sample ID, esp. the correct sample ID is of importance 
#' when using the 'dictionary' to expand the abbreviations in the sample ID - 
#' see XXX.
#' @param fs The object returned by this function if parameter \code{fs} is left
#' at its default NULL, what then can be used as input for the parameter \code{fs}.
#' @param name Character length one. The name of the fcs file within the flowset 
#' that should get a new sample ID.
#' @param newSID Character legnth one. The new Sample ID.
#' @inheritParams flowdexit
#' @param confirm Logical. If the user should be asked for additional confirmation
#' before the rewriting of the fcs file is performed. Defaults to TRUE.
#' @return Nothing, called for its side effects: the specified single fcs file gets
#' written to disc with its new sample ID.
#' @family Repair functions
#' @examples
#' \dontrun{
#' fs <- repairSID() # or 
#' fs <- repairSID(patt="foo")
#' fs@phenoData@data
#' repairSID(fs, "sample1", "newSID")
#' }
#' @export
repairSID <- function(fs=NULL, name=NULL, newSID=NULL, patt=NULL, fn=".", confirm=TRUE, ignore.text.offset=FALSE) {
	stn <- autoUpS()	
	#
	fn <- checkDefToSetVal(fn, "foN_fcsFiles", "fn", stn, checkFor="char")
	#	
	if (is.null(fs)) {
		return(invisible(readInFlowSet(folderName=fn, patt=patt, igTeOff=ignore.text.offset)))
	}
	if (is.null(name) | is.null(newSID)) {
		stop("Please provide a value to `name` and `newSID`.", call.=FALSE)
	}
	if (! name %in% ls(fs@frames)) {
		stop(paste0("Sorry, the fcs file `", name, "` seems not to be present in the provided flowSet."), call.=FALSE)
	}
	if (confirm) {
		cat(paste0("The new sample ID of the fcs-file `", name, "` will be:\n", newSID, "\n\nPress enter to continue or escape to abort:"))
		scan(file = "", n = 1, quiet = TRUE)
	}
	flowFile <- paste0("fs@frames$", name)
	txt <- paste0(flowFile, "@description$`$SMNO` <- \"", newSID, "\"")
	eval(parse(text=txt)) # write the new sample ID into the single flowFrame.
	ffn <- paste0(fn,"/", name, "") # the name of folder and file
	options(warn=-1)
	txt <- paste0("flowCore::write.FCS(", flowFile, ", \"", ffn, "\")")
	eval(parse(text=txt)) # write the single flowFrame with corrected sample ID back to file
	options(warn=0)
	if (TRUE) {cat(paste0("`", name, "` has been rewritten with the modified sample ID.\n"))}
	return(invisible(NULL))
} # EOF

#' @title Make Gating Set
#' @description Read in FCS files and put them together in a gating set.
#' @details If no folder name is specified, FCS files are being read in from the 
#' default FCS-files folder. 
#' @inheritParams flowdexit
#' @section Regarding Compensation: Due to the circumstances when developing 
#' this code, it was never required to apply any kind of compensation. The 
#' functionality to apply compensation was therefore never tested or verified. 
#' It is strongly advised to use caution when applying compensation. It might 
#' well be necessary to modify the source code of this function 
#'(fork from \url{https://github.com/bpollner/flowdex}) in order to achieve 
#' correct compensation results.
#' @return A gating set produced by \code{\link[flowWorkspace]{GatingSet}}.
#' @family Extraction functions
#' @export
makeGatingSet <- function(patt=NULL, comp=".", fn=".", tx=".", channel=".", ignore.text.offset=FALSE, verbose=".") {
	stn <- autoUpS()		
	#
	comp <- checkDefToSetVal(comp, "dV_comp", "comp", stn, checkFor="logi")
	fn <- checkDefToSetVal(fn, "foN_fcsFiles", "fn", stn, checkFor="char")
	tx <- checkDefToSetVal(tx, "dV_tx", "tx", stn, checkFor="char")
	channel <- checkDefToSetVal(channel, "dV_channel", "channel", stn, checkFor="char")
	verbose <- checkDefToSetVal(verbose, "dV_verbose", "verbose", stn, checkFor="logi")
	#
	if (verbose) {cat("Reading in fcs files... ")}
	rawdata <- readInFlowSet(folderName=fn, patt=patt, colPat=channel, igTeOff=ignore.text.offset, verbose)
	if (verbose) {cat("ok. \n")}
	if (verbose) {cat("Producing gating set... ")}
	gs <- flowWorkspace::GatingSet(rawdata) 
	if (comp) {  # first compensate, then flowJoBiexpTrans
		if (verbose) {cat("Applying compensation matrix... ")}
		compMat <- flowCore::compensation(flowCore::spillover(rawdata[[1]]))
		gs <- flowWorkspace::compensate(gs, compMat) #  !!! compensation has not been tested. I simply did not have the required dataset for that. And it never was required to apply compensation in my case. Sorry everybody for any inconvenience....
		if (verbose) {cat("maybe ok. (!! see documentation for ?makeGatingSet !!)\n")}
	}
	if (verbose) {cat(paste0("Applying ", tx, " transformation... "))}
	fiNa <- ls(rawdata@frames)[1] # take the first frame in the rawdata. It has to contain at least one.
	txt <- paste0("colnames(rawdata@frames$", fiNa, "@exprs)") # extract the colnames. Because for the gating set, that does not work here within the function (?)
	cns <- eval(parse(text=txt))  
#	biexpTFL <- flowWorkspace::transformerList(colnames(gs), flowWorkspace::flowjo_biexp_trans()) ## that did work before? now not any more ???
	biexpTFL <- flowWorkspace::transformerList(cns, flowWorkspace::flowjo_biexp_trans())
	gs <- flowWorkspace::transform(gs, biexpTFL)
	if (verbose) {cat("ok. \n")}
	return(gs)
} # EOF

importCheckGatingStrategy <- function(fiN_gateStrat, stn, gsType=".", foName=".") {
	cnsReq <- gl_requiredGateStratColnames
	#
	gsType <- checkDefToSetVal(gsType, "dV_gateStratInputType", "dV_gateStratInputType (settings.R)", stn, checkFor="char")
	foN_gating <- checkDefToSetVal(foName, "foN_gating", "foN_gating (settings.R)", stn, checkFor="char")
	#
	typE <- NULL
	if (gsType == "csv") {
		typE <- ".csv"
	}
	if (gsType == "xlsx") {
		typE <- ".xlsx"
	}
	if (is.null(typE)) {
		stop("Please provide either 'csv' or 'xlsx' as preferred input type for the gating-strategy file (settings.R file key name 'dV_gateStratInputType')", call.=FALSE)
	}
	checkFileExistence(foN_gating, fiN_gateStrat, typE, addTxt="gating strategy file ")
	gateStrat <- loadGaXFile(foN_gating, fiN_gateStrat, gsType)
	cns <- sort(colnames(gateStrat))
	if (!identical(cns, sort(cnsReq))) {
		stop(paste0("Sorry, the provided gating strategy file '", fiN_gateStrat, "' does not contain the required column names.\nPlease see the template for an example.\nThe required column names are:\n'", paste(cnsReq, collapse="', '"), "'."), call.=FALSE)
	} # end if
	return(new("gatingStrategy_fd", gateStrat, filename=paste0(fiN_gateStrat, typE)))	
} # EOF

#' @title Add Polygon Gates
#' @description Load the predefined gating strategy (as .csv or .xlsx file) and 
#' apply the gates as defined in the file.
#' @details The gating strategy file can hold one or more gates. One row in the 
#' files represents one gate. In order to see a schematic representation of 
#' 'parent' and 'child' gates, simply use 'plot'.
#' @param gs A gating set as produced by \code{\link{makeGatingSet}}.
#' @inheritParams flowdexit
#' @return A gating set.
#' @examples
#' \dontrun{
#' gs <- addGates(gs)
#' plot(gs)
#' gs <- addGates(gs, gateStrat="fooBar.csv") # the use a different then the 
#' # default gating strategy
#' plot(gs)
#' }
#' @family Extraction functions
#' @export
addGates <- function(gs, gateStrat=".", foN.gateStrat=".", type.gateStrat=".", verbose=".") {
	stn <- autoUpS()		
	#
	gateStrat <- checkDefToSetVal(gateStrat, "fiN_gateStrat", "gateStrat", stn, checkFor="char")
	foN_gating <- checkDefToSetVal(foN.gateStrat, "foN_gating", "foN_gating (settings.R)", stn, checkFor="char")
	gsType <- checkDefToSetVal(type.gateStrat, "dV_gateStratInputType", "type.gateStrat", stn, checkFor="char")
	verbose <- checkDefToSetVal(verbose, "dV_verbose", "verbose", stn, checkFor="logi")
	#
	gsdf <- importCheckGatingStrategy(gateStrat, stn, gsType, foN_gating)
	checkPggExistence(gsdf, foN_gating, gateStrat)
	#
	nlAdd <- " "
	gtNoun <- "gate"
	nrGates <- nrow(gsdf)
	if (nrGates > 1) {
		nlAdd <- "\n"
		gtNoun <- "gates"
	}
	if (verbose) {cat(paste0("Gating: (", nrGates, " ", gtNoun, ")", nlAdd)) }
	for (i in 1: nrow(gsdf)) {
		gateOn <- c(gsdf[i,"GateOnX"], gsdf[i,"GateOnY"]) # extract the x and y channels from the i-th row
		pggName <- gsdf[i,"GateDefinition"]
		gateMat <- loadGaXFile(foN_gating, pggName, type="pgg")		
		names(gateMat) <- gateOn
		pgg <- flowCore::polygonGate(.gate=gateMat, filterId=pggName)
		erm <- try(flowWorkspace::gs_pop_add(gs, pgg, parent=gsdf[i,"Parent"], name=gsdf[i, "GateName"]), silent=TRUE) # gs_pop_add is in flowWorkspace
		if (class(erm) == "try-error") {
			msgTxt <- paste0("The gate '", gsdf[i, "GateName"], "' already contains the gate as defined in '", gsdf[i, "GateDefinition"], "'.")
			message(msgTxt)
		} # end if try error
	} # end for i
	flowWorkspace::recompute(gs)
	out <- new("GatingSet_fd", gs, gateStrat=gsdf) # the gating strategy into the slot
	return(out)
} # EOF

#' @title Make Gating Set and Add Gates
#' @description Read in FCS files, put them together in a gating set and apply 
#' the gating strategy as previously defined.
#' @details This is a convenience wrapper for the two functions 
#' \code{\link{makeGatingSet}} and \code{\link{addGates}}.
#' @inheritParams flowdexit
#' @return A gating set with added and recomputed gates.
#' @examples
#' \dontrun{
#' gs <-  makeAddGatingSet()
#' plot(gs)
#' }
#' @family Extraction functions
#' @export
makeAddGatingSet <- function(patt=NULL, fn=".", gateStrat=".", foN.gateStrat=".", type.gateStrat=".", comp=".", tx=".", channel=".", ignore.text.offset=FALSE, verbose=".") {
	stn <- autoUpS()
	#
	fn <- checkDefToSetVal(fn, "foN_fcsFiles", "fn", stn, checkFor="char")
	gateStrat <- checkDefToSetVal(gateStrat, "fiN_gateStrat", "gateStrat", stn, checkFor="char")
	foN_gating <- checkDefToSetVal(foN.gateStrat, "foN_gating", "foN.gateStrat", stn, checkFor="char")
	gsType <- checkDefToSetVal(type.gateStrat, "dV_gateStratInputType", "type.gateStrat", stn, checkFor="char")
	comp <- checkDefToSetVal(comp, "dV_comp", "comp", stn, checkFor="logi")
	tx <- checkDefToSetVal(tx, "dV_tx", "tx", stn, checkFor="char")
	channel <- checkDefToSetVal(channel, "dV_channel", "channel", stn, checkFor="char")
	verbose <- checkDefToSetVal(verbose, "dV_verbose", "verbose", stn, checkFor="logi")
	#
	gsdf <- importCheckGatingStrategy(gateStrat, stn, gsType, foN_gating)
	checkPggExistence(gsdf, foN_gating, gateStrat)
	gs <- makeGatingSet(patt, comp, fn, tx, channel, ignore.text.offset, verbose)
	gs <- addGates(gs, gateStrat, foN_gating, gsType, verbose)
	return(gs)
} # EOF

#' @title Manually Draw Polygon Gate
#' @description Produce a simple x~y dotplot on the specified channels that is 
#' used with \code{locator} to define the gate boundaries, similar to defining 
#' a polygon gate in standard FCM GUIs. The resulting data are saved as an 
#' R-object under the name specified in \code{pggId}. Press 'esc' when done 
#' drawing the gate.
#' @details The generated R-object is saved automatically and can be used as a 
#' gate-definition in the gating strategy. 
#' For the lines to be drawn while the locator points 
#' are clicked, it is recommended to use this function NOT within R-Studio. 
#' The sample names within the gating set can be obtained via 'show' - see 
#' examples.
#' @inheritParams flowdexit
#' @param gs A gating set as produced by \code{\link{makeGatingSet}} or 
#' \code{\link{makeAddGatingSet}}.
#' @param flf Character or numeric length one. The identifier of the flowframe 
#' within the gating set where the gate should be drawn on. Optimally, this is 
#' a flowframe, i.e. sample, with a very good representation of the desired 
#' population. Possible input values can be determined via 'show'.
#' @param gn Character length one. The name of a gate further specifying the 
#' desired subset of data; defaults to "root".
#' @param pggId Character length one. The name of the resulting file containing 
#' the boundaries of the gate. If left at the default '.', the name as defined 
#' in the settings file (key: 'fiN_gate') will be used. 
#' @param channels Character length two. The channels the gate should be defined 
#' in. If left at the default '.', the two channels as defined in the settings 
#' file (key: 'dV_channelsForPGG') will be used. Available channels can be 
#' viewed via 'show' - see examples. 
#' @param useLoc Logical. If, after plotting, the locator should be used. 
#' Defaults to TRUE. 
#' @param locN Numeric length one. How many points to acquire in the locator. 
#' Defaults to 512; use "ESC" to abort the locator action.
#' @param bnd NULL or numeric length four. The boundaries to be marked on the 
#' plot, format: (x1, x2, y1, y2). If values are provided, two straight lines 
#' on the x-axis and two on the y-axis will be drawn.
#' @param showGate Character length one. The name of an already existing gate 
#' residing in the folder specified by 'foN.gateDefs'. If provided, this gate 
#' will be additionally drawn on the dotplot. This can be helpful when e.g. 
#' on old, sub-optimal gate should be replaced with a new one: In this case the 
#' name of the 'old' would be provided at 'showGate' with the same name 
#' specified at 'ppfId'. Thus, the old gate will be replaced with the new one.
#' @section Warning: Existing locator matrix files with the same name will be 
#' overwritten without asking!
#' @return A list with the locator coordinates resp. this data saved as an 
#' R-object in the folder specified at 'foN.gateStrat'.
#' @examples
#' \dontrun{
#' gs <- makeGatingSet()
#' gs # same as show(gs)
#' pgg <- drawGate(gs, 1)
#' drawGate(gs, 1, showGate="polyGate") # do show a previously defined gate
#' }
#' @family Plotting functions
#' @export
drawGate <- function(gs, flf=NULL, gn="root", pggId=".", channels=".", foN.gateStrat=".", useLoc=TRUE, locN=512, bnd=".", showGate=NULL) {
	stn <- autoUpS()
	#
	pggColor <- stn$dG_locatorLine
	pggLwd <- stn$dG_locatorLineWidth	
	pggShowColor <- stn$dG_gateShowColor
	#
#	checkObjClass(object=gs, "GatingSet", argName="gs") 
	foN_gating <- checkDefToSetVal(foN.gateStrat, "foN_gating", "foN.gateDefs", stn, checkFor="char")
	pggId <- checkDefToSetVal(pggId, "fiN_gate", "pggId", stn, checkFor="char")
	showGate <- checkDefToSetVal(showGate, "..x..", "showGate", stn, checkFor="charNull", defValue=NULL)
	channels <- checkDefToSetVal(channels, "dV_channelsForPGG", "channels", stn, checkFor="char", len=2)
	bnd <- checkDefToSetVal(bnd, "dV_channelBoundaries", "bnd", stn, checkFor="numNull", len=4)
	#
	if (is.null(flf)) {
		stop("Please provide either a name or an index to specify on which flowframe the gate should be drawn.\nUse `gsinfo()` to see possible values.", call.=FALSE)
	}
	datMat <- flowCore::fsApply(flowWorkspace::gs_pop_get_data(gs[flf], gn), function(x) x[, channels], use.exprs=TRUE)
	datMat <- cleanUpInfinites(datMat)
	saName <- flf
	if (is.numeric(flf)) {
		saName <- flowWorkspace::sampleNames(gs)[flf]
	}
	#
	if (is.null(bnd)) {
		xMax <- max(datMat[,1])
		yMax <- max(datMat[,2])
	} else {
		yMax <- bnd[4] + ((bnd[4]/100)*10)
		xMax <- bnd[2] + ((bnd[2]/100)*10)
 	} # end else
	#
	subTxtShow <- subTxtSave <- subJoin <- NULL
	tiTxt <- paste0("Drawing on gate '", gn, "',\n using data from '", saName, "'.")
	if (!is.null(showGate)) {
		subTxtShow <- paste0("Showing gate '", showGate, "'")
	} # end if
	if (useLoc) {
		subTxtSave <- paste0("New gate will be saved under '", pggId, "'")
		subJoin <- "; "
	} # end if
	#
	subTxt <- paste0(subTxtShow, subJoin, subTxtSave)
	plot(datMat[,1], datMat[,2], type="p", ylim=c(0, yMax), xlim=c(0, xMax), xlab=channels[1], ylab=channels[2], main=tiTxt, sub=subTxt)
	if (!is.null(bnd)) {
		abline(h=bnd[3], col="red")
		abline(h=bnd[4], col="red")
		abline(v=bnd[1], col="blue")
		abline(v=bnd[2], col="red")
	} # end if
	#
	if (!is.null(showGate)) {
		checkPggExistence(showGate, foN_gating) 
		fipa <- paste0(foN_gating, "/", showGate)
		pggShow <- eval(parse(text=load(fipa)))
		checkShowGateChannels(pggShow, datMat)
		lines(x=pggShow[[1]], y=pggShow[[2]], col=pggShowColor)
	} # end !is.null(showGate)
	locMat <- NULL
	if (useLoc) {
		if (!dir.exists(foN_gating)) {
			stop(paste0("Sorry, the destination folder `", foN_gating, "` for the polygon gate data does not seem to exist."), call.=FALSE)
		}
		locMat <- getLocMat_TS(locN) # here the locator waits for user input; use ESC to stop input
		if (class(locMat) == "try-error") {
			stop("Sorry, an error occurred while trying to use the locator.", call.=FALSE)
		}
		if (length(locMat[[1]]) < 3) {
			stop(paste0("Sorry, you need to click at least three points to define a polygon gate."), call.=FALSE)
		} # end if
		locMat <- lapply(locMat, function(x) round(x, 0))
		locMat$x[length(locMat$x)+1] <- locMat$x[1] # close the circle (to be sure)
		locMat$y[length(locMat$y)+1] <- locMat$y[1]
		names(locMat) <- channels
		lines(locMat[[1]], locMat[[2]], col=pggColor, lwd=pggLwd)
		save(locMat, file=paste0(foN_gating, "/", pggId))
	}
	return(invisible(locMat))
} # EOF

#' @title Extract Fluorescence Distribution Matrix
#' @description Extract fluorescence distribution along a specified channel 
#' from the gating set as defined in the gating strategy file and re-calculate 
#' data to events per volume. 
#' @param gs A gating set as produced by \code{\link{makeAddGatingSet}}.
#' @param dev Logical. If set to true, a histogram showing the bins and the
#' smoothed mid-points is plotted. (Only intended for development.) Defaults to
#' FALSE.
#' @inheritParams flowdexit
#' @return A matrix of class "fdmat" with one row for every FCS file in the 
#' gating set.
#' @examples
#' \dontrun{
#' XXX
#' }
#' @seealso \code{\link{makeAddGatingSet}}, \code{\link{flowdexit}}
#' @family Extraction functions
#' @export
makefdmat <- function(gs, expo.gate=".", expo=TRUE, expo.type=".", name.dict=".", foN.dict=".", type.dict=".", verbose=".", dev=FALSE) {
	#
	stn <- autoUpS()
	#
	outMat <- outMd <- res <- apc <- coR <- coV <- rcv <- igp <- smo <- smN <- smP <- chPrevWl <- volFac <- dictionary <- useDic <- cyTags <- NULL # some get assigned below
	assignHereStnValues(stn)
	#
	expoType <- checkDefToSetVal(expo.type, "dE_exportType", "expo.type", stn, checkFor="char")
	expoGate <- checkDefToSetVal(expo.gate, "dE_exportGate", "expo.gate", stn, checkFor="charNull")
	nameDict <- checkDefToSetVal(name.dict, "dD_dict_name", "name.dict", stn, checkFor="char")
	foN_dict <- checkDefToSetVal(foN.dict, "foN_dictionary", "foN.dict", stn, checkFor="char")
	verbose <- checkDefToSetVal(verbose, "dV_verbose", "verbose", stn, checkFor="logi")
	dictType <- checkDefToSetVal(type.dict, "dV_dictionaryType", "type.dict", stn, checkFor="char")
	dictTypeE <- paste0(".", dictType)
	#
	checkObjClass(object=gs, "GatingSet_fd", argName="gs") 
	checkForVolumeData(gs)
	#
	eventsPerVol <- getEventsPerVolume(gs)
	#
	if (useDic) {
		checkFileExistence(foN_dict, nameDict, dictTypeE, addTxt="dictionary file ")
		dictionary <- loadGaXFile(foN_dict, nameDict, dictType)
		cyTags <- makeCyTags(gs, dictionary, stn) # extract from the sampleId column in the pheno Data; returns FALSE if either the dictionary or the sampleId column from the single tubes is not present
	} # end if
	#
	#	
	gsdf <- gs@gateStrat
	for (i in 1: nrow(gsdf)) {
		gateName <- gsdf[i,"GateName"]
		chName <- gsdf[i,"extractOn"]
		gateDef <- gsdf[i,"GateDefinition"]
		flRange <- c(gsdf[i,"minRange"], gsdf[i,"maxRange"])
		aa <- makefdmat_single(gs, gateName, chName, res, flRange, apc, coR, coV, rcv, igp, smo, smN, smP, chPrevWl, gateDef, dev, volFac, verbose)
		if (gsdf[i, "keepData"]) {
			outMat <- rbind(outMat, aa$mat)
			outMd <- rbind(outMd, aa$md)
		} # end if
	} # end for i
	#
	mat <- new("fdmat", outMat, metadata=outMd, pData=flowWorkspace::pData(gs), eventsPerVol=eventsPerVol, cyTags=cyTags, gateStrat=gs@gateStrat, note="original")
	#
#	if (expo) {
		# XLSgate <- expo.gate 	### weg
		# toXls <- expo			### weg
#		exportMatrixToXls(cutfdmatToGate(mat, XLSgate), rcv)
#	}
	return(mat)
} # EOF

#' @title Plot Gates on all flowframes in a gating set
#' @description Plot all available gates on all flowframes in a gating set and 
#' add layers for the number of events. (In raw format, i.e. *not* 
#' re-calculated to volume!)
#' @details Plotting is performed by the function \code{\link[ggcyto]{ggcyto}}. 
#' If a gating set without applied gates is provided to the first argument, 
#' parameters \code{plotAll}, \code{spl} and \code{toPdf} do not apply.
#' @param gs A gating set.
#' @param ti Character length one, a possible character added to the title of 			
#' the gate-plot.
#' @param plotAll Logical. If left at the default \code{FALSE}, only the gates 
#' where the parameter \code{keepData} in the gating strategy is set to 
#' \code{TRUE} are plotted. If set to \code{TRUE}, all gates within the gating 
#' strategy file will be plotted.
#' @param spl Character length one. The name of the column in the cyTags that 
#' should be used to split by before plotting. If left at the default 
#' \code{NULL}, no splitting is performed. Possible values for 'spl' are the 
#' column names of the cyTags saved in the object of class 'fdmat' as produced 
#' by \code{\link{makefdmat}}.
#' @param toPdf Logical. If the plots should be saved in a pdf. Defaults to 
#' TRUE
#' @param fns Character length one. The filename suffix of the possible pdf.
#' @param x Character length one. The name of channel where data was acquired to 
#' be displayed on the x-axis. Only applies if a gating set without applied gate 
#' is provided to the argument \code{gs}.
#' @param y Character length one. The name of channel where data was acquired to 
#' be displayed on the y-axis. Only applies if a gating set without applied gate 
#' is provided to the argument \code{gs}.
#' @param foN.plots Character length one. The name of the folder where possible 
#' PDFs should be saved in. If left at the default '.', the value as defined in 
#' the settings file (key 'foN_plots') will be used. 
#' @inheritParams flowdexit
#' @examples
#' \dontrun{
#' gs <- makeAddGatingSet()
#' plotgates(gs)
#' #
#' fdm <- makefdmat(gs)
#' fdm@cyTags # look at column names for splitting
#' plotgates(gs, spl="fooBar") # to split the plot by values contained in column 
#' # 'fooBar'.
#' }
#' @family Plotting functions
#' @export
plotgates <- function(gs, ti="", spl=NULL, fns=NULL, plotAll=FALSE, toPdf=TRUE, x=NULL, y=NULL, name.dict=".", foN.dict=".", type.dict=".", foN.plots=".") {
	stn <- autoUpS()
	#
	foN_plots <- ""
	bins <- stn$dg_nrBins
	pdfHeight <- stn$dG_pdf_height
	pdfWidth <- stn$pdfWidth
	#
	if (! class(gs) %in% c("GatingSet_fd", "GatingSet")) {
		stop("Please provide a gating set to the argument 'gs'.", call.=FALSE)
	}
	if (toPdf) {
		foN_plots <- checkDefToSetVal(foN.plots, "foN_plots", "foN.plots", stn, checkFor="char")
	} # end if
	##
	if (class(gs) == "GatingSet") {
		if (is.null(x) | is.null(y)) {
			stop("Please provide valid channel names to be displayed on the x- and y-axis.", call.=FALSE)
		}
		tiUse <- paste0(ti, "   root (no gates added)")
		plot(ggcyto::ggcyto(gs, subset="root", ggplot2::aes_(x=x, y=y)) + ggplot2::ggtitle(tiUse) + ggplot2::geom_hex(bins=bins) + ggcyto::ggcyto_par_set(limits="instrument"))
		return(invisible(NULL))
	} # end if class(gs) == "GatingSet"
	##
	gsdf <- gs@gateStrat
	gateStrat <- gs@gateStrat@filename
	tiAdd <- "  |  "
	txtAdd <- suffixAdd <- ""
	#
	# prepare for possible splitting: make cyTags
	if (!is.null(spl)) { # so we want to split. Therefore we need to have cyTags, something only made for the fdmat.
		# first check. We do not check earlier, because if we do not want to split, it is irrelevant
		nameDict <- checkDefToSetVal(name.dict, "dD_dict_name", "name.dict", stn, checkFor="char")
		foN_dict <- checkDefToSetVal(foN.dict, "foN_dictionary", "foN.dict", stn, checkFor="char")
		dictType <- checkDefToSetVal(type.dict, "dV_dictionaryType", "type.dict", stn, checkFor="char")
		dictTypeE <- paste0(".", dictType)
		#
		checkFileExistence(foN_dict, nameDict, dictTypeE, addTxt="dictionary file ")
		dictionary <- loadGaXFile(foN_dict, nameDict, dictType)
		cyTags <- makeCyTags(gs, dictionary, stn) # extract from the sampleId column in the pheno Data; returns FALSE if either the dictionary or the sampleId column from the single tubes 
		#
		if (! spl %in% colnames(cyTags)) {
			stop(paste0("Sorry, the provided split column `", spl, "` is not present in the provided gating set resp. its cyTags."), call.=FALSE)
		}
		txtAdd <- paste(" split by", spl)
		suffixAdd <- paste0("_by",spl)
	} # end if
	#
	if (toPdf) {cat(paste0("Plotting gates", txtAdd, "... \n"))}
	height <- pdfHeight
	width <- pdfWidth
	suffix <- paste0("Gates_", gateStrat, suffixAdd)
#	filename <- paste(expName, suffix, sep="_")
	filename <- suffix
	filename <- paste(foN_plots, "/", filename, fns, ".pdf", sep="")
	if (toPdf) { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
#	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	for (i in 1: nrow(gsdf)) {
		xax <- gsdf[i,"GateOnX"]
		yax <- gsdf[i,"GateOnY"]
		gateName <- gsdf[i,"GateName"]
		subs <- flowWorkspace::gs_pop_get_parent(gs, gateName) # get the name of the parent node
		tiUse <- paste0(ti, tiAdd, subs, ", gate: ", gateName, ", using `", gsdf[i,"GateDefinition"], "`")
		# !! use "aes_" !!
		if (plotAll | gsdf[i,"keepData"]) {
			if (!is.null(spl)) {
		#		cyTagsUse <- cyTags[which(cyTags[,1] == gateName),]
				cyTagsUse <- cyTags[1:length(gs),] # we just take the first gate, as all the indices are the same in all of the gates in the cyTags
				splVals <- sort(unique(cyTagsUse[,spl]))
				for (k in 1: length(splVals)) {
					indsUse <- which(cyTagsUse[,spl] == splVals[k])
					tiUse <- paste0(ti, " ", splVals[k], tiAdd, subs, ", gate: ", gateName, ", using `", gsdf[i,"GateDefinition"], "`")
					options(warn=-1)
					plot(ggcyto::ggcyto(gs[indsUse], subset=subs, ggplot2::aes_(x=xax, y=yax)) + ggplot2::ggtitle(tiUse) + ggplot2::geom_hex(bins=bins) +  ggcyto::geom_gate(gateName) + ggcyto::geom_stats(gateName, type="count") + ggcyto::ggcyto_par_set(limits="instrument"))
					options(warn=0)
					cat(".")
				} # end for k
			} else { # so spl is null and we do not split
				options(warn=-1)
				plot(ggcyto::ggcyto(gs, subset=subs, ggplot2::aes_(x=xax, y=yax)) + ggplot2::ggtitle(tiUse) + ggplot2::geom_hex(bins=bins) +  ggcyto::geom_gate(gateName) + ggcyto::geom_stats(gateName, type="count") + ggcyto::ggcyto_par_set(limits="instrument"))
				options(warn=0)				
			} # end else
		} # end if (plotAll | gsdf[i,"keepData"])
		#
		cat(".")
	} # end for i (nrow(gsdf))
	#
	if (toPdf) {
		dev.off()
		cat("ok.\n")
	} # end if
	return(invisible(NULL))
} # EOF


#### explain calculation 
# evml <- evmlOrig <- round((nrEvRaw / vols) * volFac , 0)

#' @title Read in FCS Files and Extract Data
#' @description XXX
#' @details While function 'flowdexit' returns fluorescence distributions 
#' re-calculated to events per volume unit, the gating set that was produced 
#' in the way of obtaining the fluorescence distribution data getas assigned 
#' to the environment 'gsenv' under the name 'gatingSet'. Hence, it can be 
#' accessed via 'gsenv$gatingSet'.
#' @param fn Character length one. The name of the folder where FCS files should 
#' be read from. If left at the default '.', the folder name as defined in the 
#' settings file (key: 'foN_fcsFiles') will be used.
#' @param patt A regular expression defining a possible subset of FCS files
#' residing in the directory specified by \code{fn} to read in. Only matching
#' patterns will be included.
#' @param gateStrat Character length one. The name of the file defining the 
#' gating strategy. If left at the default '.', the name as defined in the 
#' settings file (key: 'fiN_gateStrat') will be used. 
#' @param foN.gateStrat Character length one. The name of the folder where the 
#' file defining the gating strategy and the gate definitions reside. If left 
#' at the default '.', the name as defined in the settings file 
#' (key: 'foN_gating') will be used. 
#' @param type.gateStrat Character length one, can be either 'csv' or 'xlsx'. 
## The type of file defining the gating strategy. Currently, csv and xlsx 
#' files are supported. If left at the default '.', the filetype as defined in 
#' the settings (key: 'dV_gateStratInputType') file will be used. 
#' @param comp Logical. If compensation should be applied or not. If left at 
#' the default '.', the value as defined in the settings file (key 'dV_comp') 
#' will be used.
#' @param tx Character length one. The transformation applied to *all* channels 
#' within the individual flow sets. If left at the default '.', the value as 
#' defined in the settings file (key 'dV_tx') will be used. (Currently only 
#' 'fjbiex' is implemented.)
#' @param verbose Logical. If status messages should be displayed. If left at 
#' the default '.', the value as defined in the settings file (key 'dV_verbose') 
#' will be used.
#' @param channel A regular expression indicating which channels, i.e. which
#' columns to keep from the original flowframes; is passed down to argument
#' \code{column.pattern} of \code{\link[flowCore]{read.flowSet}}. Set to NULL
#' to read data from all channels. If left at the default '.', the value as 
#' defined in the settings file (key 'dV_channel') will be used.
#' @param ignore.text.offset Logical. If set to true, fcs-files in the folder 
#' specified at argument 'fn' will be checked for inconsistencies in the 
#' HEADER and TEXT segment and, if those inconsistency is present, the 
#' afflicted fcs-file will be re-written to disc, with the values in TEXT 
#' being ignored. The file will be \strong{overwritten without further warning.}
#' @param expo.gate Which gate to export. NULL or character length one. Set to 
#' NULL to export data from all those gates defined in the gating strategy where 
#' 'keeoData' is set to TRUE. Provide a character length one with a gate name as 
#' defined in the gating strategy to export data from this gate only. If left 
#' at the default '.', the value as defined in the settings file (key 
#' 'dE_exportGate') will be used.
#' @param expo Logical, if extracted data should exported at all.
#' @param expo.type Character length one. The filetype of the data export. 
#' Possible values are 'csv' and 'xlsx'.  If left at the default '.', the value 
#' as defined in the settings file (key 'dE_exportType') will be used.
#' @param name.dict Character length one. The name of the dictionary. If left 
#' at the default '.', the value as defined in the settings file (key 
#' 'dD_dict_name') will be used.
#' @param foN.dict Character length one. The name of the folder where the 
#' dictionary resides. If left at the default '.', the value as defined in the 
#' settings file (key 'foN_dictionary') will be used.
#' @param type.dict Character length one. The filetype of the dictionary. Can 
#' be one of 'csv' or 'xlsx'. If left at the default '.', the value as defined 
#' in the settings file (key 'dD_dict_type') will be used.
#' @section Regarding Compensation: Due to the circumstances when developing 
#' this code, it was never required to apply any kind of compensation. The 
#' functionality to apply compensation was therefore never tested or verified. 
#' It is strongly advised to use caution when applying compensation. It might 
#' well be necessary to modify the source code of this package in order to 
#' achieve correct compensation results (compensation is applied in the 
#' function \code{\link{makeGatingSet}}).
#' @return An object of class 'fdmat' containing fluorescence distributions 
#' possibly re-calculated to events per volume unit.
#' @export
flowdexit <- function(fn=".", patt=NULL, gateStrat=".", foN.gateStrat=".", type.gateStrat=".", comp=".", tx=".", channel=".", expo.gate=".", expo=TRUE, expo.type=".", name.dict=".", foN.dict=".", type.dict=".", ignore.text.offset=FALSE, verbose=".") {
	
#	assignGatingSetToEnv(gs)
#	return(invisible(fdm))
} # EOF





