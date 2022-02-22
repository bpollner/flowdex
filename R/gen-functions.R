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
#' @export
setupSettings <- function(path=NULL) {
	uniset::uniset_setup(where=path, get("uniset_env_name"))
	return(invisible(NULL))
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
	foN_gating <- stn$folderName_gating
	foN_fcsFiles <- stn$folderName_fcsFiles
	foN_rawData <- stn$folderName_rawData
	foN_templ <- stn$folderName_templates
	#
	aa <- createSingleFolder(where, foN_gating)
	bb <- createSingleFolder(where, foN_fcsFiles)
	cc <- createSingleFolder(where, foN_rawData)
	dd <- createSingleFolder(where, foN_templ)
	return(aa & bb & cc & dd)
} # EOF

copyAllTemplates <- function(home, stn) {
	ptp <- path.package("flowdex")
	if (dir.exists(paste0(ptp, "/inst"))) {
		fromFolder <- paste0(ptp, "/inst/templates") # needed for testing
 	} else {
 		fromFolder <- paste0(ptp, "/templates")
 	} # end else
	#
	foN_templ <- stn$folderName_templates
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

readInFlowSet <- function(folderName=NULL, patt=NULL, colPat=NULL, volCheck=TRUE) {
	rawdata <- try(flowCore::read.flowSet(path = folderName, pattern=patt, column.pattern=colPat, alter.names = TRUE, name.keyword="$FIL", ), silent=FALSE)
	if (class(rawdata) == "try-error") {
		stop("Sorry, an error while trying to read in the flowSet occured.", call.=FALSE)
	} # end try error
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
#' @param verbose Logical. If detailed status messages should be displayed.
#' Defaults to TRUE.
#' @return Nothing, resp. the modified fcs files written back into the same folder
#' and replacing the original ones.
#' @family Repair functions
#' @export
repairVolumes <- function(patt=NULL, vol=NULL, fn=".", includeAll=FALSE, confirm=TRUE, verbose=TRUE) {
	stn <- autoUpS()	
	#
	folderName <- checkDefToSetVal(fn, "folderName_fcsFiles", "fn", stn, checkFor="char", defValue=".")
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
	fs <- readInFlowSet(folderName, patt, volCheck=FALSE)
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

#' @title Read in FCS Files and Extract Data
#' @description XXX
#' @param fn Character length one. The name of the folder where FCS files should 
#' be read from. If left at the default '.', the folder name as defined in the 
#' settings file will be used.
#' @param patt A regular expression defining a possible subset of FCS files
#' residing in the directory specified by \code{fn} to read in. Only matching
#' patterns will be included.#' @export
flowdexit <- function(fn=".", patt) {
	return(NULL)
} # EOF


