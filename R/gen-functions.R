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

copyAllTemplates <- function(home, stn, onTest=FALSE) {
	if (!onTest) {
		fromFolder <- paste0(path.package("flowdex"), "/templates")
	} else {
		if (file.exists("inst")) {
			fromFolder <- paste0(getwd(), "/inst/templates")
		} else {
			fromFolder <- paste0(getwd(), "/templates")
		} # end else		
	} # end else if !onTest
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
#' @family  Helper Functions
#' @export
genfs <- function(copyTemplates=TRUE, where=getwd()) {
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

