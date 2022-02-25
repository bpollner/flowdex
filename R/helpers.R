checkOnTest <- function() {
	if (exists("onFdT", where=.GlobalEnv)) {
		if (get("onFdT", pos=.GlobalEnv)) {
			return(TRUE)
		}
	} else {
		return(FALSE)
	} 
} # EOF

devGetLocalStn <- function() {
	ptp <- path.package("flowdex")
	# set up the stn object
	if (dir.exists(paste0(ptp, "/inst"))) {
			stn <- source(paste0(ptp, "/inst/flowdex_settings.R"))$value
		} else {
			stn <- source(paste0(ptp, "/flowdex_settings.R"))$value
	} # end else
	return(stn)
} # EOF


##############

haveDefDot <- function(val) {
	if (all(val == ".") & length(val) == 1) {
		return(TRUE)
	} else {
		return(FALSE)
	}
} # EOF

checkCh1 <- function(val, argName) {
	if (!haveDefDot(val)) {
		if (!all(is.character(val)) | length(val) != 1) {
			stop(paste0("Please provide a character length one to the argument '", argName, "'."), call.=FALSE)
		}
    	return(invisible(NULL))
    } # end if
} # EOF

checkLogi <- function(val, argName) {
	if (!haveDefDot(val)) {
		if (!all(is.logical(val)) | length(val) != 1) {
			stop(paste0("Please provide either TRUE or FALSE to the argument '", argName, "'."), call.=FALSE)
		}
		return(invisible(NULL))
	} # end if
} # EOF

checkNum1 <- function(val, argName) {
	if (!haveDefDot(val)) {
		if (!all(is.numeric(val)) | length(val) != 1) {
			stop(paste0("Please provide a numeric length one to the argument '", argName, "'."), call.=FALSE)
		}
		return(invisible(NULL))
	} # end if
} # EOF


getDefValFromStn <- function(val, keyName, stn, defValue=".") {
	if (val == defValue) {
		ind <- which(names(stn) == keyName)
		out <- unlist(stn[ind])
		return(out)
	} else { # now we want to take what is in val, user´s decision
		return(val)
	} # end else
} # EOF


checkDefToSetVal <- function(val, keyName, argName, stn, checkFor, defValue=".") {
	if (checkFor == "char") {
		checkCh1(val, argName)
		return(getDefValFromStn(val, keyName, stn, defValue))
	} # end if checkFor == "char"
	#
	if (checkFor == "logi") {
		checkLogi(val, argName)
		return(getDefValFromStn(val, keyName, stn, defValue))
	} # end if checkFor == "logi"
	#
	if (checkFor == "num") {
		checkNum1(val, argName)
		return(getDefValFromStn(val, keyName, stn, defValue))
	} # end if checkFor == "logi"
	#
} # EOF

checkFileExistence <- function(foN_gating, fiN_gateStrat, typE, addTxt="") {
	paToFile <- paste0(foN_gating, "/", fiN_gateStrat, typE)
	if (!file.exists(paToFile)) {
		stop(paste0("Sorry, the ", addTxt, "file '", basename(paToFile), "' does not seem to exist in '", foN_gating, "'."), call.=FALSE)
	} # end stop
	return(TRUE) # if all is good
} # EOF

loadGaXFile <- function(foN, fiN, type) { # fiN must be complete with ending
	ptf <- paste0(foN, "/", fiN)
	#
	if (type == "csv") {
		return(read.csv(paste0(ptf, ".csv")))
	} # end if
	#
	if (type == "xlsx") {
		return(openxlsx::read.xlsx(paste0(ptf, ".xlsx")))
	} # end if
	#
	if (type == "pgg") {
		return(eval(parse(text=load(ptf))))
	} # end if
	#	
} # EOF

checkPggExistence <- function(gsdf,foN_gating, fiN_gateStrat) {
	pggDef <- gsdf[, "GateDefinition"]
	if (length(pggDef) == 0) {
		stop(paste0("Sorry, there must be at least one gate defined in the gating-strategy file '", fiN_gateStrat, "'."), call.=FALSE)
	} # end if
	#
	addG <- "gate "
	addV <- " seems "
	pggAvail <- list.files(foN_gating)
	if (!all(pggDef %in% pggAvail)) {
		ind <- which(!pggDef %in% pggAvail)
		missChar <- pggDef[ind]
		if (length(missChar) > 1) {
			addG <- "gates "
			addV <- " seem "
		}
		stop(paste0("Sorry, the requested ", addG,  "'", paste(missChar, collapse= "', '"), "'", addV, "not to exist in the folder \n`", foN_gating, "`."), call.=FALSE)
	} # end if
	return(TRUE) # if all is good
} # EOF










