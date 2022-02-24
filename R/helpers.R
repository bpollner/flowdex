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
