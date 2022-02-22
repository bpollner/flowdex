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


checkCh1 <- function(char, argName) {
	if (!all(is.character(char)) | length(char) != 1) {
		stop(paste0("Please provide a character length one to the argument '", argName, "'."), call.=FALSE)
	}
    return(invisible(NULL))
} # EOF

checkDefToSetVal <- function(val, keyName, argName, stn, checkFor="char", defValue=".") {
	if (checkFor == "char") {
		checkCh1(val, argName)
		if (val == defValue) {
			ind <- which(names(stn) == keyName)
			out <- unlist(stn[ind])
			return(out)
		} else { # now we want to take what is in val, user´s decision
			return(val)
		}
	} # end if checkFor == "char
	#
} # EOF
