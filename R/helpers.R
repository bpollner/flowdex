checkOnTest <- function() {
	if (exists("onFdT_bbp", where=.GlobalEnv)) {
		if (get("onFdT_bbp", pos=.GlobalEnv)) {
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
	if (is.null(val)) {
		return(FALSE)
	}
	#
	if (all(val == ".") & length(val) == 1) {
		return(TRUE)
	} else {
		return(FALSE)
	}
} # EOF

checkCharX <- function(val, argName, len=1) {
	if (!haveDefDot(val)) {
		if (!all(is.character(val)) | length(val) != len) {
			stop(paste0("Please provide a character length ", len, " to the argument '", argName, "'."), call.=FALSE)
		}
    	return(invisible(NULL))
    } # end if
} # EOF

checkCharX_null <- function(val, argName, len=1) {
	if (!haveDefDot(val)) {
		if (is.null(val)) {
			return(invisible(NULL))
		} # end if
		if (!all(is.character(val)) | length(val) != len) {
			stop(paste0("Please provide a character length ", len, " to the argument '", argName, "'."), call.=FALSE)
		}
    	return(invisible(NULL))
    } # end if
} # EOF

checkCharX_null_Num <- function(val, argName, len=1) {
	if (!haveDefDot(val)) {
		if (is.null(val)) {
			return(invisible(NULL))
		} # end if
		if ( !all(is.character(val)) | length(val) != len) {
			if ( !all(is.numeric(val)) | length(val) != len) {
				stop(paste0("Please provide a numeric or character length ", len, " to the argument '", argName, "'."), call.=FALSE)
			} # end if num
		} # end if char
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

checkNumX <- function(val, argName, len=1) {
	if (!haveDefDot(val)) {
		if (!all(is.numeric(val)) | length(val) != len) {
			stop(paste0("Please provide a numeric length ", len, " to the argument '", argName, "'."), call.=FALSE)
		}
		return(invisible(NULL))
	} # end if
} # EOF

checkNumXNull <- function(val, argName, len=1) {
	if (!haveDefDot(val)) {
		if (is.null(val)) {
			return(invisible(NULL))
		} # end if
		if (!all(is.numeric(val)) | length(val) != len) {
			stop(paste0("Please provide a numeric length ", len, " to the argument '", argName, "'."), call.=FALSE)
		}
		return(invisible(NULL))
	} # end if
} # EOF

getDefValFromStn <- function(val, keyName, stn, argName=NULL, defValue=".") {
	if (is.null(defValue)) {
		if (haveDefDot(val)) {
			stop(paste0("Sorry, there is no default value defined for argument '", argName, "'."), call.=FALSE)
		} #end if		
		return(val)
	} # end if is null defValue
	#
	if (all(is.null(val))) {
		return(NULL)
	}
	#
	if (all(val == defValue)) {
		ind <- which(names(stn) == keyName)
		out <- unlist(stn[ind])
		return(out)
	} else { # now we want to take what is in val, user´s decision
		return(val)
	} # end else
} # EOF

checkDefToSetVal <- function(val, keyName, argName, stn, checkFor, len=1, defValue=".") {
	if (checkFor == "char") {
		checkCharX(val, argName, len)
		return(getDefValFromStn(val, keyName, stn, argName, defValue))
	} # end if checkFor == "char"
	#
	if (checkFor == "charNull") {
		checkCharX_null(val, argName, len)
		return(getDefValFromStn(val, keyName, stn, argName, defValue))
	} # end if checkFor == "char"
	#	
	if (checkFor == "charNullNum") {
		checkCharX_null_Num(val, argName, len)
		return(getDefValFromStn(val, keyName, stn, argName, defValue))
	} # end if checkFor == "char"
	#	
	if (checkFor == "logi") {
		checkLogi(val, argName)
		return(getDefValFromStn(val, keyName, stn, argName, defValue))
	} # end if checkFor == "logi"
	#
	if (checkFor == "num") {
		checkNumX(val, argName, len)
		return(getDefValFromStn(val, keyName, stn, argName, defValue))
	} # end if checkFor == "logi"
	#
	if (checkFor == "numNull") {
		checkNumXNull(val, argName, len)
		return(getDefValFromStn(val, keyName, stn, argName, defValue))
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

checkPggExistence <- function(gsdf,foN_gating, fiN_gateStrat=NULL) {
	if (is.data.frame(gsdf)) {
		pggDef <- gsdf[, "GateDefinition"]
	} else { # so we want to check for a single gate definition, gsdf comes in as character length one
		pggDef <- gsdf
	}
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

checkPath <- function(path) {
	if (!dir.exists(path)) {
		stop(paste0("Sorry, the folder '", path, "' does not seem to exist."), call.=FALSE)
	} else {
		return(TRUE)
	}
} # EOF

checkObjClass <- function(object, classChar, argName) {
	cl <- class(object)
	if  (cl != classChar) {
		stop(paste0("Please provide an abject of class '", classChar, "' to the argument '", argName, "'."), call.=FALSE)
	}
	return(TRUE)
} # EOF

cleanUpInfinites <- function(datMat) {
	#
	cleanCol <- function(mat, colNr) {
		vals <- mat[, colNr]
		indFin <- is.finite(vals)
		return(mat[indFin,])
	} # EOIF
	#
	datMat <- cleanCol(datMat, 1)
	datMat <- cleanCol(datMat, 2)
	return(datMat)
} # EOF

checkShowGateChannels <- function(pggShow, datMat) {
	namesShow <- sort(names(pggShow))
	namesHave <- sort(colnames(datMat))
	if (!identical(namesShow, namesHave)) {
		stop(paste0("Sorry, it seems that the gate specified at the argument 'showGate' does not contain the required channels \n'", paste(colnames(datMat), collapse="', '"), "'."), call.=FALSE)
	}
	return(TRUE)
} # EOF

getLocMat_TS <- function(locN) {
	#
#	return(try(graphics::locator(type="l", n=locN), silent=FALSE))
	#
	if (checkOnTest()) {
		pathToPgg <- get("pathToPgg", pos=.GlobalEnv)
		return(eval(parse(text=load(pathToPgg)))) # because in a test, we can not use the locator
	} else {
		return(try(graphics::locator(type="l", n=locN), silent=FALSE))
	}
} # EOF

assignHereStnValues <- function(stn) {
	res <- stn$dV_resolution
	apc <- stn$dV_cutoff_apply
	coR <- stn$dV_cutoff_raw
	coV <- stn$dV_cutoff_Vol
	rcv <- stn$dV_doRecalcToVolume
	igp <- stn$dV_ignoreEdgePercent
	smo <- stn$dV_doSmooth
	smN <- stn$dV_smooth_n
	smP <- stn$dV_smooth_p
	chPrevWl <- stn$dV_charBeforeFlscNr
	volFac <- stn$dV_volumeFactor
	useDic <- stn$dD_useDictionary
	dictType <- stn$dD_dict_type
	#
	doAssign <- function(name, value, npf=2) {
		assign(name, value, pos=parent.frame(n=npf))
	} # EOIF
	doAssign("res", res)
	doAssign("apc", apc)
	doAssign("coR", coR)
	doAssign("coV", coV)
	doAssign("rcv", rcv)
	doAssign("igp", igp)
	doAssign("smo", smo)
	doAssign("smN", smN)
	doAssign("smP", smP)
	doAssign("chPrevWl", chPrevWl)
	doAssign("volFac", volFac)
	doAssign("useDic", useDic)
	doAssign("dictType", dictType)
	doAssign("dictType", dictType)
	#
	return(TRUE)
} # EOF

checkAssignInput <- function(stn, fn, gateStrat, foN.gateStrat, type.gateStrat, comp, tx, channel, name.dict, foN.dict, type.dict, expo.gate, expo.name, expo.type, expo.folder, verbose) {
	#
	doAssign <- function(name, value, npf=2) {
		assign(name, value, pos=parent.frame(n=npf))
	} # EOIF
	#
	fn <- checkDefToSetVal(fn, "foN_fcsFiles", "fn", stn, checkFor="char")
	gateStrat <- checkDefToSetVal(gateStrat, "fiN_gateStrat", "gateStrat", stn, checkFor="char")
	foN.gateStrat <- checkDefToSetVal(foN.gateStrat, "foN_gating", "foN.gateStrat", stn, checkFor="char")
	type.gateStrat <- checkDefToSetVal(type.gateStrat, "dV_gateStratInputType", "type.gateStrat", stn, checkFor="char")
	comp <- checkDefToSetVal(comp, "dV_comp", "comp", stn, checkFor="logi")
	tx <- checkDefToSetVal(tx, "dV_tx", "tx", stn, checkFor="char")
	channel <- checkDefToSetVal(channel, "dV_channel", "channel", stn, checkFor="char")
	name.dict <- checkDefToSetVal(name.dict, "dD_dict_name", "name.dict", stn, checkFor="char")
	foN.dict <- checkDefToSetVal(foN.dict, "foN_dictionary", "foN.dict", stn, checkFor="char")
	type.dict <- checkDefToSetVal(type.dict, "dV_dictionaryType", "type.dict", stn, checkFor="char")
	expo.gate <- checkDefToSetVal(expo.gate, "dE_exportGate", "expo.gate", stn, checkFor="charNullNum")
	expo.name <- checkDefToSetVal(expo.name, "fiN_dataExport", "expo.name", stn, checkFor="char")
	expo.type <- checkDefToSetVal(expo.type, "dE_exportType", "expo.type", stn, checkFor="char")
	expo.folder <- checkDefToSetVal(expo.folder, "foN_rawData", "expo.folder", stn, checkFor="char")
	verbose <- checkDefToSetVal(verbose, "dV_verbose", "verbose", stn, checkFor="logi")
	##
	##
	doAssign("fn", fn)
	doAssign("gateStrat", gateStrat)
	doAssign("foN.gateStrat", foN.gateStrat)
	doAssign("type.gateStrat", type.gateStrat)
	doAssign("comp", comp)
	doAssign("tx", tx)
	doAssign("channel", channel)
	doAssign("name.dict", name.dict)
	doAssign("foN.dict", foN.dict)
	doAssign("type.dict", type.dict)
	doAssign("expo.gate", expo.gate)
	doAssign("expo.name", expo.name)
	doAssign("expo.type", expo.type)
	doAssign("expo.folder", expo.folder)
	doAssign("verbose", verbose)
	#
	return(TRUE)	
} # EOF

assignGatingSetToEnv <- function(gs) {
	nsp <- "pkg_flowdex_envs"				
	assign("gsenv", new.env(), pos=nsp)
	assign("gatingSet", gs, envir=get("gsenv"))
	return(invisible(NULL))
} # EOF

checkForGateNr <- function(fdmat, gate) { # gets the number from the range of the !!! metadata !!!
	gateNr <- gate
	if (is.character(gate)) {
		gateNr <- which(as.character(fdmat@metadata$gateName) == gate)
		if (length(gateNr) == 0) {
			stop(paste0("Sorry, the gate '", gate, "' does not seem to exist."), call.=FALSE)
		} # end if
	} # end if
	if (gateNr > nrow(fdmat@metadata)) {
		stop(paste0("Sorry, the gate nr ", gateNr, " does not exist."), call.=FALSE)
	} # end if
	#
	return(gateNr)
} # EOF
