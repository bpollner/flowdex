
getFlscX <- function(matSingle) {
	ncpwl <- matSingle@ncpwl
	xx <- colnames(matSingle)
	out <- as.numeric(substr(xx, ncpwl+1, nchar(xx)))
	return(out)
} # EOF

checkBandpass <- function(bandpass) {
	if (!all(is.numeric(bandpass)) | length(bandpass) != 2) {
		stop(paste0("Please provide a numeric length two to the argument 'bandpass'"), call.=FALSE)
	} # end if
	if (bandpass[1] >= bandpass[2]) {
		stop(paste0("The first value in 'bandpass' has to be smaller than the second one."), call.=FALSE)
	} # end if
	return(NULL)
} # EOF




#' @title Apply Bandpass Filter to Fluorescence Intensities
#' @description Apply a bandpass like filter to the fluorescence intensities 
#' within a single gate.
#' @inheritParams exportFdmatData
#' @param gate Numeric or character length one, defining the gate on which the 
#' bandpass should be applied. Defaults to 1. (See 'fdmat@metadata' for possible 
#' values for gate name resp. number.)
#' @param bandpass Numeric vector length two, holding the lower and upper 
#' boundaries of the bandpass filter.
#' @return The same object of class 'fdmat' as was provided to the argument 
#' 'fdmat', but with modified range of fluorescence intensities in the gate 
#' specified under 'gate'.
#' @examples
#' \dontrun{
#' fdmat_bp <- applyBandpass(fdmat, c(1400, 1600))
#' fdmat_bp <- applyBandpass(fdmat_bp, c(400, 1300), "fooBar")
#' }
#' @family Helper functions
#' @export
applyBandpass <- function(fdmat, bandpass, gate=1) {
	stn <- autoUpS()
	#
	checkBandpass(bandpass)
	gateNr <- checkForGateNr(fdmat, gate) # number is in the scope of the fdm@metadata !!!  # stops when bad gate
	gateName <- fdmat@metadata[gateNr, "gateName"]
	gaStrInd <- which(fdmat@gateStrat[, "GateName"] == gateName)
	matSingle <- fdmat[[gateNr]]
	#
    bpFrom <- bandpass[1]
    bpTo <- bandpass[2]
    flsc <- getFlscX(matSingle)
    cutLow <- which(flsc < bpFrom)
    cutHigh <- which(flsc > bpTo)
    #
    newFlRange <- paste0(bpFrom, ",", bpTo)
    matSingle@.Data <- matSingle@.Data[, -(c(cutLow, cutHigh))]
    matSingle@metadata$flRange <- newFlRange
    newEvpv <- rowSums(matSingle@.Data) # basically area under curve for each sample (in the rows)
    matSingle@eventsPerVol[,1] <- newEvpv # XXX we leave all the other columns be, for the moment. Not perfect.
    matSingle@note <- "bandpass applied"
	#
    fdmat@gateStrat[gaStrInd, "minRange"] <- bpFrom
    fdmat@gateStrat[gaStrInd, "maxRange"] <- bpTo
    fdmat@metadata[gateNr, "flRange"] <- newFlRange
    fdmat@note <- "some bandpass applied"
    #
    fdmat[[gateNr]] <- matSingle # put back the modified fdmat_single into the list within 'fdmat'
	#
	return(fdmat)
	#	
# 	now correct the evml data
# 	  newEvml <- rowSums(fdmat@.Data) # basically area under curve for each sample (in the rows)
#     fdmat@eventsPerVol$events_ml <- newEvml 
#     fdmat@eventsPerVol$events_ml.filt <- newEvml 
#     isF <- fdmat@eventsPerVol$is_filtered
#     if (any(isF == TRUE)) {
#         ind <- which(isF == TRUE)
#         fdmat@eventsPerVol$events_ml.filt[ind] <- 0 # XXX not correct any more
#     } # end if
#
} # EOF
