
makeCyTags_inner <- function(gs,  dictionary, stn) { #
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
    pdExists <- cnSampleId %in% colnames(flowWorkspace::pData(gs))     # check for existence of sampleId column in the pData of the gating set
    if (!pdExists) {
        stop(paste0("Sorry, the required column 'sampleId' is not available in the present flow frames. \nMaybe there was a problem when reading in the fcs files?\nYou could retry calling 'makeGatingSet'."), call.=FALSE)
    } # end if !pdExists
    #


    ## now everything should be good to go
    cyt <- as.character(flowWorkspace::pData(gs)[, cnSampleId])
    groupedList <- strsplit(cyt, grSep)
#        if (!all(diff(unlist(lapply(grl, length)))==0)) { # is TRUE when all elements in the list have the same length
#            stop(paste0("Sorry, it seems that not all sample IDs have the same length."), call.=FALSE)
#        }
    maxInd <- which.max(unlist(lapply(groupedList, length))) # if not all sampleIDs have the same length, get the longest
    aa <- groupedList[[maxInd]] # just take the first one, they have to have all the same length
    cnShort <- trimws(unlist(lapply(strsplit(aa, ":"), function(x) x[1])))     #  get the column names coded in short
    cydf <- as.data.frame(matrix(NA, nrow=(length(gs)), ncol=length(cnShort)))
    colnames(cydf) <- cnShort
    for (i in seq_along(groupedList)) {
        siTube <- groupedList[[i]]
        for (k in seq_along(siTube)) {
            elm <- unlist(strsplit(siTube[k], elmSep))
            cnS <- trimws(elm[1])
            value <- trimws(elm[2])
            cydf[i,cnS] <- value # insert the value into the data frame
        } # end for k
    } # end for i
    # now we have the class and y-variables (still as character) in a data frame; go and translate the column names using the dictionary
    dict <- dictionary # returns a data frame with the short names in the first column and the translation in the second
    cnLong <- character(length(cnShort))
    for (i in seq_along(cnShort)) {
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
        for (i in seq_along(yInd)) {
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
    #
    clVarPref <- stn$dD_classVarPrefix
    gateNameChar <- "gate"
    #
    cyt <- makeCyTags_inner(gs, dictionary, stn)
    return(new("cyTags", cyt))
} # EOF

################
checkForVolumeData <- function(gs, stn) {
    useVolume <- stn$dV_use_volumeData
    #
    if (!useVolume) {
        return(FALSE)
    } # end if
    #
    options(warn=-1)
    naInd <- which(is.na(as.numeric(as.character(flowWorkspace::pData(gs)[,"volume"]))))
    options(warn=0)
    tubeIns <- "tube"
    verbIns <- "is"
    if (length(naInd) > 0) {
        if (length(naInd) > 1) {
             tubeIns <- "tubes"
             verbIns <- "are"
        }
        stop(paste0("Sorry, it seems that for the ", tubeIns, " '", paste(rownames(flowWorkspace::pData(gs))[naInd], collapse="', '"), "' there ", verbIns, " no volume-data available."), call.=FALSE)
    } # end if
    return(TRUE)
} # EOF

getEventsPerVolume_single <- function(gs, gateName="DNA+", chName="FITC.A", volFac=1e6,  volUnit="ml", apc=TRUE, coV=125) {
    colNameFilt <- "is_filtered"
    colNameMean <- "mean"
    #
    volUnitTxt <- paste0("events_", volUnit)
    cnsTotalGs <- flowWorkspace::colnames(gs) # colnames(fls) not working any more; #    cnsFls <- names(flowCore::markernames(fls)) is excluding the FSC and SSC channels
    #
    vols <- as.numeric(as.character(flowWorkspace::pData(gs)[,"volume"])) # read the acquired volumes from the pheno data of the gating set
    fls <- flowWorkspace::gs_pop_get_data(gs, gateName) # function was "getData"
    #
#    print(cnsTotalGs); print(chName); print(gateName)
    if (! chName %in% cnsTotalGs) {
        stop(paste0("Sorry, the channel '", chName, "' seems not to exist in the provided data."), call.=FALSE)
    }
    fluorList <- flowCore::fsApply(fls, function(x) x[,chName], use.exprs = TRUE, simplify = FALSE) # extract a single channel
    nrEvRaw <- as.numeric(unlist(lapply(fluorList, length)))
    means <- round(as.numeric(unlist(lapply(fluorList, mean))),0)
    evml <- evmlOrig <- round((nrEvRaw / vols) * volFac , 0) ##### here calculation ######
    filtVec <- rep("FALSE", length(evml))
    out <- data.frame(evml, means, filtVec)
    primCns <- c(volUnitTxt, colNameMean, colNameFilt)
    colnames(out) <- primCns
    rownames(out) <- paste0(flowWorkspace::sampleNames(gs))
    if (apc) { # change in the first and add the original
        out <- out[,-3] # remove the old is_filtered column
        aa <- which(evml <= coV)
        out[aa, volUnitTxt] <- 0
        filtVec[aa] <- "TRUE"
        out <- cbind(out, data.frame(filtVec), data.frame(evmlOrig))
        colnames(out) <- c(primCns[-3], colNameFilt, paste0(volUnitTxt, "_orig"))
    } # end if
    outClass <- new("eventsPV", out, gateName=gateName, volumeUnit=volUnit)
    return(outClass)
} # EOF

makeEmptyEvPVDataFrame <- function(nr) {
    outList <- vector("list", length=nr)
    eF <- new("eventsPV", data.frame(NULL))
    outList <- lapply(outList, function(x) x <- eF)
    return(outList)
} # EOF

#' @title Get Events per Volume Unit
#' @description From the data contained in the provided gating set, obtain a
#' list containing the events per volume unit in an object of class 'eventsPV'
#' for every gate in every single flowFrame (.e. sample tube) in a list element. 
#' @details This function is exported for (internal) back-compatibility. 
#' Its immediate benefit for the user is rather mild. 
#' @param gs A gating set as produced by \code{\link{makeAddGatingSet}}.
#' @return A list with the same length as there are 'keepData == TRUE' in the
#' gating strategy file, containing a data frame with the overall events per
#' volume unit for that gate in each list element.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' gs <- makeAddGatingSet()
#' evpv <- getEventsPerVolume(gs)
#' @template t_ex_finale
#' @seealso \code{\link{flowdexit}}
#' @export
getEventsPerVolume <- function(gs) {
    stn <- auto_up_s()
    #
    checkObjClass(object=gs, "GatingSet_fd", argName="gs")
    useVolume <- checkForVolumeData(gs, stn) # gets back FALSE when dV_use_volumeData in settings is FALSE
    #
    volFac <- stn$dV_volumeFactor
    volUnit <- stn$dV_volumeUnit
    apc <- stn$dV_cutoff_apply
    coV <- stn$dV_cutoff_Vol
    #
    gsdf <- gs@gateStrat
    gsdfUse <- gsdf[gsdf[,"keepData"],]
    nrKeep <- nrow(gsdfUse)
    if (!useVolume) {
        return(makeEmptyEvPVDataFrame(nrKeep))
    } # end if

    outList <- vector("list", length=nrKeep)
    #
    for (i in 1: nrow(gsdfUse)) {
        gateName <- gsdfUse[i,"GateName"]
        chName <- gsdfUse[i,"extractOn"]
        siEvml <- getEventsPerVolume_single(gs, gateName, chName, volFac, volUnit, apc, coV)
        outList[[i]] <- siEvml
    } # end for i
    return(outList)
} # EFO

#################

ignoreEdge <- function(x, perc=5, minLe=10) {
    tot <- length(x)
    if (tot <= minLe) {
        return(x)
    }
    if (perc <= 0) {
        return(x)
    }
    cut <- round((perc * tot) / 100, 0)
    cutIndLow <- 1:cut
    cutIndHigh <- seq(length(x)-cut+1, length(x))
    cutOff <- c(cutIndLow, cutIndHigh)
    x <- sort(x)
    out <- x[-cutOff]
    return(out)
} # EOF

getSomeXmin <- function(fluorList) {
    someMin <- NULL
    for (i in seq_along(fluorList)) {
        if (is.null(someMin)) {
            if (length(fluorList[[i]]) > 0) { # so it is not empty
                someMin <- round(min(fluorList[[i]]), 0)
            }
        } # end if
    } # end for i
    return(someMin)
} # EOF

extractHistoData <- function(x, sm, flscRan, res=220, igp=FALSE, smN=11, smP=5, dev=FALSE) {
       mainAdd <- ""; xRange <- NULL; mainTxt <- ""; plotHist <- FALSE; devPlot <- dev # used in DEV for plotting
       if (devPlot) {xOrig <- x; plotHist <- TRUE; mainTxt <- paste0("resolution: ", res, mainAdd)}
    #
    if (length(x) == 0) {
        return(list(mids=sm, countsSmo=0, countsOrig=0)) # is assigning the count of zero to a fluorescence value existent in the range; sm = "some minimum"
    }
    breaksPos <- seq(flscRan[1], flscRan[2], length.out=res)
    if (is.numeric(igp)){
        x <- ignoreEdge(x, perc=igp)
         if (devPlot) {
                  mainAdd <- paste0(", ", igp, "% of edge ignored")
            graphics::par(mfrow=c(2,1))
            hda <- graphics::hist(xOrig, breaks=breaksPos, plot=plotHist, main=paste0("resolution: ", res, "  (all data)"))
               graphics::lines(x=hda$mids, y=hda$counts, type="l", col="gray")
              graphics::lines(hda$mids, signal::sgolayfilt(hda$counts, n=smN, p=smP), col="red", lwd=2)
        } # DEV only
    } else {if (igp == TRUE) {stop("Please provide either a numeric or `FALSE` to ignore the edge percent", call.=FALSE)}}
    options(warn=-1)
    histData <- graphics::hist(x, breaks=breaksPos, plot=plotHist, main=mainTxt)
    options(warn=0)
    out <- list(mids=round(histData$mids, 0), countsSmo=signal::sgolayfilt(histData$counts, n=smN, p=smP), countsOrig=histData$counts)
    if (devPlot) {
        graphics::lines(x=out$mids, y=out$countsOrig, type="l", col="gray", xlim=NULL)
        graphics::lines(out$mids, out$countsSmo, col="blue", lwd=2)
    } # DEV only
    return(out)
} # EOIF

cleanUpHistList <- function(histList) {
    midsVec <- NULL
    for (i in seq_along(histList)) {
        if (is.null(midsVec)) {
            if (length(histList[[i]]$mids) > 1) { # so that is a non-empty sample
                midsVec <- histList[[i]]$mids
            }
        }
    } # end for i
    for (i in seq_along(histList)) {
        if (length(histList[[i]]$mids) == 1) { # so that is an empty sample
            histList[[i]]$mids <- midsVec
            histList[[i]]$countsSmo <- rep(0, length(midsVec))
            histList[[i]]$countsOrig <- rep(0, length(midsVec))
        }
    } # end for i
    return(histList)
} #EOF

recalcHistListToVolume <- function(histList, gs, volFac) {
    volumes <- as.numeric(as.character(flowWorkspace::pData(gs)[,"volume"]))
    for (i in seq_along(histList)) {
        histList[[i]]$countsSmo <- ( histList[[i]]$countsSmo / volumes[[i]] ) * volFac # gives the counts in events/volume for each fluorescence unit
        histList[[i]]$countsOrig <- ( histList[[i]]$countsOrig / volumes[[i]] ) * volFac
    }
    return(histList)
} # EOF

# is also calculating events/ml
checkCutFluorList <- function(fluorList, gs, apc=TRUE, coR=10, coV=125, volFac=1e6, rcv=TRUE) {
    if (apc) {
        if (rcv) {
            vols <- as.numeric(as.character(flowWorkspace::pData(gs)[,"volume"])) # read the acquired volumes from the pheno data of the gating set
        } else {
            vols <- NULL
        } # end else
#        nrEvRaw <- as.numeric(unlist(lapply(fluorList, length)))
        for (i in seq_along(fluorList)) {
            nrEvRaw <- length(fluorList[[i]])
            if (rcv) {
                nrEvVol <- round((nrEvRaw/vols[i]) * volFac, 0) # calculate the events per volume
            } else {
                nrEvVol <- coV + 1
            } # end else
            if (nrEvRaw <= coR | nrEvVol <= coV) {
                fluorList[[i]] <- numeric(0) # if below the defined cutoff values, set all the values to zero, i.e. have no single event in the list
            } # end if
        } # end for i
    } # end if apc
    return(fluorList)
} # EOF

# can return calculated to volume, here checking for cutoff
getHistoData <- function(gs, gateName="DNA+", chName="FITC.A", res=220, flRange=c(1250, 4000), apc=TRUE, coR=10, coV=125, igp=FALSE, smN=11, smP=5, rcv=TRUE, dev=FALSE, volFac=1e6) {
#    fls <- getData(gs, gateName)
    fls <- flowWorkspace::gs_pop_get_data(gs, gateName) # function was "getData"
    cnsTotalGs <- flowWorkspace::colnames(gs) # colnames(fls) not working any more; #    cnsFls <- names(flowCore::markernames(fls)) is excluding the FSC and SSC channels
    #
    if (! chName %in% cnsTotalGs) {
        stop(paste0("\nSorry, the channel '", chName, "' seems not to exist in the provided data."), call.=FALSE)
    }
    fluorList <- flowCore::fsApply(fls, function(x) x[,chName], use.exprs = TRUE, simplify = FALSE) # extract a single channel
    fluorList <- checkCutFluorList(fluorList, gs, apc, coR, coV, volFac, rcv) # here perform the cutoff-value check; needs the gs for the volume in the pheno data
    outIndList <- lapply(fluorList, function(x) which(x > flRange[2]))
    for (i in seq_along(fluorList)) {
        if (length(outIndList[[i]]) > 0) {
            fluorList[[i]] <- fluorList[[i]][-(outIndList[[i]])] # cut off everything above max(flRange), so that in the histogram we have no data running over the defined breaks
        }
    }
    # same on the lower side
    outIndList <- lapply(fluorList, function(x) which(x < flRange[1]))
    for (i in seq_along(fluorList)) {
        if (length(outIndList[[i]]) > 0) {
            fluorList[[i]] <- fluorList[[i]][-(outIndList[[i]])] # cut off everything below min(flRange), so that in the histogram we have no data running below the defined breaks
        }
    }
    #
    histList <- lapply(fluorList, extractHistoData, sm=getSomeXmin(fluorList), flscRan=flRange, res=res, igp=igp, smN=smN, smP=smP, dev=dev) ### CORE ###
    histList <- cleanUpHistList(histList)
    if (rcv) {
        histList <- recalcHistListToVolume(histList, gs, volFac)
    }
    return(histList)
} # EOF

makefdmat_single <- function(gs, gateName="DNA+", chName="FITC.A", res=220, flRange=c(1250, 4000), apc=TRUE, coR=10, coV=125, rcv=TRUE, igp=FALSE, smo=TRUE, smN=11, smP=5, chPrevWl="flsc", gateDef="locMat", dev=FALSE, volFac=1e6, verbose=TRUE) {
    rcvAdd <- ""
    if (rcv) {
        rcvAdd <- " and recalc. to volume"
    } # end if
    if (verbose) {cat(paste0(gateName, ": Extracting binned data on ", chName, " (res=", res, ")", rcvAdd, "... "))}
    ##
    histList <- getHistoData(gs, gateName, chName, res, flRange, apc, coR, coV, igp, smN, smP, rcv, dev, volFac) ### CORE ###
    flscX <- histList[[1]]$mids # just take the first one, the mids are all identical
    mat <- matrix(0, ncol=length(flscX), nrow=(length(histList)))
    for (i in seq_along(histList)) {
        vals <- histList[[i]]$countsOrig # take the original values (possibly re-calculated to volume)
        if (smo) {
            vals <- try(signal::sgolayfilt(vals, n=smN, p=smP), silent=FALSE) # smoothing
            if (class(vals) == "try-errpr") {
                message("Smoothing was skipped as it producecd an error.") ## XXX improve here. Try to catch no-data scenarios earlier.
            }
            vals[which(vals < 0)] <- 0 # because with smoothing values below zero can appear
        } # end if smo
        mat[i,] <- vals
    } # end for i
    colnames(mat) <- paste0(chPrevWl, flscX)
    rownames(mat) <- paste0(make.names(names(histList), unique=TRUE), "|", gateName)
#    zeroInd <- as.numeric(which(apply(mat,1, function(x) all(x==0)))) # the indices of all the rows that contain all zero
    md <- data.frame(gateName=gateName, gateDef=gateDef, extractOn=chName, res=res, flRange=paste(flRange, collapse=","), apc=apc, coR=coR, coV=coV, rcv=rcv, igp=igp, smo=smo, smN=smN, smP=smP, ncpwl=nchar(chPrevWl))
    if (verbose) {cat("ok. \n")}
    #
    eev <- new("eventsPV", data.frame(NULL))
    out <- new("fdmat_single", mat, eventsPerVol=eev, gateName=gateName, metadata=md, ncpwl=nchar(chPrevWl), note="original")
    return(out)
} # EOF

