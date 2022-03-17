
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
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' fdmat <- flowdexit(patt = "GPos_T4")
#' fdmat_bp <- applyBandpass(fdmat, c(1600, 2400))
#' @template t_ex_finale
#' @family Accessory functions
#' @export
applyBandpass <- function(fdmat, bandpass, gate=1) {
    stn <- auto_up_s()
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
        if (nrow(matSingle@eventsPerVol) != 0) {
        matSingle@eventsPerVol[,1] <- newEvpv # XXX we leave all the other columns be, for the moment. Not perfect.
    } # end if
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
} # EOF

##################

makeColors <- function(nrCols, stn) {
    #
    paletteName <- stn$dG_RcolorBrewerPal
    aaa <- nrCols
    whatColors <- c("black", "red", "green", "blue", "cyan", "magenta", "yellow2", "gray")
    #
    if (nrCols > 8) {
        colRamp <- colorRampPalette(whatColors)
        return(colRamp(nrCols))
    } else {
        if (nrCols < 3) {aaa <- 3}
        colPool <- RColorBrewer::brewer.pal(aaa, paletteName) # we have to take minium 3 (do not know why.)
        return(colPool[1:nrCols])
    } # end else
} # EOF

plotCounts_inner <- function(mat, stn, ti="", ylog=FALSE, ccol=NULL, clt=NULL, ...) {
    lty <- 1
    plLog <- ""
    yaxt <-"n"
    yLabelAdd <- ""
    yLabelVolAdd <- ""
    cexLeg <- 0.85
    cexLegAlt <- 0.72
    maxLegLe <- 12
    ncLegRight <- 1
    ncLegLeft <- 1
    ZeV <- 0 # the "zero-value", used for checking what is all zero
    nonFluorescenceChar <- stn$dG_nonFluorescenceChar
    alphaForLegends <- stn$dG_alphaForLegends
    legBgCol <- rgb(255,255,255, alpha=alphaForLegends, maxColorValue=255) # a transparent background for the legend
    volUnit <- mat@eventsPerVol@volumeUnit
    #
    if (ylog) {
        plLog <- "y"
        yaxt <- "l" # just so, because l works
        yLabelAdd <- ", [log]"
        ZeV <- 1
        for (i in 1: nrow(mat)) {
            ind <- which(mat@.Data[i,] < 1)
            mat@.Data[i,ind] <- 1
        }
    } # end if ylog
    if (!is.null(clt)) {
        lty <- clt
    }
    #
    totNr <- nrow(mat) # just need that for the subtext
    zeroInd <- as.numeric(which(apply(mat,1, function(x) all(x==ZeV)))) # the indices of all the rows that contain all zero
    zeroChar <- rownames(mat)[zeroInd]
    if (length(zeroInd) > 0) {
        dataInd <- seq(1: nrow(mat))[-zeroInd]
    } else {
        dataInd <- seq(1: nrow(mat))
    }
    #
    mat@.Data <- mat@.Data[dataInd,] # only leave those rows in the matrix that do have data
#    mat <- mat[dataInd,]
    cols <- makeColors(nrow(mat), stn)
    if (!is.null(ccol)) {
        if (length(ccol) != nrow(mat)) {
        #    stop(paste0("Sorry, the provided custom color vector has a different size than the matrix.\nPlease provide a custom color vector with ", nrow(mat), " elements in it."), call.=FALSE)
        }
        cols <- ccol
    } # end if
    md <- mat@metadata
    flsc <- getFlscX(mat)
    if (nrow(mat@eventsPerVol) == 0) { # so we have no volume data
        evpv <- NULL
    } else {
        evpv <- mat@eventsPerVol[,1]
    } # end else

    typeXaxChar <- "Fluorescence distribution along " # the default title
    labXaxChar <- "Fluorescence intensity " # the default for the x-axis label
    channelChar <- md[1, "extractOn"]
    if (channelChar %in% nonFluorescenceChar) {
        typeXaxChar <- "Value along "
        labXaxChar <- "Scatter value "
    }
    mainTxt <- paste(ti)
    coAdd <- ""
    if (any(md$apc)) {
        coAdd <- paste0("; coV=", md$coV[1], "; coR=", md$coR[1])
    }
    gateDef <- paste0("using `", as.character(mat@metadata$gateDef), "`")
#    subTxt <- paste0("Gate: ", md$gateName, "; res=", md$res, coAdd, "; (S:", totNr, "/d", length(dataInd), ",z", length(zeroInd), ")")
    subTxt <- paste0("Gate: ", paste(md$gateName, collapse=", "), "; ", gateDef, "; res=", md$res[1], coAdd, "; (S:", totNr, "/", length(dataInd), ",", length(zeroInd), ")")
    extrOn <- as.character(md$extractOn)
    if (length(unique(extrOn)==1)) { # that comes from the time when we were plotting more than one gate on one graphic. Long gone now....
        extrOnAdd <- unique(extrOn)
    } else {
        extrOnAdd <- paste(extrOn, collapse=", ")
    }

    xlT <- paste0(labXaxChar, "(", extrOnAdd,")")
    ylT <- paste0("Raw Events ", yLabelAdd)
    if (any(md$rcv) & !is.null(evpv)) {
        ylT <- paste0("Events/", volUnit, "", yLabelAdd)
    } # end if
    yRange <- c(0, max(t(mat)))
    atY <- pretty(yRange)
    matplot(x=flsc, y=t(mat@.Data), yaxt=yaxt, type="l", log=plLog, main=mainTxt, sub=subTxt, col=cols, lty=lty, ylab=ylT, xlab=xlT, ...)
    abline(h=0, col="lightgray")
    legTxt <- rownames(mat)
#    legTxt <- paste(legTxt, " | ", prettyNum(evpv, big.mark=".", decimal.mark=","), " ev/ml", sep="")
    if (!is.null(evpv)) {
        legTxt <- paste0(legTxt, " | ", format(evpv, width=max(nchar(evpv)), big.mark = ".", decimal.mark=",", justify="right"), " ev/", volUnit)
    } # end if
    if (length(legTxt) > maxLegLe) {
        cexLeg <- cexLegAlt
        ncLegRight <- 2
    }
    if (length(zeroChar) > maxLegLe) {
        cexLeg <- cexLegAlt
        ncLegLeft <- 2
    }
    legend("topright", legTxt, col=cols, lty=lty, lwd=1, cex=cexLeg, ncol=ncLegRight, bg=legBgCol)
    if (length(zeroChar) > 0) {
        legend("topleft", zeroChar, cex=cexLeg, title="Zero:", ncol=ncLegLeft, bg=legBgCol)
    }
    if (!ylog) {
        axis(side=2, at=atY, labels=scales::scientific_format(1)(atY)) # could add las=2
    }
} # EOF

#' @title Plot Fluorescence Distribution
#' @description Plot the fluorescence distribution contained in the object of
#' class 'fdmat'. For each gate contained in 'fdmat', a graphic will be
#' produced.
#' @section Note: This function is merely intended to give a first overview of
#' the data resp. the fluorescence distribution. Its purpose is not to provide
#' ample and sufficient data visualisation.
#' @param fdmat An object of class "fdmat" as produced by \code{\link{makefdmat}}
#' or \code{\link{flowdexit}} .
#' @param gate Character or numeric length one. If more than one gate is present
#' in the provided fdmatrix, provide either the name of the gate, or a numeric
#' specifying the position of the gate in the metadata within the fdmatrix to
#' plot data from only that gate. If left at the default \code{NULL} and more
#' than one gate is present in the data, fluorescence distributions from
#' all gates will be plotted in individual plots.
#' @param ti Character length one. Will be used for the title in the plot.
#' @param ylog Logical. If the y-axis (the counts) should be plotted in log scale.
#' @param ccol An optional color vector for custom coloring. Must have the same
#' length as number of rows in the matrix.
#' @param clt Numeric vector specifying a sequence of custom line-types.
#' @param spl The column name in the cyTags of the values used for splitting.
#' Defaults to NULL, i.e. no splitting.
#' @param toPdf Logical. If output should be saved in results as PDF. Defaults to
#' TRUE.
#' @param fns Character length one. The filename suffix, defaults to NULL.
#' @param ... Additional plotting parameters passed on to 'matplot'
#' @inheritParams plotgates
#' @return (Invisible) NULL; is used for its side effects, i.e. to plot
#' fluorescence distributions.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' fdmat <- flowdexit(patt = "T4")
#' plotFlscDist(fdmat, toPdf = FALSE)
#' plotFlscDist(fdmat, spl = "C_treatment", toPdf = FALSE)
#' @template t_ex_finale
#' @family Plotting functions
#' @export
plotFlscDist <- function(fdmat, gate=NULL, ti="", spl=NULL, ylog=FALSE, ccol=NULL, clt=NULL, toPdf=TRUE, fns=NULL, foN.plots=".", ...) {
    #
    stn <- auto_up_s()
    #
    path <- ""
    pdfWidth <- stn$dG_pdf_width
    pdfHeight <- stn$dG_pdf_height
    #
    gsdf <- fdmat@gateStrat
    gateStrat <- fdmat@gateStrat@filename
    tiAdd <- "  |  "
    txtAdd <- suffixAdd <- ""
    #
    if (!is.null(gate)) {
        gateNr <- checkForGateNr(fdmat, gate)
        fdmat <- cutFdmatToGate(fdmat, gateNr)
    } # end if
#    gateDef <- aa$gateDef
#    gateName <- aa$gateSelected
    #
    if (!is.null(spl)) {
        cyTags <- fdmat@cyTags
        if (! spl %in% colnames(cyTags)) {
            stop(paste0("Sorry, the provided split column '", spl, "' is not present in the provided fdmat resp. its cyTags.\nPossible values are:\n'", paste0(colnames(cyTags), collapse="', '"), "'."), call.=FALSE)
        }
        txtAdd <- paste("split by", spl)
        suffixAdd <- paste0("_by",spl)
    } # end if
    if (toPdf) {cat(paste0("Plotting fluorescence distributions ", txtAdd, " ... "))}
    height <- pdfHeight
    width <- pdfWidth
    if (toPdf) {
        path <- checkDefToSetVal(foN.plots, "foN_plots", "foN.plots", stn, checkFor="char")
    } # end if
    filename <- paste0("FlscDist_", gateStrat, suffixAdd)
    filename <- paste(path, "/", filename, fns, ".pdf", sep="")
    if (toPdf) { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
#    if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}
    ####
    for (k in seq_along(fdmat)) {
        matSingle <- fdmat[[k]]
        if (!is.null(spl)) {
            splVals <- unique(cyTags[,spl])
            #
            for (i in seq_along(splVals)) {
                ind <- which(cyTags[,spl] == splVals[i])
                maSiUse <- matSingle
                maSiUse@.Data <- matSingle@.Data[ind,]
                maSiUse@eventsPerVol@.Data <- matSingle@eventsPerVol[ind,] # I know. We could have a method for subscripting the 'fdmat_single'. Later. XXX
                plotCounts_inner(maSiUse, stn, ti, ylog, ccol, clt, ...)
            } # end for i going through the splitVals
            #
        } else { # so spl is null, we do not want to split
            plotCounts_inner(matSingle, stn, ti, ylog, ccol, clt, ...)
        } # end else
    } # end for k going through the fdmat
    ####
    if (toPdf) {
        dev.off()
        cat("ok\n")
    } # end if
    return(invisible(NULL))
} # EOF
