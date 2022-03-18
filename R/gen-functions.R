# function used in the preamble of the examples to see if the example data are
# already there. Is downloading the data as defined in the template if not

#' @title Check and Download Example Dataset
#' @description Checks if the example dataset is present. If not, the example
#' dataset is downloaded.
#' @details Intended to be used within the examples; is downloading and
#' unzipping the folder 'flowdex_examples' in the folder specified at argument
#' 'where'. 
#' @param where Character length one. The path where the example dataset should
#' be looked for.
#' @param data_source The path to the remote .zip file.
#' @param force_download Logical. If data should be downloaded anyway.
#' Defaults to FALSE.
#' @return Logical. FALSE if the remote .zip file was downloaded, TRUE if the 
#' example dataset was present and no download was necessary.
#' @examples
#' td <- tempdir()
#' data_source <- "https://github.com/bpollner/data/raw/main/flowdex_examples/flowdex_examples.zip"
#' check_download_data(td, data_source)
#' @export
check_download_data <- function(where, data_source, force_download = FALSE) {
    dsname <- gl_name_of_example_dataset
    ptds <- paste0(where, "/", dsname)
    if (! dir.exists(ptds) | force_download) {
        targ_zip <- paste0(where, "/", dsname, ".zip")
        download.file(data_source, targ_zip, mode = "wb") ## DOWNLOAD ##
        unzip(targ_zip, exdir = where)
        return(FALSE)
    } # end if
    return(TRUE)
} # EOF

#' @title Perform Settings Setup
#' @description Perform the setup of the setting-system provided by package
#' 'uniset'.
#' @details Has to be done only once.
#' @param path Character length one, holding the path to the location where the
#' folder containing the settings.R file should be located. Defaults to 'NULL'.
#' If left at the default 'NULL', the location should be selectable
#' interactively.
#' @return (Invisible) NULL; is called for its side effects, i.e. to set up
#' the settings-file system as provided by package 'uniset'.
#' @examples
#' \donttest{
#' where <- "~/desktop"
#' setup_settings(where)
#' }
#' @family Setup functions
#' @export
setup_settings <- function(path = NULL) {
    uniset::uniset_setup(where = path, get("uniset_env_name"))
    return(invisible(NULL))
} # EOF

#' @title Manually Update the Settings
#' @description Source the settings-list as defined in the settings.R file and
#' create the object 'stn' in its environment. The settings-file system is
#' based on functionality from package \code{\link[uniset]{uniset}}.
#' @param silent Logical
#' @return A list holding key-value pairs as defined in the 'flowdex_settings.R'
#' file.
#' @family Setup functions
#' @template t_ex_assign
#' @examples
#' stn <- fd_update_settings()
#' str(stn)
#' @export
fd_update_settings <- function(silent=FALSE) {
    return(update_settings(silent))
} # EOF

create_single_folder <- function(where, fn) {
    path <- paste0(where, "/", fn)
    if (!dir.exists(path)) {
        ok <- dir.create(path)
    } else {
        return(FALSE)
    }
    return(ok)
} # EOF

create_folders <- function(where, stn) {
    foN_gating <- stn$foN_gating
    foN_fcsFiles <- stn$foN_fcsFiles
    foN_rawData <- stn$foN_rawData
    foN_templ <- stn$foN_templates
    foN_dict <- stn$foN_dictionary
    foN_plots <- stn$foN_plots
    #
    aa <- create_single_folder(where, foN_gating)
    bb <- create_single_folder(where, foN_fcsFiles)
    cc <- create_single_folder(where, foN_rawData)
    dd <- create_single_folder(where, foN_templ)
    ee <- create_single_folder(where, foN_dict)
    ff <- create_single_folder(where, foN_plots)
    #
    return(aa & bb & cc & dd & ee & ff)
} # EOF

copy_all_templates <- function(home, stn) {
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
#' @param copy_templates Logical, if available templates should be copied into
#' the folder 'templates'.
#' @param where Character length one, holding a valid path. Defaults to the
#' current working directory.
#' @return No return value, called for its side effects, i.e. the creation of
#' the required folder structure.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' genfs(exp_home)
#' @template t_ex_finale
#' @family Accessory functions
#' @export
genfs <- function(where=getwd(), copy_templates = TRUE) {
    stn <- auto_up_s()
    #
    check_path(where)
    create_folders(where, stn)
    #
    if (copy_templates) {
        copy_all_templates(where, stn)
    }
    return(invisible(NULL))
} # EOF

#' @title Check and Repair FCS Files
#' @description Check all fcs files in a specified folder for non-unique
#' keywords. Multiple entries in the keywords are removed, and the file is
#' written back to disc. The original ("corrupt") fcs file will be overwritten.
#' @details When reading in resp. working with some fcs files it can happen that
#' the following error message is displayed: \cr\code{"The HEADER and the TEXT
#' segment define different starting point ... to read the data"} \cr
#' After some testing, the author came to the conclusion that a solution to this
#' error can be to delete multiple entries of the same keyword in the keywords
#' of the fcs file. (As fcs files *not* displaying this error seem to have only
#' unique keywords.)\cr
#' It also appeared that always the last of the multiplied entries was the
#' correct one, hence the default keeping of the *last* of multiple entries. \cr
#' Currently, only uniformly multiplied keyword entries get remedied -- if there
#' should be a mixture of keyword-multiplication (e.g. some are two-fold, some
#' others are three-fold) an error message is displayed. \cr
#' Other approaches to this problem via e.g. ignoring the text offset as
#' possible in \code{\link[flowWorkspace]{load_cytoframe_from_fcs}} resulted
#' in data loss.
#' @param fcsRepair Logical. If defect fcs files should be attempted to repair.
#' If left at the default FALSE, fcs files with double entries in their keywords
#' will only be listed. If set to TRUE, the keyword doublets will be deleted and
#' the fcs file will be saved to disc. The original fcs file will be
#' overwritten.
#' @param confirm Logical. If confirmation is required before overwriting the
#' faulty fcs files with their corrected version. Defaults to TRUE. If set to
#' FALSE. original fcs files will be overwritten without further warning.
#' @param showMultiples Logical, If the multiplied keywords should be displayed.
#' Defaults to FALSE.
#' @param keepLast Logical or Numeric. If the last or the first item of a 
#' keyword multiplication should be kept. If left at the default TRUE, the 
#' last keyword of a keyword multiplication will be kept, if set to FALSE the
#' first will be kept. Provide a numeric length one to denote the number of 
#' the multiplied keyword to keep.
#' @inheritParams flowdexit
#' @return (Invisible) TRUE. Is used for its side effect, i.e. to repair fcs
#' files with multiplied keywords and to write those fcs files to disc.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' fon <- "fcsF_E_rep"
#' # checkRepairFcsFiles(fon)  # just to see what needs repairing
#' checkRepairFcsFiles(fon, TRUE, FALSE)
#' @template t_ex_finale
#' @family Accessory functions
#' @family Repair functions
#' @export
checkRepairFcsFiles <- function(fn=".", fcsRepair=FALSE, confirm=TRUE, 
    showMultiples=FALSE, keepLast=TRUE, verbose=TRUE) {
    #
    stn <- auto_up_s()
    #
    folderName <- checkDefToSetVal(fn, "foN_fcsFiles", "fn", stn, checkFor="char")
    #
    fcsNames <- list.files(folderName)
    txtShowMultiples <- "(You can use 'checkRepairFcsFiles' directly and set 'showMultiples' to TRUE to display the multiplied keywords.)\n"

    # now go and check every file in the folder if the text offset problem is there. To save time, we only read in the fcs header.
    banaDef <- ptfDef <- takeOutInds <-  NULL
    takeOutInds <- vector("list", length=0)
    for (i in seq_along(fcsNames)) {
        ptf <- paste0(folderName, "/", fcsNames[i])
        bana <- basename(ptf)
        siHe <- flowCore::read.FCSheader(bana, folderName)[[1]] # a single header, comes in as a list length one, is now a named character
        rlNa <- rle(names(siHe))
        if (all(unique(rlNa$lengths) == 1)) { # all keywords appear one time. All should be good.
            next
        } else { # so we do have some non uniques
            ind <- which(rlNa$lengths != 1) # check for more than once keywords
            multKeys <- rlNa$values[ind]
            ind <- which(names(siHe) %in% multKeys)
            multNames <- names(siHe)[ind]
            rlMultNa <- rle(multNames)
            multFac <- unique(rlMultNa$lengths) # gives the factor of how often a single keyword is repeated
            #
            if (length(multFac) != 1) { # so we do have a mixture of multiplications, not all keywords are only two-fold or only three-folde etc.
                msg <- paste0("Sorry, it seems that I am not able to automatically repair the keywords of file '", bana, "'.\nThere appears to be a mixture of keyword multiplications (", paste0(multFac, collapse=", "), ").\nCurrently, only keywords that are uniformly multiplied (all two-fold, or all three-fold etc.) can be automatically removed.\n")
                stop(msg, call.=FALSE)
            } # end mixture of multiplication
            #
            if (is.numeric(keepLast)) {
                if (keepLast < 1 | keepLast > multFac) {
                    stop(paste0("Sorry, please provide a number ranging from 1 to ", multFac, " to the argument 'keepLast'."), call.=FALSE)
                } # end if
                remStep <- keepLast
                remChar <- paste0("All except #", keepLast, " ")
                if (keepLast == multFac) { # same as keepLast=TRUE
                    keepLast <- TRUE
                } else {
                    if (keepLast == 1) {
                        keepLast <- FALSE
                    } # end if
                } # end else
            } # end if is numeric keekPast
            #
            if (is.logical(keepLast)) {
                if (keepLast) {
                    remStep <- multFac
                    remChar <- "All except the last "
                } else {
                    remStep <- 1
                    remChar <- "All except the first "
                } # end else
            } # end if is logical keepLast
            #
            ind <- which(names(siHe) %in% multNames) #### map back to total names ####
            io <- seq(1, length(ind))
            ioo <- seq(remStep, length(io), by=multFac) # define the indices to remove from io
            io <- io[-ioo] # io: define the indices to keep from ind
            toi <- ind[io] # toi is the index in the scope ot the total keywords that has to go out
            takeOutInds <- c(takeOutInds, list(toi)) # collect take out indices in a list
            banaDef <- c(banaDef, bana) # collect file names
            ptfDef <- c(ptfDef, ptf) # collect paths
            #
            if (showMultiples) {
                txtShowMultiples <- ""
                print(bana)
                print((siHe)[ind])
            #    cat("\n  cleaned up that will be:\n")
            #    print(siHe[-toi]) # gives a nasty output
                cat("-------------------\n\n")
            } # end if showMultiples
            #
        } # end else
    } # end for i going through the fcsNames in the folder


    ## by now we checked every fcs file in the folder and collected names, paths and take-out indices of the keywords of the defect fcs files
    if (is.null(banaDef)) { # no defect fcs keywords were found
        if (verbose) {
            cat(paste0("All fcs files in the folder '", folderName, "' \nseem to be ok, i.e. do have only single entries in their keywords.\n"))
        } # end if verbose
        return(invisible(NULL))
    } # end is null banaDef


    ## so banaDef is not null, that means we have at least one defect fcs file
    sAdd <- "s"
    doAdd <- "do have"
    itAdd <- "their"
    if (length(banaDef) == 1) {
        sAdd <- ""
        doAdd <- "has"
        itAdd <- "its"
    } # end if
    ##
    txtInfo <- paste0("\nThe following ", length(banaDef), " file", sAdd, " from the folder '", folderName, "' \n", doAdd, " non-unique entries (all ", multFac, " fold) in ", itAdd, " keywords:\n", paste0(banaDef, collapse="\n"), "\n")

    ## in case we do NOT want to repair
    if (!fcsRepair) {
        cat(txtInfo)
        stop("Consider setting 'fcsRepair' to TRUE.\nCAVE: Original fcs files will then be overwritten.", call.=FALSE)
    } # end if !fcsRepair


    ## so by now we do have at least one defect fcs file, and we DO want to repair
    txtW <- "will be "
    if (!confirm) {
        txtW <- "were "
        txtShowDoubles <- ""
    } # end if
    txtAction <- paste0("\n", remChar, "of each multiplied keyword ", txtW, "removed, and the original fcs file ", txtW, "overwritten.\n")
    cat(txtInfo)
    cat(txtAction)
    cat(txtShowMultiples)
    #
    if (confirm) {
        cat("\nPress enter to continue or escape to abort:\n")
        scan(file = "", n = 1, quiet = TRUE)
    } # end if

    ## now actually repair the fcs files
    # banaDef, ptfDef, takeOutInds
    for (i in seq_along(banaDef)) {
        siFF <- flowCore::read.FCS(ptfDef[i], ignore.text.offset=TRUE) # but I think that 'ignore.text.offset' is not really working here.
        newDes <- siFF@description[-takeOutInds[[i]]]
        newFF <- new("flowFrame",  exprs=siFF@exprs, parameters=siFF@parameters, description=newDes)
        flowCore::write.FCS(newFF, ptfDef[i])
    } # end for i going through the defect names
    #
    if (confirm) {
        cat(paste0(length(banaDef), " fcs files were saved to disc.\n"))
    } # end if
    return(invisible(TRUE))
} # EOF

readInFlowSet <- function(folderName=NULL, patt=NULL, colPat=NULL, 
    volCheck=TRUE, fcsRepair=FALSE, verbose=TRUE) {
    #
    checkRepairFcsFiles(fn=folderName, fcsRepair, confirm=FALSE, showMultiples=FALSE, keepLast=TRUE, verbose=FALSE)
    #
    rawdata <- try(flowCore::read.flowSet(path = folderName, pattern=patt, column.pattern=colPat, alter.names = TRUE, name.keyword="$FIL"), silent=FALSE)
    if (is(rawdata, "try-error")) {
        stop("Sorry, an error while trying to read in the flowSet occured.", call.=TRUE)
    } # end try error
    #
    # now transform into flowCore "space" so that repairing volume and sample ID still work
    rawdata <- flowWorkspace::cytoset_to_flowSet(rawdata) # now we are back in the flowSet as produced by package "flowCore"
    #
    pdkw <- list(volume="VOL", btim="$BTIM", sampleId="$SMNO") # define what to use in the pheno-data
    kw <- flowCore::keyword(rawdata, pdkw)  # "keyword" is a function in package "flowCore"; as seen on 21.04.2021 on https://support.bioconductor.org/p/p132747/#p132763
    flowWorkspace::pData(rawdata) <- as.data.frame(kw) # pData is a function in package "flowWorkspace"
    # now check for missing volume values and, if a value for val is provided, replace with that
    pDat <- flowWorkspace::pData(rawdata)
    indNA <- which(as.character(pDat[,"volume"]) == "NA")
    if (length(indNA) > 0 & volCheck) {
            if (length(indNA) == 1) {add <- ""} else {add <-"s"}
            stop(paste0("Sorry, there appear to be missing volume values in the sample", add, "\n", paste(pDat[indNA,"name"], collapse=", "), ".\nPlease use the function `repairVolumes` to repair the affected FCS files."), call.=FALSE)
    } # end length(indNA)
    return(rawdata)
} # EOF

#' @title Repair Volume Values in FCS Files
#' @description Read in all or a subset of FCS files, replace missing volume
#' values in the description of the single flowFrames with a provided default
#' value and write the FCS-files back to disc. Original fcs get overwritten.
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
#' @return (Invisible) NULL. Is used for its side effects, i.e. to repair fcs
#' files with missing volume data and write them to disc, thereby overwriting
#' the original fcs files.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' check_download_data(td, data_source, TRUE) # force download here
#' #
#' fon <- "fcsF_E_vol_sid"
#' repairVolumes(vol=1234567, fn=fon, confirm = FALSE)
#' @template t_ex_finale
#' @family Accessory functions
#' @family Repair functions
#' @export
repairVolumes <- function(patt=NULL, vol=NULL, fn=".", includeAll=FALSE, 
    confirm=TRUE, fcsRepair=FALSE, verbose=TRUE) {
    stn <- auto_up_s()
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
    fs <- readInFlowSet(folderName=folderName, patt=patt, volCheck=FALSE, fcsRepair=fcsRepair, verbose=verbose)
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
    for (i in seq_along(namesUse)) {
        flowFile <- paste0("fs@frames$", namesUse[i])
        txt <- paste0(flowFile, "@description$VOL <- ", vol)
        eval(parse(text=txt)) # here write the provided volume into the description of a single flowFrame
#        ffn <- paste0(folderName,"/",namesUse[i], ".fcs") # the name of folder and file
        ffn <- normalizePath(paste0(folderName,"/",namesUse[i])) # the name of folder and file   ## also file.path()
        options(warn=-1)
        txt <- paste0("flowCore::write.FCS(", flowFile, ", \"", ffn, "\")") # good on UNIX
        if (.Platform$OS.type == "windows") {
            pat <- "\\\\"
            repl <- "/"
            txt <- gsub(pat, repl, txt) # because we come in as the windows path, but then execute that from within R, so it needs to be forward slashes. Ha.
        } # end if windows
        eval(parse(text=txt)) # write the single flowFrame with corrected Volume back to file
        options(warn=0)
        cat(".")
    } # end for i
    cat(" ok.\n")
    return(invisible(NULL))
} # EOF

#' @title Repair a Single Sample ID
#' @description Replace a faulty sample ID with a new one and write the single
#' fcs file back to disc.
#' @details To first obtain the flowSet, leave the parameter `fs`at its default
#' NULL. By providing a pattern to `patt`, subgroups of fcs files can be read in.
#' Provide the so obtained flowSet to the parameter `fs`, and specify the
#' name and the new sample ID in order to re-write the specified file with its
#' new Sample ID. 'object@phenoData@data' can be used to inspect and verify names
#' of FCS files and the sample IDs therein -- see examples.
#' @section Note: A correct sample ID is of importance when using the
#' 'dictionary' to expand the abbreviations in the sample ID - see
#' \url{https://bpollner.github.io/flowdex/articles/acquire_data.html}.
#' @param fs The object returned by this function if parameter \code{fs} is left
#' at its default NULL, what then can be used as input for the parameter \code{fs}.
#' @param name Character length one. The name of the fcs file within the flowSet
#' that should get a new sample ID.
#' @param newSID Character length one. The new Sample ID.
#' @inheritParams flowdexit
#' @param confirm Logical. If the user should be asked for additional confirmation
#' before the rewriting of the fcs file is performed. Defaults to TRUE.
#' @return (Invisible) NULL. Is called for its side effects: the specified single
#' fcs file gets written to disc with its new sample ID.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' fon <- "fcsF_E_vol_sid"
#' repairVolumes(vol=1234567, fn=fon, confirm = FALSE) # because these files are also used 
#' # to demonstrate function 'repairVolumes'. We need to repair the volumes there first
#' flowset <- repairSID(fn = fon)
#' flowset@phenoData@data # very bad sample ID in the fourth sample
#' # view the correct sample IDs of the other samples
#' # copy one of those correct sample IDs
#' # paste and modify it - it should be beaker #3:
#' nsid <- "tr: GPos; Td: 5; wt: nativ; ap: no; th: th1; ha: ha1; bk: b3"
#' # also copy and paste the sample name
#' sana <- "N_na_GPos_T5_th1_b3.fcs" # the  name of the sample having the faulty sample ID
#' # now put all together and write fcs file with correct sample ID back to disk
#' repairSID(fs=flowset, fn = fon, name = sana, newSID = nsid, confirm = FALSE)
#' #
#' # and check again:
#' flowset <- repairSID(fn = fon)
#' flowset@phenoData@data # all is good 
#' @template t_ex_finale
#' @family Accessory functions
#' @family Repair functions
#' @export
repairSID <- function(fs=NULL, name=NULL, newSID=NULL, patt=NULL, fn=".", 
    confirm=TRUE, fcsRepair=FALSE) {
    stn <- auto_up_s()
    #
    fn <- checkDefToSetVal(fn, "foN_fcsFiles", "fn", stn, checkFor="char")
    #
    if (is.null(fs)) {
        return(invisible(readInFlowSet(folderName=fn, patt=patt, fcsRepair=fcsRepair)))
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
    eval(parse(text = txt)) # write the new sample ID into the single flowFrame.
    ffn <- normalizePath(paste0(fn,"/", name, "")) # the name of folder and file
    options(warn=-1)
    txt <- paste0("flowCore::write.FCS(", flowFile, ", \"", ffn, "\")")
    if (.Platform$OS.type == "windows") {
            pat <- "\\\\"
            repl <- "/"
            txt <- gsub(pat, repl, txt) # because we come in as the windows path, but then execute that from within R, so it needs to be forward slashes. Ha.
        } # end if windows
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
#' @return A gating set as produced by \code{\link[flowWorkspace]{GatingSet}}.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' gs1 <- makeGatingSet()
#' gs2 <- makeGatingSet(patt="T4")
#' @template t_ex_finale
#' @family Extraction functions
#' @export
makeGatingSet <- function(patt=NULL, comp=".", fn=".", tx=".", channel=".", 
    fcsRepair=FALSE, verbose=".") {
    stn <- auto_up_s()
    #
    comp <- checkDefToSetVal(comp, "dV_comp", "comp", stn, checkFor="logi")
    fn <- checkDefToSetVal(fn, "foN_fcsFiles", "fn", stn, checkFor="char")
    tx <- checkDefToSetVal(tx, "dV_tx", "tx", stn, checkFor="char")
    channel <- checkDefToSetVal(channel, "dV_channel", "channel", stn, checkFor="char")
    verbose <- checkDefToSetVal(verbose, "dV_verbose", "verbose", stn, checkFor="logi")
    #
    if (verbose) {cat("Reading in fcs files... ")}
    thisVolCheck <- stn$dV_use_volumeData
    rawdata <- readInFlowSet(folderName=fn, patt=patt, colPat=channel, volCheck=thisVolCheck, fcsRepair=fcsRepair, verbose=verbose)
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
#    biexpTFL <- flowWorkspace::transformerList(colnames(gs), flowWorkspace::flowjo_biexp_trans()) ## that did work before? now not any more ???
    biexpTFL <- flowWorkspace::transformerList(cns, flowWorkspace::flowjo_biexp_trans())
    gs <- flowWorkspace::transform(gs, biexpTFL)
    if (verbose) {cat("ok. \n")}
    return(gs)
} # EOF

importCheckGatingStrategy <- function(fiN_gateStrat, stn, gsType=".", 
    foName=".") {
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
    } # end is null
    checkFileExistence(foN_gating, fiN_gateStrat, typE, addTxt="gating strategy file ")
    gateStrat <- loadGaXFile(foN_gating, fiN_gateStrat, gsType)
    cns <- sort(colnames(gateStrat))
    if (!identical(cns, sort(cnsReq))) {
        stop(paste0("Sorry, the provided gating strategy file '", fiN_gateStrat, "' does not contain the required column names.\nPlease see the template for an example.\nThe required column names are:\n'", paste(cnsReq, collapse="', '"), "'."), call.=FALSE)
    } # end if
    #
    if (all(gateStrat[,"keepData"] == FALSE)) {
        stop(paste0("All values in the column 'keepData' in the gating strategy file '", paste0(fiN_gateStrat, typE), "' are set to FALSE. \nYou need to keep data from at least one gate."), call.=FALSE)
    } # end if
    #
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
#' @return An object of \code{\link{class-GatingSet_fd}}.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' gs <- makeGatingSet()
#' gs
#' gs <- addGates(gs)
#' gs
#' @template t_ex_finale
#' @family Extraction functions
#' @export
addGates <- function(gs, gateStrat=".", foN.gateStrat=".", type.gateStrat=".",
    verbose=".") {
    stn <- auto_up_s()
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
        if (is(erm, "try-error")) {
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
#'  In order to see a schematic representation of 'parent' and 'child' gates,
#' simply use 'plot'.
#' @inheritParams flowdexit
#' @return An object of \code{\link{class-GatingSet_fd}} with added and
#' recomputed gates.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' gs1 <- makeAddGatingSet()
#' gs2 <- makeAddGatingSet(patt = "T4")
#' gs3 <- makeAddGatingSet(gateStrat = "gateStrat_2")
#' gs3
#' @template t_ex_finale
#' @family Extraction functions
#' @export
makeAddGatingSet <- function(patt=NULL, fn=".", gateStrat=".", foN.gateStrat=".",
    type.gateStrat=".", comp=".", tx=".", channel=".", fcsRepair=FALSE,
    verbose=".") {
    stn <- auto_up_s()
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
    gs <- makeGatingSet(patt, comp, fn, tx, channel, fcsRepair, verbose)
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
#' @param flf Character or numeric length one. The identifier of the flowFrame
#' within the gating set where the gate should be drawn on. Optimally, this is
#' a flowFrame, i.e. sample, with a very good representation of the desired
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
#' specified at 'ppgId'. Thus, the old gate will be replaced with the new one.
#' @section Warning: Existing locator matrix files with the same name will be
#' overwritten without asking!
#' @return A list with the locator coordinates. Mainly called for its side
#' effect, i.e. the locator matrix data saved as an R-object in the folder
#' specified at 'foN.gateStrat'.
#' @section Link: Please refer also to 
#' \url{https://bpollner.github.io/flowdex/articles/workflow_1.html} 
#' for an in-depth description of the workflow how to create the gating 
#' strategy.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' # only required for the automated tests
#' # this simulates the manual drawing of a gate:
#' pathToPgg <- paste0(exp_home, "/gating/BactStainV1")
#' assign("pathToPgg", pathToPgg, pos=.GlobalEnv) # do NOT call this ('assign')
#' # when running example manually
#' #####
#' #####
#' gs <- makeGatingSet(patt="GPos_T4")
#' drawGate(gs, 1, useLoc = FALSE) # to just check
#' drawGate(gs, 1, pggId = "new_pgg", showGate = "BactStainV1")
#' drawGate(gs, 1, pggId = "new_pgg", showGate = "new_pgg") 
#' @template t_ex_finale
#' @family Plotting functions
#' @export
drawGate <- function(gs, flf=NULL, gn="root", pggId=".", channels=".",
    foN.gateStrat=".", useLoc=TRUE, locN=512, bnd=".", showGate=NULL) {
    stn <- auto_up_s()
    #
    pggColor <- stn$dG_locatorLine
    pggLwd <- stn$dG_locatorLineWidth
    pggShowColor <- stn$dG_gateShowColor
    #
#    checkObjClass(object=gs, "GatingSet", argName="gs")
    foN_gating <- checkDefToSetVal(foN.gateStrat, "foN_gating", "foN.gateDefs", stn, checkFor="char")
    pggId <- checkDefToSetVal(pggId, "fiN_gate", "pggId", stn, checkFor="char")
    showGate <- checkDefToSetVal(showGate, "..x..", "showGate", stn, checkFor="charNull", defValue=NULL)
    channels <- checkDefToSetVal(channels, "dV_channelsForPGG", "channels", stn, checkFor="char", len=2)
    bnd <- checkDefToSetVal(bnd, "dV_channelBoundaries", "bnd", stn, checkFor="numNull", len=4)
    #
    if (is.null(flf)) {
        stop("Please provide either a name or an index to specify on which flowFrame the gate should be drawn.\nUse `gsinfo()` to see possible values.", call.=FALSE)
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
        if (is(locMat, "try-error")) {
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
    } # end if useLoc
    return(invisible(locMat))
} # EOF

#' @title Cut 'fdmat' Object to Gate
#' @description Cut an object of class `fdmat` down to only a single gate
#' @param gate Numeric or Character length one. The designator for the gate to
#' keep, as defined in the gating strategy (from those gates where 'keepData' is
#' set to TRUE.
#' @inheritParams exportFdmatData
#' @return An object of \code{\link{class-fdmat}} containing only the data for
#' the gate as specified in \code{gate}.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' fdmat <- flowdexit(gateStrat = "gateStrat_2")
#' fdmat_cut <- cutFdmatToGate(fdmat, 2)
#' @template t_ex_finale
#' @family Accessory functions
#' @export
cutFdmatToGate <- function(fdmat, gate=NULL) {
    if (length(fdmat) == 1 ) { # nothing to cut
        return(fdmat)
    }
    if (is.null(gate)) {
        stop("Please provide a gate name or a number (as defined in the metadata) to the argument 'gate'.", call.=FALSE)
    } # end if
    #
    gateNr <- checkForGateNr(fdmat, gate)
    #
    fdmat@.Data <- list(fdmat[[gateNr]]) # make a new list length one, holding the object of class 'fdmat_single' (obtained by fdmat[[gateNr]])
    fdmat@metadata <- fdmat[[1]]@metadata #
    fdmat@note <- paste0("cut down to gate: ", fdmat[[1]]@eventsPerVol@gateName)
    #
    return(fdmat)
} # EOF

#' @title Export Fluorescence Distributions
#' @description Export fluorescence distributions contained in the 'fdmat'
#' object to file.
#' @details If data are exported to xlsx, additional data like the metadata
#' describing the parameters that lead to the calculation of the fluorescence
#' distribution, the cyTags and the gating strategy are saved in an extra sheet
#' as well. If exporting to csv, only the fluorescence data from one single gate
#' can be exported.
#' @param fdmat An object of \code{\link{class-fdmat}} as produced by
#' \code{\link{makefdmat}}.
#' @inheritParams flowdexit
#' @return Invisible NULL; used for its side effects, i.e. to export the data
#' contained in 'fdmat' to file.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' fdmat <- flowdexit()
#' exportFdmatData(fdmat)
#' #
#' fdmat <- flowdexit(gateStrat = "gateStrat_2")
#' exportFdmatData(fdmat, expo.gate = 2, expo.name="data_gate_highSSC_only")
#' @template t_ex_finale
#' @family Accessory functions
#' @export
exportFdmatData <- function(fdmat, expo.gate=".", expo.name=".",
    expo.type=".", expo.folder=".", verbose=".") {
    stn <- auto_up_s()
    #
    expoType <- checkDefToSetVal(expo.type, "dE_exportType", "expo.type", stn, checkFor="char")
    expoGate <- checkDefToSetVal(expo.gate, "dE_exportGate", "expo.gate", stn, checkFor="charNullNum")
    expoName <- checkDefToSetVal(expo.name, "fiN_dataExport", "expo.name", stn, checkFor="char")
    expoFolder <- checkDefToSetVal(expo.folder, "foN_rawData", "expo.folder", stn, checkFor="char")
    verbose <- checkDefToSetVal(verbose, "dV_verbose", "verbose", stn, checkFor="logi")
    flscChar <- stn$dV_charBeforeFlscNr
    evpvChar <- stn$dV_charEventsPerVolume
    #
    check_path(expoFolder)
    #
    typE <- NULL
    if (expoType == "csv") {
        typE <- ".csv"
    }
    if (expoType == "xlsx") {
        typE <- ".xlsx"
    }
    if (is.null(typE)) {
        stop("Please provide either 'csv' or 'xlsx' as preferred output type for the file holding the exported data (settings.R file key name 'dE_exportType')", call.=FALSE)
    } # end is null
    #
    if (!is.null(expoGate)) {
        fdmat <- cutFdmatToGate(fdmat, expoGate)
    } # end if

    #
    nrG <- length(fdmat) # as it has an object of class 'fdmat_single' in each list element
    if (nrG > 1) {
        gaChar <- "s"
    } else {
        gaChar <- ""
    } # end else
    #
    if (verbose) {cat(paste0("Exporting data (", nrG, " gate", gaChar, ") to ", expoType, "..."))}
    gsn <- fdmat@gateStrat@filename
    fiName <- paste0(expoFolder, "/", expoName, "_", gsn, typE)
    if (expoType == "xlsx") {
        flscList <- evpvList <- vector("list", length(fdmat))
        flscNames <- evpvNames <- vector("character", length(fdmat))
        for (i in seq_along(flscList)) {
            thisGate <- fdmat[[i]]@gateName
            flscList[[i]] <- fdmat[[i]]@.Data
            flscNames[i] <- paste0(flscChar, "_", thisGate)
            evpvList[[i]] <- fdmat[[i]]@eventsPerVol
            evpvNames[i] <- paste0(evpvChar, "_", thisGate)
        } # end for i
        metaList <- list(fdmat@gateStrat, fdmat@metadata, fdmat@cyTags)
        names(metaList) <- c(fdmat@gateStrat@filename, "metadata", "cyTags")
        names(flscList) <- flscNames
        names(evpvList) <- evpvNames
        expoList <- c(metaList, flscList, evpvList)
        openxlsx::write.xlsx(expoList, fiName, rowNames=TRUE, overwrite=TRUE)
        if (verbose) {cat("ok. \n")}
    } else {
        if (length(fdmat) > 1) { # down here at writing csv we can only write data from a single gate.
            fdmat <- cutFdmatToGate(fdmat, 1)
            message("\nWhen exporting to csv, only one single gate can be exported. \nThe input object has been cut down to the first gate.\nPlease consider exporting to 'xlsx' in order to be able to export data from all gates.")
        } # end if
        write.csv(fdmat[[1]], file=fiName)
        if (verbose) {cat("ok. \n")}
    } # end else
    #
    return(invisible(NULL))
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
#' @return An object of \code{\link{class-fdmat}} containing a list holding
#' an object of \code{\link{class-fdmat_single}} in each list element, which in
#' turn contains a matrix holding the fluorescence distribution of a single gate,
#' and the overall data for events per volume unit in the slot
#' \code{eventsPerVol}.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' gs <- makeAddGatingSet()
#' fdmat <- makefdmat(gs, expo = FALSE) # to NOT export the data
#' fdmat <- makefdmat(gs)
#' @template t_ex_finale
#' @seealso \code{\link{makeAddGatingSet}}, \code{\link{flowdexit}}
#' @family Extraction functions
#' @export
makefdmat <- function(gs, name.dict=".", foN.dict=".", type.dict=".", expo=TRUE, 
    expo.gate=".", expo.name=".", expo.type=".", expo.folder=".", verbose=".",
    dev=FALSE) {
    #
    stn <- auto_up_s()
    #
    outMat <- outMd <- res <- apc <- coR <- coV <- rcv <- igp <- smo <- smN <- smP <- 
        chPrevWl <- volFac <- dictionary <- useDic <- cyTags <- NULL # some get assigned below
    assignHereStnValues(stn)
    if (!stn$dV_use_volumeData) {
        rcv <- FALSE # because if we do not want to use volume data, that implies we do not want to recalculate back to volume even if we *have* volume data
    } # end if
    #
    expoType <- checkDefToSetVal(expo.type, "dE_exportType", "expo.type", stn, checkFor="char")
    expoGate <- checkDefToSetVal(expo.gate, "dE_exportGate", "expo.gate", stn, checkFor="charNullNum")
    expoName <- checkDefToSetVal(expo.name, "fiN_dataExport", "expo.name", stn, checkFor="char")
    expoFolder <- checkDefToSetVal(expo.folder, "foN_rawData", "expo.folder", stn, checkFor="char")
    nameDict <- checkDefToSetVal(name.dict, "dD_dict_name", "name.dict", stn, checkFor="char")
    foN_dict <- checkDefToSetVal(foN.dict, "foN_dictionary", "foN.dict", stn, checkFor="char")
    verbose <- checkDefToSetVal(verbose, "dV_verbose", "verbose", stn, checkFor="logi")
    dictType <- checkDefToSetVal(type.dict, "dV_dictionaryType", "type.dict", stn, checkFor="char")
    dictTypeE <- paste0(".", dictType)
    #
    checkObjClass(object=gs, "GatingSet_fd", argName="gs")
    checkForVolumeData(gs, stn) # comes back FALSE if we do not want to use volume data.
    #
    eventsPerVol <- getEventsPerVolume(gs) # returns a list with class 'eventsPV' in each list element. Gets back a data frame NULL in each object of class 'eventsPV' if we do not want to use volume data
    #
    if (useDic) {
        checkFileExistence(foN_dict, nameDict, dictTypeE, addTxt="dictionary file ")
        dictionary <- loadGaXFile(foN_dict, nameDict, dictType)
        cyTags <- makeCyTags(gs, dictionary, stn) # extract from the sampleId column in the pheno Data
    } else { # so we do not want to use the dic :-)
        cyTags <- new("cyTags", data.frame(NULL)) # due to the strange behavior when making class-unions (??)
    }# end if useDic
    #
    gsdf <- gs@gateStrat
    gsdfUse <- gsdf[gsdf[,"keepData"],]
    nrKeep <- nrow(gsdfUse)
    outList <- vector("list", length=nrKeep)
    #
    for (i in 1: nrow(gsdfUse)) {
        gateName <- gsdfUse[i,"GateName"]
        chName <- gsdfUse[i,"extractOn"]
        gateDef <- gsdfUse[i,"GateDefinition"]
        flRange <- c(gsdfUse[i,"minRange"], gsdfUse[i,"maxRange"])
        aa <- makefdmat_single(gs, gateName, chName, res, flRange, apc, coR, coV, rcv, igp, smo, smN, smP, chPrevWl, gateDef, dev, volFac, verbose)
        outList[[i]] <- aa # returns an object of class 'fdmat_single' in each list element
    } # end for i
    #
    outMd <-  data.frame(NULL)
    for (i in seq_along(outList)) { # re-sort events per volume and collect metadata
        outList[[i]]@eventsPerVol <- eventsPerVol[[i]] # must have same length
        outMd <- rbind(outMd, outList[[i]]@metadata)
    } # end for i
    #
    fdmat <- new("fdmat", outList, metadata=outMd, cyTags=cyTags, gateStrat=gs@gateStrat, pData=flowWorkspace::pData(gs), note="original")
    #
    if (expo) {
        aaa <- try(exportFdmatData(fdmat, expoGate, expoName, expoType, expoFolder), silent=FALSE)
        if (is(aaa, "try-error")) {
            message(paste0("Sorry, exporting data to ", expoType, " was not successful."), call.=FALSE)
        } # end if
    } # end if
    return(invisible(fdmat))
} # EOF

#' @title Plot Gates on All flowFrames in a Gating Set
#' @description Plot all available gates on all flowFrames in a gating set and
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
#' @return (Invisible) NULL. Is used for its side effect, i.e. to plot gated
#' data resp. to visualize the gating strategy.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' gs <- makeAddGatingSet(gateStrat = "gateStrat_2")
#' plotgates(gs, toPdf = FALSE)
#' plotgates(gs, spl = "C_treatment", toPdf = FALSE)
#' plotgates(gs, spl = "C_treatment", plotAll = TRUE, fns = "_allGates")
#' @template t_ex_finale
#' @family Plotting functions
#' @export
plotgates <- function(gs, ti="", spl=NULL, fns=NULL, plotAll=FALSE, toPdf=TRUE,
    x=NULL, y=NULL, name.dict=".", foN.dict=".", type.dict=".", foN.plots=".") {
    stn <- auto_up_s()
    #
    foN_plots <- ""
    bins <- stn$dG_nrBins
    pdfHeight <- stn$dG_pdf_height
    pdfWidth <- stn$dG_pdf_width
    useDic <- stn$dD_useDictionary
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
        plot(ggcyto::ggcyto(gs, subset="root", ggplot2::aes_(x=x, y=y)) + 
            ggplot2::ggtitle(tiUse) + ggplot2::geom_hex(bins=bins) + 
            ggcyto::ggcyto_par_set(limits="instrument"))
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
        #
        if (!useDic) {
            stop("Sorry, it is not possible to use 'spl'. \nThere can be no cyTags as the global option to use the dictionary is set to FALSE. (key 'dD_useDictionary' in the 'flowdex_settings.R' file)", call.=FALSE)
        } # end if !useDic
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
            stop(paste0("Sorry, the provided split column '", spl, "' is not present in the provided gating set resp. its cyTags.\nPossible values are:\n'", paste0(colnames(cyTags), collapse="', '"), "'."), call.=FALSE)
        }
        txtAdd <- paste(" split by", spl)
        suffixAdd <- paste0("_by",spl)
    } # end if
    #
    if (toPdf) {cat(paste0("Plotting gates", txtAdd, "... \n"))}
    height <- pdfHeight
    width <- pdfWidth
    suffix <- paste0("Gates_", gateStrat, suffixAdd)
#    filename <- paste(expName, suffix, sep="_")
    filename <- suffix
    filename <- paste(foN_plots, "/", filename, fns, ".pdf", sep="")
    if (toPdf) { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
#    if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}
    for (i in 1: nrow(gsdf)) {
        xax <- gsdf[i,"GateOnX"]
        yax <- gsdf[i,"GateOnY"]
        gateName <- gsdf[i,"GateName"]
        subs <- flowWorkspace::gs_pop_get_parent(gs, gateName) # get the name of the parent node
        tiUse <- paste0(ti, tiAdd, subs, ", gate: ", gateName, ", using `", gsdf[i,"GateDefinition"], "`")
        # !! use "aes_" !!
        if (plotAll | gsdf[i,"keepData"]) {
            if (!is.null(spl)) {
        #        cyTagsUse <- cyTags[which(cyTags[,1] == gateName),]
                cyTagsUse <- cyTags[1:length(gs),] # we just take the first gate, as all the indices are the same in all of the gates in the cyTags
                splVals <- sort(unique(cyTagsUse[,spl]))
                for (k in seq_along(splVals)) {
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

#' @title Save Fluorescence Distribution 'fdmat' Object
#' @description Saves the R-object containing the fluorescence distributions
#' (the 'fdmat' object) in the standard data export / rawdata folder.
#' @details The name of the saved file is put together using the default name
#' for data exports (settings.R file key 'fiN_dataExport'), the name and type
#' of the gating strategy, the character 'fdmatObj' and a possible filename
#' suffix as defined in 'fns'.
#' @param fns Character length one or NULL. Possible character to be added to
#' the filename.
#' @inheritParams exportFdmatData
#' @inheritParams flowdexit
#' @return Invisible NULL; is called for its side effect, i.e. to save an
#' object of \code{\link{class-fdmat}} to file.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' fdmat <- flowdexit(stf = FALSE)
#' fd_save(fdmat, fns="_foo")
#' @template t_ex_finale
#' @family Accessory functions
#' @export
fd_save <- function(fdmat, fns=NULL, expo.folder=".", verbose=".") {
    #
    stn <- auto_up_s()
    #
    fiN_dataExport <- stn$fiN_dataExport
    fns <- checkDefToSetVal(fns, "..x..", "fns", stn, checkFor="charNull", defValue=NULL)
    expoFolder <- checkDefToSetVal(expo.folder, "foN_rawData", "expo.folder", stn, checkFor="char")
    verbose <- checkDefToSetVal(verbose, "dV_verbose", "verbose", stn, checkFor="logi")
    fdSuff <- gl_fdmatObjSuff
    #
    checkObjClass(fdmat, "fdmat", "fdmat")
    gateStrat <- fdmat@gateStrat@filename
    if (is.null(fns)) {
        fnsAdChar <- ""
    } else {
        fnsAdChar <- "_"
    } # end else
    #
    rdsName <- paste0(stn$fiN_dataExport, "_", gateStrat, fdSuff, fnsAdChar, fns)
    path <- paste0(expoFolder, "/", rdsName)
    saveRDS(fdmat, file=path)
    if (verbose) {
        cat(paste0("fdmat-object saved.\n"))
#        cat(paste0("fdmat-object saved in \n'", expoFolder, "' \nunder the name '", rdsName, "'.\n"))
    } # end if
    #
    return(invisible(NULL))
} # EOF

#' @title Load Fluorescence Distribution 'fdmat' Object
#' @description Load the R-object containing the fluorescence distributions
#' (the 'fdmat' object) from the standard data export / rawdata folder.
#' @details If 'fn' is left at NULL, the file containing the default name
#' for exported data and gating strategy is being attempted to load.
#' @param fn Character length one, the name of the file.
#' @param expo.folder The name of the folder where the file should be looked for.
#' If left at the default '.', the value as defined in the settings file (key
#' 'foN_rawData') will be used.
#' @inheritParams flowdexit
#' @return An object of \code{\link{class-fdmat}}  as produced by
#' \code{\link{makefdmat}} or \code{\link{flowdexit}}.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' fdmat <- flowdexit()
#' # and load the same again on an other shiny day... 
#' fdmat <- fd_load()
#' @template t_ex_finale
#' @family Accessory functions
#' @export
fd_load <- function(fn=NULL, expo.folder=".", verbose=".") {
    #
    stn <- auto_up_s()
    #
    expoFolder <- checkDefToSetVal(expo.folder, "foN_rawData", "expo.folder", stn, checkFor="char")
    verbose <- checkDefToSetVal(verbose, "dV_verbose", "verbose", stn, checkFor="logi")
    defName <- stn$fiN_dataExport
    defGateStrat <- stn$fiN_gateStrat
    defType <- stn$dV_gateStratInputType
    fdSuff <- gl_fdmatObjSuff
    #
    if (is.null(fn)) {
        defFileName <- paste0(defName, "_", defGateStrat, ".", defType, fdSuff)
    } else {
        defFileName <- fn
    } # end else
    pathName <- paste0(expoFolder, "/", defFileName)
    #
    if (!file.exists(pathName)) {
        stop(paste0("Sorry, the requested fdmat-object '", defFileName, "' does not seem to exist in \n'", expoFolder, "'."), call.=FALSE)
    }
    fdmat <- readRDS(file=pathName)
    if (verbose) {cat(paste0("The fdmat-object with the name `", defFileName, "` was loaded.\n"))}
    #
    return(fdmat)
} # EOF

#' @title Read in FCS Files and Extract Data
#' @description Read in fcs files from a specified folder, produce a
#' gating set, add gates as defined in the gating strategy file, extract
#' fluorescence distribution data from each gate, possibly re-calculate the
#' fluorescence distribution to events per volume unit, export all data to file
#' and save the R-object holding all the data to file as well.
#' @details While function 'flowdexit' returns fluorescence distributions
#' re-calculated to events per volume unit, the gating set that was produced
#' in the way of obtaining the fluorescence distribution data gets assigned
#' to the environment 'gsenv' under the name 'gatingSet'. Hence, it can be
#' accessed via \code{gsenv$gatingSet}. \cr \cr
#' It is paramount to obtain the correct volume factor from the help / the
#' manual of the FCM-machine that did produce the fcs files. Please see section
#' 'Calculating Events per Volume Unit' for more details.
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
#' The type of file defining the gating strategy. Currently, csv and xlsx
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
#' columns to keep from the original flowFrames; is passed down to argument
#' \code{column.pattern} of \code{\link[flowCore]{read.flowSet}}. Set to NULL
#' to read data from all channels. If left at the default '.', the value as
#' defined in the settings file (key 'dV_channel') will be used.
#' @param fcsRepair Logical. If set to TRUE, fcs-files in the folder
#' specified at argument 'fn' will be checked for multiplied entries in the
#' keywords, as after some testing the author came to the humble conclusion that
#' these multiplied keywords can be the reason for the error message: \cr
#' \code{"The HEADER and the TEXT segment define different starting
#' point ... to read the data"}. \cr
#' If 'fcsRepair' is set to TRUE, all except the last of each multiplied
#' keyword will be removed and the fcs file will be saved to disc,
#' overwriting the original fcs file \strong{without further warning}. \cr
#' Use the function \code{\link{checkRepairFcsFiles}} which does offer more
#' options to manually check and repair afflicted fcs files. There, it is
#' possible to display multiplied keywords and to select which one to keep.
#' @param expo.gate Which gate to export. NULL or numeric or character length
#' one. Set to NULL to export data from all those gates defined in the gating
#' strategy where 'keepData' is set to TRUE. Provide a character length one
#' with a gate name or the number of that gate as defined in the gating strategy
#' to export data from this gate only. If left at the default '.', the value as
#' defined in the settings file (key 'dE_exportGate') will be used.
#' @param expo.name Character length one. The name of the file holding the
#' exported fluorescence distribution(s). If left at the default '.', the value
#' as defined in the settings file (key 'fiN_dataExport') will be used.
#' @param expo Logical, if extracted data should exported at all.
#' @param expo.type Character length one. The filetype of the data export.
#' Possible values are 'csv' and 'xlsx'.  If left at the default '.', the value
#' as defined in the settings file (key 'dE_exportType') will be used.
#' @param expo.folder Character length one. The name of the folder where exported
#' should reside. If left at the default '.', the value as defined in the
#' settings file (key 'foN_rawData') will be used.
#' @param name.dict Character length one. The name of the dictionary. If left
#' at the default '.', the value as defined in the settings file (key
#' 'dD_dict_name') will be used.
#' @param foN.dict Character length one. The name of the folder where the
#' dictionary resides. If left at the default '.', the value as defined in the
#' settings file (key 'foN_dictionary') will be used.
#' @param type.dict Character length one. The filetype of the dictionary. Can
#' be one of 'csv' or 'xlsx'. If left at the default '.', the value as defined
#' in the settings file (key 'dD_dict_type') will be used.
#' @param stf Logical. If the resulting object of class 'fdmat' should be saved
#' to file in the data export folder. Defaults to TRUE.  If saved, the name
#' of the gating strategy used to generate the data will be appended to the
#' filename.
#' @section Calculating Events per Volume Unit:
#' The calculation of events per volume unit is performed via the following
#' code: \code{round((nrEvRaw / vols) * volFac , 0)}, with \code{nrEvRaw} being
#' the number of (raw) events in a specific channel as saved in the fcs file,
#' \code{vols} being the acquired volume of a sample, and \code{volFac} being
#' a factor obtained from the manual of the FCM-machine. The 'volFac' is a
#' number provided by the manufacturer of the FCM-machine / of the volumetric
#' measurement module. It is the number required to convert raw events back
#' to events per volume. It must be obtained from the manual of the FCM-machine
#' resp. the volumetric measurement module.
#' The \code{volFac} is stored in the flowdex_settings.R file
#' (key: 'dV_volumeFactor').
#' @section Regarding Compensation: Due to the circumstances when developing
#' this code, it was never required to apply any kind of compensation. The
#' functionality to apply compensation was therefore never tested or verified.
#' It is strongly advised to use caution when applying compensation. It might
#' well be necessary to modify the source code
#' (\url{https://github.com/bpollner/flowdex}) of this package in order to
#' achieve correct compensation results (compensation is applied in the
#' function \code{\link{makeGatingSet}}).
#' @section Exporting Data: If data are exported to xlsx, additional data like
#' the metadata describing the parameters that lead to the calculation of
#' the fluorescence distribution, the cyTags and the gating strategy are
#' saved in an extra sheet as well. If exporting to csv, only the fluorescence
#' data are exported.
#' @return An (invisible) object of \code{\link{class-fdmat}} containing a
#' list holding an object of \code{\link{class-fdmat_single}} in each list
#' element, which in turn contains a matrix holding the fluorescence
#' distribution of a single gate, and the overall data for events per volume
#' unit in the slot \code{eventsPerVol}.
#' @section Link: Please refer also to 
#' \url{https://bpollner.github.io/flowdex/}.
#' @template t_ex_intro
#' @template t_ex_assign
#' @examples
#' fdmat <- flowdexit()
#' fdmat2 <- flowdexit(patt = "T5", gateStrat = "gateStrat_2")
#' fdmat_small <- flowdexit(patt = "T4", expo = FALSE, stf = FALSE)
#' #
#' fdmat_small
#' fdmat_small[[1]]
#' @template t_ex_finale
#' @seealso \code{\link{flowdex}}, \code{\link{makefdmat}}
#' @export
flowdexit <- function(fn=".", patt=NULL, gateStrat=".", foN.gateStrat=".", 
    type.gateStrat=".", comp=".", tx=".", channel=".", name.dict=".", 
    foN.dict=".", type.dict=".", expo=TRUE, expo.gate=".", expo.name=".", 
    expo.type=".", expo.folder=".", fcsRepair=FALSE, stf=TRUE, verbose=".") {
    #
    stn <- auto_up_s()
    #
    checkAssignInput(stn, fn, gateStrat, foN.gateStrat, type.gateStrat, comp, tx, channel, name.dict, foN.dict, type.dict, expo.gate, expo.name, expo.type, expo.folder, verbose)    # is possibly re-assigning the values here
    #
    gsdf <- importCheckGatingStrategy(gateStrat, stn, type.gateStrat, foN.gateStrat)
    checkPggExistence(gsdf, foN.gateStrat, gateStrat)
    #
    gs <- makeGatingSet(patt, comp, fn, tx, channel, fcsRepair, verbose)
    gs <- addGates(gs, gateStrat, foN.gateStrat, type.gateStrat, verbose)
    assignGatingSetToEnv(gs)
    #
    fdmat <- makefdmat(gs, name.dict, foN.dict, type.dict, expo, expo.gate, expo.name, expo.type, expo.folder, verbose)
    #
    if (stf) {
        fd_save(fdmat, fns=NULL, expo.folder, verbose)
    } # end if
    #
    return(invisible(fdmat))
} # EOF
