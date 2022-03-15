#' @include flowdex.R
#' @include cl_classFunctions.R



#### classes ####

# setClassUnion(name="dfNull", members=c("data.frame", "NULL"))

#' @title Class 'eventsPV'
#' @description Contains a data frame holding the overall events per volume
#' unit.
#' @slot .Data A data frame holding the overall events per volume
#' unit.
#' @slot gateName Character. The name of the corresponding gate,
#' @slot volumeUnit The character string denoting the volume unit. \cr
#' The volume unit is, in the end, defined by the volume factor as provided in
#' the flowdex.settings.R file (key: 'dV_volumeFactor')
#' @family Class documentation
#' @name class-eventsPV
setClass("eventsPV", slots=c(gateName="character", volumeUnit="character"), contains="data.frame")


#' @title Class 'cyTags'
#' @description Contains a data frame holding the cyTags, i.e. the class and
#' the numerical variables that were possibly extracted from the structured
#' sample ID of each sample.
#' @slot .Data A data frame containing the cyTags
#' @family Class documentation
#' @name class-cyTags
setClass("cyTags", contains="data.frame")


#' @title Class 'gatingStrategy_fd'
#' @description Contains a data frame with the gating strategy.
#' @slot .Data A data frame containing the gating strategy as defined in the
#' gating strategy file.
#' @slot filename The filename of the gating strategy file.
#' @family Class documentation
#' @name class-gatingStrategy_fd
setClass("gatingStrategy_fd", slots=c(filename="character"), contains="data.frame")


#' @title Class 'GatingSet_fd'
#' @description Is extending the class \code{\link[flowWorkspace]{GatingSet}}.
#' @slot .Data An object of class \code{\link[flowWorkspace]{GatingSet}}
#' @slot gateStrat An object of  \code{\link{class-gatingStrategy_fd}}
#' @family Class documentation
#' @name class-GatingSet_fd
setClass("GatingSet_fd", slots=c(gateStrat="gatingStrategy_fd"), contains="GatingSet") # extends the class "GatingSet" from package flowWorkspace
#
# setClassUnion(name="evpvNull", members = c("eventsPV", "NULL")) ## Why not work ??



#' @title Class 'fdmat_single'
#' @description Contains a matrix with the fluorescence distribution from data
#' coming from a single gate.
#' @slot .Data A matrix containing the fluorescence distributions
#' @slot eventsPerVol An object of \code{\link{class-eventsPV}}
#' @slot gateName Character. The name of the gate where the data used to calculate
#' the fluorescence distributions came from.
#' @slot metadata A data frame holding the metadata, i.e. the parameters used
#' to calculate the fluorescense intensities. Most of these parameters can be set
#' in the file 'flowdex_settings.R'.
#' @slot ncpwl Numeric. The number of characters before the actual fluorescence
#' intensities in the column names of the matrix at \code{.Data}.
#' @slot note Character. Displaying comments like e.g. if
#' \code{\link{applyBandpass}} was applied to the data.
#' @family Class documentation
#' @name class-fdmat_single
setClass("fdmat_single", slots=c(eventsPerVol="eventsPV", gateName="character", metadata="data.frame", ncpwl="numeric", note="character"), contains="matrix")
#
# setClassUnion(name="cytNull", members = c("cyTags", "NULL")) ## Why not work ?? sometimes getting the recache error "definitions not updated" message.


#' @title Class 'fdmat'
#' @description Contains a list, and within each list element there is an object
#' of class 'fdmat_single', holding in turn the fluorescence distributions. For
#' each gate in the gating strategy where 'keepData' is set to TRUE there is
#' one list element in 'fdmat'.
#' @slot .Data A list with the same length as there are gates, i.e. with the
#' same length as there are rows in the data frame in the slot \code{metadata}.
#' @slot metadata A data frame holding the metadata, i.e. the parameters used
#' to calculate the fluorescense intensities. Most of these parameters can be set
#' in the file 'flowdex_settings.R'.
#' @slot cyTags An object of \code{\link{class-cyTags}}
#' @slot gateStrat An object of \code{\link{class-gatingStrategy_fd}}
#' @slot pData A data frame holding the pheno-data, i.e. the data showing the
#' raw sample-ID and the volume data stored in each single fcs file.
#' @slot note Character. Displaying comments like e.g. if
#' \code{\link{applyBandpass}} was applied to data from a gate.
#' @family Class documentation
#' @name class-fdmat
setClass("fdmat", slots=c(metadata="data.frame", cyTags="cyTags", gateStrat="gatingStrategy_fd",  pData="data.frame", note="character"), contains="list")


#### methods ####
setMethod("show", signature(object = "GatingSet"), definition = show_GatingSet_flowWorkspace)
setMethod("show", signature(object = "GatingSet_fd"), definition = show_GatingSet_fd )

setMethod("show", signature(object = "fdmat_single"), definition = show_fdmat_single )
setMethod("show", signature(object = "fdmat"), definition = show_fdmat )



setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
# setMethod("plot", signature(x = "GatingSet_fd", y = "missing"), definition = plot_gateStructure )
