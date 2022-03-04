#' @include flowdex.R
#' @include cl_classFunctions.R



#### classes ####
setClassUnion(name="dfNull", members = c("data.frame", "NULL"))
#
setClass("gatingStrategy_fd", slots=c(filename="character"), contains="data.frame")
setClass("GatingSet_fd", slots=c(gateStrat="gatingStrategy_fd"), contains="GatingSet") # extends the class "GatingSet" from package flowWorkspace
#
setClass("fdmat", slots=c(metadata="data.frame", cyTags="dfNull", eventsPerVol="dfNull", gateStrat="gatingStrategy_fd",  pData="data.frame", note="character"), contains="matrix")




#### methods ####
setMethod("show", signature(object = "GatingSet"), definition = show_GatingSet_flowWorkspace)
setMethod("show", signature(object = "GatingSet_fd"), definition = show_GatingSet_fd )
setMethod("show", signature(object = "fdmat"), definition = show_fdmat )


#setGeneric("plot", function(x, y, ...) standardGeneric("plot"))


#setMethod("plot", signature(x = "GatingSet_fd", y = "missing"), definition = plot_gateStructure )

	##' @rdname plot_counts
	##' @export
	#setMethod("plot", signature(x="fdmat", y="missing"), definition = plot_CountsFdMat_M)
