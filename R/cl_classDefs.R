#' @include flowdex.R
#' @include cl_classFunctions.R



#### classes ####
setClassUnion(name="dflog", members = c("data.frame", "logical"))
#
setClass("gatingStrategy_fd", slots=c(filename="character"), contains="data.frame")
setClass("GatingSet_fd", slots=c(gateStrat="gatingStrategy_fd"), contains="GatingSet") # extends the class "GatingSet" from package flowWorkspace





#### methods ####
setMethod("show", signature(object = "GatingSet"), definition = show_GatingSet_flowWorkspace)
setMethod("show", signature(object = "GatingSet_fd"), definition = show_GatingSet_fd )
