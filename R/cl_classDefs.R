


# classes
setClassUnion(name="dflog", members = c("data.frame", "logical"))
#
setClass("GatingSet_fd", slots=c(gateStrat="character", gsdf="data.frame"), contains="GatingSet") # extends the class "GatingSet" from package flowWorkspace


