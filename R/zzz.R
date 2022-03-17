########################################################################################################
##################### .onLoad and .onUnload functions for package 'flowdex' ####################
########################################################################################################


######## Do NOT change the names of the variables. It will cripple the functionality of the package "uniset" ############

.onLoad <- function(libname, pkgname) {
    nsp <- "pkg_flowdex_envs"                                                ## is defining the name on the search path
    if (!any(grepl(nsp, search()))) {attach(what=NULL, name=nsp)}                    ## create a new entry on the search path if not already there
    assign(".flowdex_unisetEnv", new.env(), pos=nsp)                                        ## create a new environment called ".flowdex_unisetEnv"
    assign("pkgUniset_UserPackageName","flowdex", envir=.flowdex_unisetEnv)        ## the name of the target package using the uniset system
    assign("pkgUniset_RenvironSettingsHomeName","flowdex_SH", envir=.flowdex_unisetEnv)        ## the name of the variable in the .Renviron file that contains the path to the user-defined settings-home
    assign("pkgUniset_EnvironmentName",".flowdex_settingsEnv", envir=.flowdex_unisetEnv)        ## the name of the environment containing the settings for the package using 'uniset. It is recommended to use a rather short name starting with a '.' dot.
    assign("pkgUniset_SettingsObjectName","settings", envir=.flowdex_unisetEnv)            ## the name of the object (within the environment defined above) that is containing the settings-list. This *MUST* be the same name as the name of the list in the settings.R file 
    assign("pkgUniset_SuffixForTemplate","_TEMPLATE", envir=.flowdex_unisetEnv)        ## the character string that should be appended to the fresh settings file that could be copied to the users settings home directory
} # EOF

.onUnload <- function(libpath) {
    if (any(grepl("pkg_flowdex_envs", search()))) {detach(name="pkg_flowdex_envs")}
} # EOF
