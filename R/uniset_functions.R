#################################################################################################################
######################## Custom-tailored functions for package 'flowdex' ################################
#################################################################################################################

# The following three functions are custom-tailored by package 'uniset', and intended to be used inside 
# functions defined in the package 'flowdex':


# Can be used inside a function of the package 'flowdex' to manually update the settings.
# If silent=FALSE, upon success a message will be displayed.
updateSettings <- function(silent=FALSE) {
	 stn <- uniset::uniset_updateSettings(get("uniset_env_name"), setupFunc="setupSettings", silent)
	return(invisible(stn))
} # EOF



# Can be used inside a function of the package 'flowdex' to automatically update the settings.
# No message will be displayed upon success.
autoUpS <- function() {
	stn <- uniset::uniset_autoUpS(get("uniset_env_name"), setupFunc="setupSettings")
	return(invisible(stn))
} # EOF



# Can be used inside a function of the package 'flowdex' to conveniently get the list holding the settings, 
# i.e. the key=value pairs from the file flowdex_settings.R
getstn <- function(){
    stn <-  try(get("settings", envir=get(".flowdex_settingsEnv", pos="pkg_flowdex_envs")), silent=TRUE)
    if (class(stn) == "try-error") {
    	# if no manual 'updateSettings' has been called yet, this will throw an error. Hence, we have to force the 
    	# manual update here.
        uniset::uniset_updateSettings(get("uniset_env_name"), setupFunc="setupSettings", silent=TRUE)
        stn <-  try(get("settings", envir=get(".flowdex_settingsEnv", pos="pkg_flowdex_envs")), silent=FALSE)
    }
    return(stn)
} # EOF
