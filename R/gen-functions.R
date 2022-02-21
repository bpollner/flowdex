#' @title Perform settings setup
#' @description Perform the setup of the setting-system provided by package 
#' 'uniset'.
#' @details Has to be done only once.
#' @return No return value, is called for its side effects, i.e. to set up 
#' the settings-file system as provided by package 'uniset'.
#' @export
setupSettings <- function() {
	path <- "/Users/bernhard/Documents/RPS/flowdex_R/flowdex_Settings"
	uniset::uniset_setup(where=path, get("uniset_env_name"))
	return(invisible(NULL))
} # EOF


#' @title Test
#' @description Test
#' @export
test <- function() {
	print("Hello World")
} # EOF


