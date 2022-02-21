###############################################################################################
######################## Settings file for package "flowdex" ##################################
###############################################################################################

# do NOT change the name of the object holding the list - in this case, 'settings'

# use the code '.flowdex_settingsEnv$settings$KEY' (with 'KEY' being any of the key=value pairs defined below) 
# in the target package to access the values of the object 'settings'. 

#' Or (recommended) use the function 'getstn()' as defined in the package 'flowdex' to directly 
# get the list 'settings' (below).

settings <- list(
	# tag = value, # with a comma !!
		
	## general behavior
	gen_autoUpdateSettings = TRUE, 			## Do not delete this variable (but of course you can change its value)
	
	
		
	## folders and files
	folderName_gating = "gating",
	folderName_fcsFiles = "fcsFiles",
	folderName_rawData = "rawdata",
	folderName_templates = "templates", 
	
	
	
	######
	last = 0 # do not add anything below that
	## the last one without comma !!
) # end of list called 'settings'
