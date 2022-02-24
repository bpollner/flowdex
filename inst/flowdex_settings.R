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
	dV_verbose = TRUE,						## default value for 'verbose'; if status messages should be displayed

	
		
	## default names for folders (foN) and files (fiN)
	foN_gating = "gating",
	foN_fcsFiles = "fcsFiles",
	foN_rawData = "rawdata",
	foN_templates = "templates", 
	fiN_gate = "", 
	
	
	## default values in functions
	dV_tx = "fjbiexp", 						## which transformation to apply. Currently, only "fjbiexp" is implemented
	dV_channel = "A$", 						## which channels to import when making the gating set
	dV_comp = FALSE, 						## if compensation should be applied
	
	
	## calculation of events / volume
	dV_volFac = 100000, 						## the volume factor as given from the manual / help of the FCS machine
	
	
	######
	last = 0 # do not add anything below that
	## the last one without comma !!
) # end of list called 'settings'
