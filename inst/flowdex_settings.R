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
	fiN_gate = "polyGate",					## the default name for the polygon gate 
	fiN_gateStrat = "gateStrat", 			## the default name for the file holding the gating strategy
	
	
	
	## default values in functions
	dV_tx = "fjbiexp", 						## which transformation to apply. Currently, only "fjbiexp" is implemented
	dV_channel = "A$", 						## which channels to import when making the gating set
	dV_comp = FALSE, 						## if compensation should be applied
	dV_gateStratInputType = "csv", 			## can be "csv" or "xlsx. If the file holding the gating strategy is a csv or an xlsx file.
	dV_rawDataOutputType = "csv", 			## the format of the rawdata output. Can be "csv" or "xlsx".
	dV_channelsForPGG = c("FITC.A", "PerCP.A"), 	## the default channels used for manually drawing a polygon gate
	dV_channelBoundaries = c(1250, 4000, 0, 4000), 	## XXX
	
	
	## calculations
	dV_volFac = 100000, 					## the volume factor as given from the manual / help of the FCS machine
	
	
	
	## graphic parameters
	dG_locatorLine = "darkgreen", 			## the color used to re-draw the polygon gate after having pressed 'esc'
	dG_locatorLineWidth = 1.4, 				## the line width used to re-draw the polygon gate
	dG_gateShowColor = "violet", 			## the color used for drawing a gate possibly specified at 'showGate'
	
	
	
	######
	last = 0 # do not add anything below that
	## the last one without comma !!
) # end of list called 'settings'
