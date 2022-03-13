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
	dV_use_volumeData = TRUE, 				## Set to FALSE if no volume data are present at all in your fcs files. Re-calculation to events per volume unit will of course be not possible then.
	
	
		
	## default names for folders (foN) and files (fiN)
	foN_gating = "gating",					## the name of the folder where everything related to gating (gating strategy, gate definitions) reside
	foN_fcsFiles = "fcsFiles",				## the name of the folder where the fcsFiles to be read in reside
	foN_rawData = "rawdata",				## the name of the folder where data should be exported to
	foN_templates = "templates", 			## the name of the folder containing relevant templates
	foN_dictionary = "dictionary", 			## the name of the folder where the dictionary resides
	foN_plots = "plots", 					## the name of the folder where plots resp. pdfs should be saved
	fiN_gate = "polyGate",					## the default name for the polygon gate 
	fiN_gateStrat = "gateStrat", 			## the default name for the file holding the gating strategy
	
	
	
	## exporting Data
	dE_exportType = "xlsx", 				## The filetype of the exported data. Possible values are 'csv' and 'xlsx'.
	dE_exportGate = NULL, 					## The name of the gate that should be exported. Set to NULL to export data from ALL gates in the gating set where 'keepData' is set to TRUE in the gating strategy
	fiN_dataExport = "flscData", 			## the default name of the file holding the exported fluorescence distributions
	dV_charEventsPerVolume = "evpv", 		## The string, when exporting data to excel, used to name sheets holding data from events per volume unit.
	
	
		
	## default values in functions
	dV_tx = "fjbiexp", 						## which transformation to apply. Currently, only "fjbiexp" is implemented
	dV_channel = "A$", 						## which channels to import when making the gating set
	dV_comp = FALSE, 						## if compensation should be applied
	dV_gateStratInputType = "xlsx", 		## can be "csv" or "xlsx. If the file holding the gating strategy is a csv or an xlsx file.
	dV_dictionaryType = "xlsx", 			## the type of the dictionary file. Can be 'csv' or 'xlsx'.
	dV_channelsForPGG = c("FITC.A", "PerCP.A"), 	## the default channels used for manually drawing a polygon gate
	dV_channelBoundaries = c(1250, 4000, 0, 4000), 	## the boundaries in the format (x1, x2, y1, y2) to displayed as horizontal and vertical lines. 
	
	
	
	## calculation of fluorescence distribution
	dV_doRecalcToVolume = TRUE, 			## Logical. If counts should be re-calculated to volume.
	dV_volumeFactor = 1e6, 					## the volume factor as given from the manual / help of the FCS machine
	dV_volumeUnit = "ml", 					## The volume unit the re-calculation of events will lead to (i.e. on graphics etc. it will read e.g. 'events/ml')
	dV_resolution = 220, 					## Numeric length one. The resolution defining the number of bins for the histogram along the fluorescence-axis where data extraction is performed.
	dV_cutoff_raw = 10, 					## Numeric length one. The cutoff-value for the raw-events (disregarding the acquired volume). All data in any flowframe that has equal or less events in the specified gate than the value specified in dV_cutoff_raw will be set to zero.
	dV_cutoff_Vol = 125, 					## Numeric length one. The cutoff-value for the events re-calculated to events per volume. All data in any flowframe that has equal or less events/vol in the specified gate than the value specified indV_cutoff_Vol will be set to zero.
	dV_cutoff_apply = TRUE, 				## Logical. If the cutoff hould be applied or not.If set to FALSE, any flowframe containing events lower than specified in dV_cutoff_raw and dV_cutoff_Vol will NOT be set to zero.
	dV_ignoreEdgePercent = FALSE, 			##  "Ignore Edge Percent"; Logical or numeric length one. If left at the default FALSE, all data along the fluorescence-axis is taken into account. If set to a numeric, this percent of data on both edges of the fluorescence-axis is ignored.
	dV_doSmooth = TRUE, 					## Logical. If smoothing of the extracted data should be performed.
	dV_smooth_n = 11,						## The width of the smoothing window
	dV_smooth_p = 5, 						## The order of the smoothing function
	dV_charBeforeFlscNr = "flsc", 			## Character length one. The characters to be put in front of the luorescence-levels in the column name of the resulting matrix
	
	
	
	## regarding dictionary
	dD_useDictionary = TRUE, 				## If the cy-tag system and a dictionary should be used at all
	dD_dict_name = "dictionary", 			## the default name for the dictionary
	dD_dict_type = "xlsx", 					## the default type of the dictionary (can be 'csv' or 'xlsx')
	dD_elementSep = ":", 					## The character dividing the key-value pair in the sampleId
	dD_groupSep = ";", 						## The character dividing the groups in the sampleId
	dD_classVarPrefix = "C_", 				## The prefix in the column 'Long_Name' in the dictionary for class-variables, i.e. for variables holding characters
	dD_numericVarPrefix = "Y_", 			## The prefix in the column 'Long_Name' in the dictionary for numerical variables
	
	
	
	## graphic parameters
	dG_locatorLine = "darkgreen", 			## the color used to re-draw the polygon gate after having pressed 'esc'
	dG_locatorLineWidth = 1.4, 				## the line width used to re-draw the polygon gate
	dG_gateShowColor = "violet", 			## the color used for drawing a gate possibly specified at 'showGate'
	dG_pdf_height = 10,						## The height of the pdf when gates are plotted
	dG_pdf_width = 14,						## The width of the pdf when gates are plotted
	dG_nrBins = 128, 						## The number of bins applied for generating the heatmap when plotting gates
	dG_nonFluorescenceChar = c("FSC.A", "SSC.A"), ## Characters defining non-fluorescence axes when plotting. Is affecting the label on the axes.
	dG_alphaForLegends = 200, 				## When plotting fluorescence distributions, the alpha (transparence) for the legend
	dG_RcolorBrewerPal = "Dark2", 			## the default palette from RColorBrewer to generate colors when plotting fluorescence distributions
	
	
	######
	last = 0 # do not add anything below that
	## the last one without comma !!
) # end of list called 'settings'
