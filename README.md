<!-- badges: start -->
[![R-CMD-check](https://github.com/bpollner/flowdex/workflows/R-CMD-check/badge.svg)](https://github.com/bpollner/flowdex/actions)
[![codecov](https://codecov.io/gh/bpollner/flowdex/branch/main/graph/badge.svg?token=aZFS02SMwz)](https://app.codecov.io/gh/bpollner/flowdex)
[![CRAN status](https://www.r-pkg.org/badges/version/flowdex)](https://CRAN.R-project.org/package=flowdex)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/flowdex)](https://cran.r-project.org/package=flowdex)
<!-- badges: end -->


# flowdex
Extract Fluorescence Distribution Data from FCM Files and Recalculate to Events per Volume

## Description
Extract fluorescence distribution data from any bivariate distribution within a previously defined flow cytometry gating strategy and recalculate the fluorescence distribution data to events per volume. To (meaningfully) use flowdex, the FCM files have to contain volumetric measurement data denoting the acquired sample volume. 
Additionally, it is possible to generate any number of class- and numerical variables describing the dataset by providing a structured character string in the Sample-ID field of each individual sample at the time of data acquisition. At the time of reading in the FCS files, this structured character string is translated into class- und numerical variables, using a dictionary to translate any abbreviations into its long form. 
Finally, the fluorescence-distribution data that were (possibly) re-calculated to events per volume unit can be visualised and exported to file, along with data denoting the overall events per volume unit in each sample in each gate.   
`flowdex` is leaning heavily on the packages `flowCore`and `flowWorkspace` for data import, generation of gating sets etc. 

To sum it up, package flowdex can:
* Extract fluorescence distribution data from FCM files and recalculate them to events per volume unit, 
* Generate variables describing the dataset by using a structured character string in the sample-Id field of the sample at the time of data acquisition, and
* Visualise fluorescence distributions and export them to file.

## Advantage
To the authors humble knowledge, having ones hand directly on the rawdata of fluorescence distributions is not something the average desktop GUI of FCM-machines is providing easily or willingly. Package `flowdex` was designed to alleviate that problem.   
Additionally, if the FCM-machine used for data acquisition does have a volumetric measurement module and the FCS files contain volumetric data, it is possible to re-calculate any extracted fluorescence distribution to events per volume unit. In other words, the y-axis of any fluorescence distribution can be displayed in a fixed scale, i.e. events per volume unit. This enables the comparison of numbers on a fixed scale across samples, groups or experiments, instead of being confined to compare percentages of sub-groups of an overal population.


## Installation
Install release version from CRAN via
```
install.packages("flowdex") 
```
Or download from github:
```
library(devtools)
install_github(repo="bpollner/flowdex", ref="main")
```
   
***
   
# Usage
In order to walk the user over the main-features of package `flowdex`, a small tutorial-dataset has been created, along with gating strategy files and the required polygon-gate definitions. Also, the workflow how to create the gating strategy file and the polygon gate definitions will be explained. 

## Set up Tutorial
First, download the tutorial data from its Github repository to your desktop (or any place you prefer). 
```
from <- "https://github.com/bpollner/data/raw/main/flowdexTutorial/flowdexTutorial.zip"
destination <- "~/desktop"
targetZip <- paste0(destination, "/flowdexTutorial.zip")
download.file(from, targetZip, mode="wb")
unzip(targetZip, exdir = destination)
```
(All the code below is also available in the file 'tutorialScript.R' in the uncompressed folder.)  

#### Note: Description of Tutorial Dataset
* All samples are tap water samples stained with cybergreen following the method as described in:   
Prest, E. I., Hammes, F., Kotzsch, S., van Loosdrecht, M. C., & Vrouwenvelder, J. S. (2013). Monitoring microbiological changes in drinking water systems using a fast and reproducible flow cytometric method. Water Research, 47(19), 7131-7142 https://doi.org/10.1016/j.watres.2013.07.051
* It was the purpose of the experiment to monitor the number of autochthonous bacteria in the water samples and their fluorescence distribution (LNA vs. HNA bacteria) over time.
* A sample-ID string has been provided at the time of data acquisition, providing class and numerical variables describing each sample as described below.
* There are two groups, experiment and control. The experiment group is denoted as 'GPos', the control group is denoted as 'GNeg'.
* From each group, 24 samples were taken each day, from which 18 samples are included In this dataset. 
* FCS files from day 4, day 5 and day 6 are included (denoted as T4, T5, and T6).
* The 18 samples per day from each group are further structured in 'thirds' and beaker number: th1, th2 and th3 for the first, second and third third of the 18 samples, b1, b2, … up to b6 for the 6 samples (b for beaker) within each third.
   
### Initialise Settings File System
Package `flowdex` makes use of the dynamic settings file system provided by package `uniset`. This has to be initialised **just once** by calling
```
library(flowdex)
destination <- "~/desktop"
setupSettings(destination)
```
Follow the on-screen instructions and possibly re-start R.  
From now on, various global settings and default values used by `flowdex` can conveniently be viewed and changed in the file `flowdex_settings.R` residing at `~/desktop/flowdex_SH` (if you kept it there as in the example).

***
### Quickstart
If you want a quickstart to immediately see what `flowdex` can do, call:
```
destination <- "~/desktop" # again, as R was possibly restarted
setwd(paste0(destination, "/flowdexTutorial"))
library(flowdex)
fdmat <- flowdexit() # this might take a few seconds
fdmat@pData # to inspect volume and sample ID data
fdmat@cyTags # to inspect class- and numerical variables assigned to each sample
fdmat[[1]] # to inspect fluorescence distribution
```
Look at 'flowdexTutorial/rawdata/flscData_gateStrat.xlsx.xlsx' to see the fluorescence distribution exported to file, observe the various sheets there.

***

### Prepare Folder Structure
Take it step by step, not interested in the quickstart above: 
We assume that data from a single experiment will be residing in a single folder -- the 'home' folder for this experiment.   
Within this home-folder, `flowdex`requires a simple folder structure to properly work. Create that folder structure now within a newly created experiment home-folder called 'tapWater@home':
```
destination <- "~/desktop" # again, as R was possibly restarted
expHome <- paste0(destination, "/tapWater@home") 
dir.create(expHome)
setwd(expHome)
library(flowdex) # as you might have had to restart R above
genfs()
```

### Acquire Data
Now would be the time for data acquisition -- go and measure your samples, then come back and put your FCS files into the folder 'fcsFiles'.   
Simulate data acquisition now by manually copying the files from 'flowdexTutorial/fcsFiles' to 'tapWater@home/fcsFiles', or call
```
from <- list.files(paste0(destination, "/flowdexTutorial/fcsFiles"), full.names = TRUE)
to <- paste0(destination, "/tapWater@home/fcsFiles")
file.copy(from, to)
```

#### Using Structured ID String and Dictionary
If so desired, it is possible to generate class- and numerical variables further describing each sample resp. the dataset by providing a structured character string to the **Sample-ID field** of each sample in the GUI of the FCM-machine **at the time of data acquisition**. This character string will then later be expanded using a dictionary (located in 'tapWater@home/dictionary') to expand the abbreviations to its long names.   
As this step, the assigning of the structured character string to each sample, has to be done at the time of data acquisition in the GUI of the FCM-machine, we´d like to explain the process by showing and describing what was used in the case of our tutorial data:   
Lets consider for example the sample named 'N_na_GPos_T4_th1_b3.fcs'. This is a sample of the experiment-group ('GPos'), from day 4 ('T4'), first third of beakers ('th1'), and from that the beaker number 3 ('b3'). The structured character string now contains **elements and groups**, each separated by a dedicated single character. The default value for separating groups is ';' and that for separating elements is ':'.   
There can be as many groups as desired. A single group consists of exactly two elements: the key and the value, separated per default by ':'.
For the sample above, the input in the sample-ID field in the GUI of the FCM-machine at the time of data acquisition now would be:
```
tr: GPos; Td: 4; wt: nativ; ap: no; th: th1; ha: ha1; bk: b3 
```

Here, we have 7 groups: tr, Td, wt, ap, th, ha and bk, all separated by a ';'. 'tr' e.g. will be expanded to 'C_treatment' as defined in the dictionary. 'GPos' is the value in the first group, meaning that this sample belongs to the experiment group (denoted as 'GPos'). The next group 'Td: 4' means that the treatment time for this sample was 4 days, and so on.

* The first part of an element is always the short name, the abbreviation that gets expanded to its long name using the dictionary. The second part of the element is its actual value. The first part can be thought of as the column name in a spreadsheet, while the second part is the actual value within that column.   
* The order of the groups is not relevant, they can be in different order for different samples (but as this could be confusing during data acquisition, this is not recommended). If an erroneous sample ID is provided, it can be repaired later using the function `repairSID`.   
* The data, i.e. the class- and numerical variables assigned to each sample can later be accessed via the slot `cyTags` in the object created by e.g. function `flowdexit`, and they will also be exported to file (if exporting to xlsx). These data can be very helpful and convenient when further analysing the fluorescence distributions.

Considering the sample-IDs present in our tutorial fcs files, we require a dictionary translating the present abbreviations to their long names. For that, manually copy the dictionary from `flowdexTutorial/dictionary/dictionary.xlsx` to `tapWater@home/dictionary`, or call
```
from <- list.files(paste0(destination, "/flowdexTutorial/dictionary"), full.names = TRUE)
to <- paste0(destination, "/tapWater@home/dictionary")
file.copy(from, to)
```
Open the copied xlsx file, look at the column named 'Abbreviation': for each element name (the first part of an element) in the sample ID there is an entry in the dictionary, along with its  long name (the final 'column name' when thinking in a spreadsheet).   
* By prepending the long names with dedicated strings it is possible to define each group as being either a class-variable or a numerical variable.
* The default string for class variables is 'C_', and that for numerical variables is 'Y_'. 
* Prepending the long names in the dictionary with either class-variable or numerical variable prefix is mandatory.
    
By now we should have 108 FCS files in 'tapWater@home/fcsFiles' and one file 'dictionary.xlsx' in the folder 'tapWater@home/dictionary'.


## Two Workflow Scenarios
There are two basic scenarios when using `flowdex` that further define the appropriate workflow:
* A) Gating strategy and polygon gates are **not yet defined**.  
Here, the focus is on establishing the gating strategy and manually drawing the polygon gates.  
  
* B) Gating strategy and polygon gates are **already defined**.  
Here, the focus is on using the previously established gating strategy to extract fluorescence distributions, visualise them and export them to file.    
All this (except visualisation) is conveniently done via `flowdexit`. Look at [Quickstart](#quickstart) for an immediate example. 


### Workflow A: Define Gating Strategy and Polygon Gates
The next step after data acquisition is to visualize the raw-counts, to decide which sample to use for drawing the gate, and then to manually draw the polygon gate(s) and safe them for later use. Each row in the gating strategy file defines a single gate along with its polygon gate definition. Gates can be nested or 'standalone'.    

#### Read in FCS Files and Make Gating Set
Before drawing a polygon gate on a single sample, we have to read in some FCS files and produce the gating set.
```
gsAll <- makeGatingSet()
```
If everything was left at the default, this will read in all 144 fcs-files contained in the folder 'fcsFiles'.   
For the purpose of this demonstration we will restrict the fcs files to be read in to a lower number by defining a filename pattern: Only fcs files with matching pattern will be read in. 
```
gsRed <- makeGatingSet(patt="GPos_T6_th2")
gsRed
```
Note the printout of the samples contained in the gating set and of the available channels.   
Only fcs files from the experiment group from day 6 (second third, i.e. 6 beakers) were read in.
We can now use this reduced gating set to visualise the raw fcs data by specifying the channel we want for the x and for the y axis:
```
plotgates(gsRed, toPdf = FALSE, x="FITC.A", y="PerCP.A")
```


#### Manually Draw Polygon Gate
All 6 samples seem to have a good representation of our desired population, the stained bacteria. Lets assume we choose beaker 'GPos_T6_th2_b5', which is the 5th sample in this gating set, as sample to manually draw the polygon gate on:
```
drawGate(gsRed, flf=5 gn="root", pggId="BactStainV1", channels=".")
# point and click to draw a polygon gate around the population of interest
```

`gn`is the name of the gate we want to use to display data. As no gates are yet defined let alone added to the gating set, we leave that at the default 'root'.    
`pggId` specifies the name the new polygon gate should be saved under. If left at the default, 'polyGate' will be used.   
`channels` is specifying which channels we want to be displayed on the x and y axis. Defaults to 'c("FITC.A", "PerCP.A")'.   
The lines displayed in the graphic have their origin in the `bnd` argument, defining the boundaries to be displayed.   
(All default values can be easily changed in the 'flowdex_settings.R' file located at '~/desktop/flowdex_SH'.)    
   
If you are not satisfied with the polygon gate definition, you can draw it again while simultaneously displaying an other (the old) gate:
```
drawGate(gsRed, flf=5, gn="root", pggId="BactStainV1", show="BactStainV1") 
# point and click to draw an improved polygon gate around the population of interest
```
Polygon gate definitions get automatically saved under the specified name in the folder 'gating'.   

   
You can leave the gate definition of 'BactStainV1' as you were drawing it, or you can copy the gate definition that was originally drawn for this project:
```
from <- paste0(destination, "/flowdexTutorial/gating/BactStainV1")
to <- paste0(destination, "/tapWater@home/gating")
file.copy(from, to, overwrite = TRUE)
```


#### Write Gating Strategy

For each gate that you want to apply to your gating set, there has to be one row in the gating strategy file.
Now copy the template for the gating strategy file into the experiment home folder:
```
from <- paste0(destination, "/tapWater@home/templates/gateStrat.xlsx")
to <- paste0(destination, "/tapWater@home/gating")
file.copy(from, to)
```
Open the copied file. For every gate, i.e. every row, all 8 fields have to filled in.
* The 'GateName' is, obviously, the name of the gate. Short names recommended.
* 'Parent' is the name of the parent-gate, i.e. the gate where the data are coming from. In the first row, this has to be 'root'.
* 'GateOnX' and GateOnY' define the channels to be used for that gate in the x and y axis.
* 'GateDefinition' is the name of the polygon gate manually drawn in the previous step. 
* 'extractOn' defines on which channel, i.e. along which axis the fluorescence data should be extracted. Must be either the channel used for the x axis or the channel used for the y axis.
* 'minRange' and 'maxRange' define the lower and upper limit of the channel where data are extracted.
* 'keepData' is denoting whether the data from this gate are to be kept or not. Set this field to FALSE when the data of this gate are not of interest and it is merely used to prepare the data for an other, a nested gate. There has to be at least one field in 'keepData' set to TRUE.
* The column names must not be changed.


For the example at hand, fill in the gating strategy file as follows:
```
GateName: DNA+
Parent: foot
GateOnX: FITC.A
GateOnY: PerCP.A
GateDefinition: BactStainV1
extractOn: FITC.A
minRange: 1250
maxRange: 4000
keepData: TRUE
```
Or copy the already filled out gating strategy file defining the gate 'DNA+' with its polygon gate definition 'BactStainV1' from the tutorial folder:
```
from <- paste0(destination, "/flowdexTutorial/gating/gateStrat.xlsx")
to <- paste0(destination, "/tapWater@home/gating")
file.copy(from, to, overwrite=TRUE)
```
   
Now everything should be ready to apply the gating strategy to the gating set:
```
gsRed_ga <- addGates(gsRed)
flowWorkspace::plot(gsRed_ga) # to view the gate hierarchy 
plotgates(gsRed_ga, toPdf = FALSE) # to view the gated data
```

#### Draw Gate, Add to Gating Strategy, Add Gate to Gating Set – and Repeat
When more than one gate should be defined in the gating strategy, the circle of 
* drawing the gate on a single sample,
* adding that gate to the gating strategy, and
* adding that gate to the gating set   
is repeated for every gate that should be  applied to the fcs data.

In the next iteration, i.e. when for example a second gate within the data from the first gate should be defined, one would call:

```
drawGate(gsRed_ga, flf=7, gn="DNA+", pggId="pg2", channels = c("FITC.A", "SSC.A"))
# point and click to draw a polygon gate around the population of interest

# add the gate in the gating strategy file:
GateName: FooGate
Parent: DNA+
GateOnX: FITC.A
GateOnY: SSC.A
GateDefinition: pg2
extractOn: SSC.A
minRange: 0
maxRange: 4000
keepData: TRUE

gsRed_ga <- addGates(gsRed_ga) # add the gate to the gating set 
```

Repeat these three steps to define any set of possibly nested gates.
You can always view the gating hierarchy and the gated data via:

```
flowWorkspace::plot(gsRed_ga) # to view the gating hierarchy
plotgates(gsRed_ga, toPdf = F) # to view the gated data
```

Now copy a ready made example for a gating strategy holding more than one gate from the tutorial folder in the experiment home folder
```
from <- list.files(paste0(destination, "/flowdexTutorial/gating"), full.names = TRUE)
to <- paste0(destination, "/tapWater@home/gating")
file.copy(from, to, overwrite=TRUE) # this might overwrite the polygon gate definition you created above
```
and apply it to a gating set. For ease of demonstration, we will again use the subset of the fcs files as above.   
Reading in the fcs files and adding the gates can be conveniently done using `makeAddGatingSet`:
```
gsRed2 <- makeAddGatingSet(patt="GPos_T6_th2", gateStrat = "gateStrat_2")
# we are not using the default name for gating strategy any more
flowWorkspace::plot(gsRed2) # to view the gating hierarchy
plotgates(gsRed2, toPdf = F) # to view the gated data
```
Note that only those gated data get displayed in 'plotgates' where the field 'keepData' in the gating strategy file is set to 'TRUE'.   
(Admittedly, the practical value of this gating hierarchy is rather non-existent, but it is merely for demonstrating the setup of nested gates...)

***

### Workflow B: Extract Fluorescence Distributions


#### Flowdexit
XXX
XXX



#### Visualize Gates
XXX
XXX
explain the split



#### Visualize Fluorescence Distribution
XXX
XXX
explain the split



***


## Accessory Functions

### Check and Repair FCS Files
checkRepairFcsFiles



### Repair Sample ID and Volume Data
In case it happened that the volumetric measurement of a single sample did not succeed, or that an erroneous sample-ID string was provided in the sample-ID field of a single sample at the time of data acquisition, there are two functions to remedy these issues: `repairVolumes`and `repairSID`.  
XXX


### Apply Bandpass Filter
XXX
apply filter, plot distribution and export to rawdata file.

***
## Acknowledgements
This work was made possible by support from IPF  Austria - Georg Huber.   
Flow cytometry data were acquired at the Institute for Hygiene and Medical Microbiology, Medical University of Innsbruck, Austria.
