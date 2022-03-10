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
from <- "https://github.com/bpollner/data/raw/main/myLFS/flowdexTutorial.zip"
destination <- "~/desktop"
targetZip <- paste0(destination, "/flowdexTutorial.zip")
download.file(from, targetZip, mode="wb")
unzip(targetZip, exdir = destination)
```
(All the code below is also available in the file 'tutorialScript.R' in the uncompressed folder.)  

#### Note: Description of Tutorial Dataset
* All samples are tap water samples stained with cybergreen following the method as described in: Prest, E. I., Hammes, F., Kotzsch, S., van Loosdrecht, M. C., & Vrouwenvelder, J. S. (2013). Monitoring microbiological changes in drinking water systems using a fast and reproducible flow cytometric method. Water Research, 47(19), 7131-7142 https://doi.org/10.1016/j.watres.2013.07.051
* It was the purpose of the experiment to monitor the number of autochthonous bacteria in the water samples and their fluorescence distribution (LNA vs. HNA bacteria) over time.
* A sample-ID string has been provided at the time of data acquisition, providing class and numerical variables describing each sample as described below.
* There are two groups, experiment and control. The experiment group is denoted as 'GPos', the control group is denoted as 'GNeg'.
* From each group, 24 samples were taken each day. In this dataset, FCS files from day 4, day 5 and day 6 are included (denoted as T4, T5, and T6).
* The 24 samples per day from each group are further structured in 'thirds' and beaker number: th1, th2 and th3 for the first, second and third third of the 24 samples, b1, b2, … up to b8 for the 8 samples (b for beaker) within each third.
   
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
    
By now we should have 144 FCS files in 'tapWater@home/fcsFiles' and one file 'dictionary.xlsx' in the folder 'tapWater@home/dictionary'.


## Two Workflow Scenarios
There are two basic scenarios when using `flowdex` that further define the appropriate workflow:
* A) Gating strategy and polygon gates are **not yet defined**.  
Here, the focus is on establishing the gating strategy and manually drawing the polygon gates.  
  
* B) Gating strategy and polygon gates are **already defined**.  
Here, the focus is on using the previously established gating strategy to extract fluorescence distributions, visualise them and export them to file.    
All this (except visualisation) is conveniently done via `flowdexit`. Look at [Quickstart](#quickstart) for an immediate example. 


### Workflow A: Define Gating Strategy and Polygon Gates
The next step after data acquisition is to visualize the raw-counts, to decide which sample to use for drawing the gate, and then to manually draw the polygon gate(s) and safe them for later use. Each row in the gating strategy file defines a single gate along with its polygon gate definition. Gates can be nested or 'standalone'.    

#### Read FCS Files and Make Gating Set
Before drawing a polygon gate on a single sample, we have to read in some FCS files and produce the gating set.
```
gsAll <- makeGatingSet()
```
If everything was left at the default, this will read in all 144 fcs-files contained in the folder 'fcsFiles'.   
For the purpose of this demonstration we will restrict the fcs files to be read in to a lower number by defining a filename pattern: Only fcs files with matching pattern will be read in. 
```
gsRed <- makeGatingSet(patt="GPos_T4_th1")
gsRed
```
Thus, only fcs files from the experiment group from day 4 (first third, i.e. 8 beakers) will be read in.
We can now use this reduced gating set to visualise raw counts:
```

XXX
```




* The gating strategy file is defining the gating strategy. Every row contains one gate with one polygon gate definition. 
* Gates can be nested
In the gating strategy file (can be an xlsx or csv file) the gating strategy is defined: 


If more than one gate is required, 


explain workflow how to create complex gating strategy; create nested gates
Step by Step gating hin und her Gating Strategy; define gate, draw Gate.. build it up




### Workflow B: Extract Fluorescence Distributions


## Flowdexit
XXX
XXX



## Repair Sample ID and Volume Data
In case it happened that the volumetric measurement of a single sample did not succeed, or that an erroneous sample-ID string was provided in the sample-ID field of a single sample at the time of data acquisition, there are two functions to remedy these issues: `repairVolumes`and `repairSID`.  
XXX



## Visualize Gates
XXX
XXX



## Visualize Fluorescence Distribution
XXX
XXX




***
### Acknowledgements
This work was made possible by support from IPF  Austria - Georg Huber.   
Flow cytometry data were acquired at the Institute for Hygiene and Medical Microbiology, Medical University of Innsbruck, Austria.
