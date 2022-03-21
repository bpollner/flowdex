# Results from rhub::check_for_cran()

## Windows Server 2022



## Ubuntu Linux



## Fedora Linux



## Regarding the Prep-Error on Ubuntu and Fedora Linux
If I am interpreting this error message correctly, then an error occurred when **preparing** to run package flowdex, not in package flowdex itself.
   


*****


# Results from devtools::check_win_devel()

* using log directory 'd:/RCompile/CRANguest/R-devel/flowdex.Rcheck'
* using R Under development (unstable) (2022-03-20 r81946 ucrt)
* using platform: x86_64-w64-mingw32 (64-bit)
* using session charset: UTF-8
* checking for file 'flowdex/DESCRIPTION' ... OK
* checking extension type ... Package
* this is package 'flowdex' version '0.4.2'
* package encoding: UTF-8
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Bernhard Pollner <bernhard.pollner@mac.com>'

New submission

Non-FOSS package license (file LICENSE)

Possibly misspelled words in DESCRIPTION:
  FCS (3:52)
  fcs (8:58, 11:40)
  flowdex (11:27)

Package has a VignetteBuilder field but no prebuilt vignette index.
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking whether package 'flowdex' can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... [20s] NOTE
Found the following calls to attach():
File 'flowdex/R/zzz.R':
  attach(what = NULL, name = nsp)
See section 'Good practice' in '?attach'.

Found if() conditions comparing class() to string:
File 'flowdex/R/gen-functions.R': if (class(gs) == "GatingSet") ...
Use inherits() (or maybe is()) instead.
* checking Rd files ... [1s] OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... [92s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
           user system elapsed
plotgates 21.99   0.80   24.64
addGates   4.82   6.08   12.01
* checking for unstated dependencies in 'tests' ... OK
* checking tests ... [72s] OK
  Running 'testthat.R' [71s]
* checking PDF version of manual ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: 3 NOTEs



*****


# Regarding the non-FOSS license
This is due to the limitations imposed by [package flowWorkspace](https://bioconductor.org/packages/release/bioc/html/flowWorkspace.html)

I am not sure if the license as I formulated it is necessary like that.
But when I set the license to GPL, then I get the note: 

Package has a FOSS license but eventually depends on the following
packages which restrict use:
  flowWorkspace, cytolib


I did try to find a standard license that says research only, not commercially, but I could not find one.

So, the license file as I formulated is the best I could do in order to encompass the requirements I believe are there due to e.g. package flowWorkspace.

If I amn wrong and the file-license as I formulated it is not necessary or correct, I would therefore kindly ask for your help to find the appropriate licensing for package flowdex.


*****

# Regarding the call to "attach()"

Please allow me to explain why I strongly believe that the call to "attach" is perfectly justified in this case:

Attach(), and also detach(), is called from the .onLoad and the .onUnload function. In this case, attach() is required for the functionality of the required package „uniset“ (of which I am the author as well.)

[https://CRAN.R-project.org/package=uniset](https://CRAN.R-project.org/package=uniset)
[https://bpollner.github.io/uniset/](https://bpollner.github.io/uniset/)



This is the code of the .onLoad and .onUnload function in package flowdex.
This code has been generated by package uniset, and it is required for the functionality provided by uniset.
```
.onLoad <- function(libname, pkgname) {
#
nsp <- "pkg_flowdex_envs"                                                
## is defining the name on the search path

if (!any(grepl(nsp, search()))) {attach(what=NULL, name=nsp)}                
## create a new entry on the search path if not already there

assign(".flowdex_unisetEnv", new.env(), pos=nsp)                                    
## create a new environment called ".flowdex_unisetEnv"

assign("pkgUniset_UserPackageName","flowdex", envir=.flowdex_unisetEnv)        
## the name of the target package using the uniset system

assign("pkgUniset_RenvironSettingsHomeName","flowdex_SH", envir=.flowdex_unisetEnv)        
## the name of the variable in the .Renviron file that contains the path to the user-defined settings-home

assign("pkgUniset_EnvironmentName",".flowdex_settingsEnv", envir=.flowdex_unisetEnv)        
## the name of the environment containing the settings for the package using 'uniset. 

assign("pkgUniset_SettingsObjectName","settings", envir=.flowdex_unisetEnv)            
## the name of the object (within the environment defined above) that is containing the settings-list.

assign("pkgUniset_SuffixForTemplate","_TEMPLATE", envir=.flowdex_unisetEnv)        
## the character string that should be appended to the fresh settings file 
#
} # EOF


.onUnload <- function(libpath) {
    if (any(grepl("pkg_flowdex_envs", search()))) {detach(name="pkg_flowdex_envs")}
} # EOF
```

As I am the author of package „uniset“, I of course checked thoroughly the success of the detachment of the previously attached environment resp. namespace-entry.. 
As it says in the „Good Practice“ rules, attach() should be followed by detach() — yes, this is the case here. As soon as the package flowdex stops using the functionality of package uniset, the entry on the search path is detached. 


It was suggested to use using namespace mechanisms then:
If ai am understanding the implications of that suggestion correctly, it says that the required objects should be exported in a namespace.
But this is not possible, as package uniset serves the target package (in my case the package flowdex) a  custom tailored environment containing custom tailored objects.  It is not possible for package uniset to have static content in its namespace; this would defeat the purpose. Package uniset has to serve custom tailored objects to the target package (flowdex in this case), hence the implementation of that in the .onLoad and .onUnload function.  As I pointed out, the content of these two functions have been written by package uniset. 

It would also defeat the purpose if package uniset would write something to be exported in the namespace of package flowdex, as then we would have a naming problem: If the name is specific, then how to let package uniset know that name? If the name is general and then hardcoded into package uniset, this would make it impossible for more than one package at the same time to use package uniset, potentially leading to errors and confusion.  (If one looks at the walk-through example at [https://bpollner.github.io/uniset/](https://bpollner.github.io/uniset) it will be clearer what I mean).



Additionally, it is not some data frame or so that gets attached, what, admittedly, could easily lead to confusing and error-prone situations. 
As one can see in the code above, what gets attached to the search path is one environment containing 5 objects with long names, containing full package name etc. The chances of these names being used again by someone else are practically zero. 

As I pointed out, the environment contains 5 objects with long and very specific names that need to be found  by package uniset.  Attaching these 5 objects in the .onLoad function (of the target package, flowdex in this case)  is the only way that I as the author of package uniset found to hand over these objects.

The use of attach() here is not in the sense as described frowned upon in the Good Practice rules. 
To sum it up, I can not think of any situation where attaching these 5 objects in their environment could lead to problems.
   
Bernhard Pollner, 21.03.2022

