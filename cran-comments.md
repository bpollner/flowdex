# Results from rhub::check_for_cran()

## Windows Server 2022

Build ID:
flowdex_0.4.2.tar.gz-e7f3cd4d05f24fe39e21decc879eb5d0

I got emailed back a PREPERROR, and it could look like there is a problem 
setting up the build-environment ??

In the output, it says:

  51#> VERBOSE: Downloading https://builder.r-hub.io/file/087ed903c6354d0c9cbff55121621eab
  52#> VERBOSE: GET https://builder.r-hub.io/file/087ed903c6354d0c9cbff55121621eab with 0-byte payload
  53#> VERBOSE: received 898229-byte response of content type application/octet-stream
  54#> FATAL: no longer a configured node for windows-ucrt-80153829
  55#> java.lang.IllegalStateException: no longer a configured node for windows-ucrt-80153829

Please not that devtools::check_win_devel() goes through without a problem, 
see below.

Also the run on Windows 2019 as provided by Github is ok:
[https://github.com/bpollner/flowdex/actions/runs/2016601930](https://github.com/bpollner/flowdex/actions/runs/2016601930)




## Ubuntu Linux

Build ID:
flowdex_0.4.2.tar.gz-bd945804e266441cb289de75781bdf80

On the results I got emailed back it says "PREPERROR". but when I look at the 
file R-CMD-CHECK goes through as follows:

### output from ubuntu linux
19702#> About to run xvfb-run R CMD check --as-cran flowdex_0.4.2.tar.gz
19703#> 'getOption("repos")' replaces Bioconductor standard repositories, see
19704#> '?repositories' for details
19705#> replacement repositories:
19706#> CRAN: https://cloud.r-project.org
19707#> * using log directory ‘/home/docker/flowdex.Rcheck’
19708#> * using R version 4.1.2 (2021-11-01)
19709#> * using platform: x86_64-pc-linux-gnu (64-bit)
19710#> * using session charset: UTF-8
19711#> * using option ‘--as-cran’
19712#> * checking for file ‘flowdex/DESCRIPTION’ ... OK
19713#> * checking extension type ... Package
19714#> * this is package ‘flowdex’ version ‘0.4.2’
19715#> * package encoding: UTF-8
19716#> * checking CRAN incoming feasibility ... NOTE
19717#> Maintainer: ‘Bernhard Pollner ’
19718#> New submission
19719#> Non-FOSS package license (file LICENSE)
19720#> Possibly mis-spelled words in DESCRIPTION:
19721#> fcs (8:58, 11:40)
19722#> FCS (3:52)
19723#> flowdex (11:27)
19724#> Package has a VignetteBuilder field but no prebuilt vignette index.
19725#> * checking package namespace information ... OK
19726#> * checking package dependencies ... OK
19727#> * checking if this is a source package ... OK
19728#> * checking if there is a namespace ... OK
19729#> * checking for executable files ... OK
19730#> * checking for hidden files and directories ... OK
19731#> * checking for portable file names ... OK
19732#> * checking for sufficient/correct file permissions ... OK
19733#> * checking whether package ‘flowdex’ can be installed ... OK
19734#> * checking installed package size ... OK
19735#> * checking package directory ... OK
19736#> * checking for future file timestamps ... OK
19737#> * checking DESCRIPTION meta-information ... OK
19738#> * checking top-level files ... OK
19739#> * checking for left-over files ... OK
19740#> * checking index information ... OK
19741#> * checking package subdirectories ... OK
19742#> * checking R files for non-ASCII characters ... OK
19743#> * checking R files for syntax errors ... OK
19744#> * checking whether the package can be loaded ... OK
19745#> * checking whether the package can be loaded with stated dependencies ... OK
19746#> * checking whether the package can be unloaded cleanly ... OK
19747#> * checking whether the namespace can be loaded with stated dependencies ... OK
19748#> * checking whether the namespace can be unloaded cleanly ... OK
19749#> * checking loading without being on the library search path ... OK
19750#> * checking use of S3 registration ... OK
19751#> * checking dependencies in R code ... OK
19752#> * checking S3 generic/method consistency ... OK
19753#> * checking replacement functions ... OK
19754#> * checking foreign function calls ... OK
19755#> * checking R code for possible problems ... NOTE
19756#> Found the following calls to attach():
19757#> File ‘flowdex/R/zzz.R’:
19758#> attach(what = NULL, name = nsp)
19759#> See section ‘Good practice’ in ‘?attach’.
19760#> * checking Rd files ... OK
19761#> * checking Rd metadata ... OK
19762#> * checking Rd line widths ... OK
19763#> * checking Rd cross-references ... OK
19764#> * checking for missing documentation entries ... OK
19765#> * checking for code/documentation mismatches ... OK
19766#> * checking Rd \usage sections ... OK
19767#> * checking Rd contents ... OK
19768#> * checking for unstated dependencies in examples ... OK
19769#> * checking examples ... NOTE
19770#> Examples with CPU (user + system) or elapsed time > 5s
19771#> user system elapsed
19772#> plotgates 32.768 0.328 46.209
19773#> exportFdmatData 4.713 0.230 7.434
19774#> flowdexit 4.066 0.163 5.257
19775#> * checking examples with --run-donttest ... OK
19776#> * checking for unstated dependencies in ‘tests’ ... OK
19777#> * checking tests ...
19778#> Running ‘testthat.R’ [55s/82s]
19779#> OK
19780#> * checking PDF version of manual ... OK
19781#> * checking for non-standard things in the check directory ... OK
19782#> * checking for detritus in the temp directory ... OK
19783#> * DONE
19784#> Status: 3 NOTEs
19785#> See
19786#> ‘/home/docker/flowdex.Rcheck/00check.log’
19787#> for details.



## Fedora Linux

Build ID:
flowdex_0.4.2.tar.gz-49757959decc415b8f165c43e7af17c3

On the results I got emailed back it says "PREPERROR". but when I look at the 
file R-CMD-CHECK goes through as follows:

### output from fedora linux
53824#> About to run xvfb-run R CMD check --as-cran flowdex_0.4.2.tar.gz
53825#> 'getOption("repos")' replaces Bioconductor standard repositories, see
53826#> '?repositories' for details
53827#> replacement repositories:
53828#> CRAN: https://cloud.r-project.org
53829#> * using log directory ‘/home/docker/flowdex.Rcheck’
53830#> * using R Under development (unstable) (2022-02-06 r81658)
53831#> * using platform: x86_64-pc-linux-gnu (64-bit)
53832#> * using session charset: UTF-8
53833#> * using option ‘--as-cran’
53834#> * checking for file ‘flowdex/DESCRIPTION’ ... OK
53835#> * checking extension type ... Package
53836#> * this is package ‘flowdex’ version ‘0.4.2’
53837#> * package encoding: UTF-8
53838#> * checking CRAN incoming feasibility ... NOTE
53839#> Maintainer: ‘Bernhard Pollner ’
53840#> New submission
53841#> Non-FOSS package license (file LICENSE)
53842#> Possibly misspelled words in DESCRIPTION:
53843#> FCS (3:52)
53844#> fcs (8:58, 11:40)
53845#> flowdex (11:27)
53846#> Package has a VignetteBuilder field but no prebuilt vignette index.
53847#> * checking package namespace information ... OK
53848#> * checking package dependencies ... OK
53849#> * checking if this is a source package ... OK
53850#> * checking if there is a namespace ... OK
53851#> * checking for executable files ... OK
53852#> * checking for hidden files and directories ... OK
53853#> * checking for portable file names ... OK
53854#> * checking for sufficient/correct file permissions ... OK
53855#> * checking whether package ‘flowdex’ can be installed ... OK
53856#> * checking installed package size ... OK
53857#> * checking package directory ... OK
53858#> * checking for future file timestamps ... OK
53859#> * checking DESCRIPTION meta-information ... OK
53860#> * checking top-level files ... OK
53861#> * checking for left-over files ... OK
53862#> * checking index information ... OK
53863#> * checking package subdirectories ... OK
53864#> * checking R files for non-ASCII characters ... OK
53865#> * checking R files for syntax errors ... OK
53866#> * checking whether the package can be loaded ... OK
53867#> * checking whether the package can be loaded with stated dependencies ... OK
53868#> * checking whether the package can be unloaded cleanly ... OK
53869#> * checking whether the namespace can be loaded with stated dependencies ... OK
53870#> * checking whether the namespace can be unloaded cleanly ... OK
53871#> * checking loading without being on the library search path ... OK
53872#> * checking use of S3 registration ... OK
53873#> * checking dependencies in R code ... OK
53874#> * checking S3 generic/method consistency ... OK
53875#> * checking replacement functions ... OK
53876#> * checking foreign function calls ... OK
53877#> * checking R code for possible problems ... NOTE
53878#> Found the following calls to attach():
53879#> File ‘flowdex/R/zzz.R’:
53880#> attach(what = NULL, name = nsp)
53881#> See section ‘Good practice’ in ‘?attach’.
53882#> * checking Rd files ... OK
53883#> * checking Rd metadata ... OK
53884#> * checking Rd line widths ... OK
53885#> * checking Rd cross-references ... OK
53886#> * checking for missing documentation entries ... OK
53887#> * checking for code/documentation mismatches ... OK
53888#> * checking Rd \usage sections ... OK
53889#> * checking Rd contents ... OK
53890#> * checking for unstated dependencies in examples ... OK
53891#> * checking examples ... NOTE
53892#> Examples with CPU (user + system) or elapsed time > 5s
53893#> user system elapsed
53894#> plotgates 29.016 0.608 43.153
53895#> exportFdmatData 4.692 0.286 6.787
53896#> flowdexit 3.828 0.228 6.704
53897#> makeAddGatingSet 3.333 0.187 5.709
53898#> * checking examples with --run-donttest ... OK
53899#> * checking for unstated dependencies in ‘tests’ ... OK
53900#> * checking tests ...
53901#> Running ‘testthat.R’ [54s/78s]
53902#> OK
53903#> * checking PDF version of manual ... OK
53904#> * checking for non-standard things in the check directory ... OK
53905#> * checking for detritus in the temp directory ... OK
53906#> * DONE
53907#> Status: 3 NOTEs
53908#> See
53909#> ‘/home/docker/flowdex.Rcheck/00check.log’
53910#> for details.





*****


# Results from devtools::check_win_devel()

[https://win-builder.r-project.org/A3q638t6j0w7](https://win-builder.r-project.org/A3q638t6j0w7)



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

This is due, so I believe, to the limitations imposed by [package flowWorkspace](https://bioconductor.org/packages/release/bioc/html/flowWorkspace.html)

I am not sure if the license as I formulated it is necessary like that.
But when I set the license to GPL, then I get the note: 

Package has a FOSS license but eventually depends on the following
packages which restrict use:
  flowWorkspace, cytolib


I did try to find a standard license that says research only, not commercially,
but I could not find one.

So, the license file as I formulated is the best I could do in order to
encompass the requirements I believe are there due to e.g. package flowWorkspace.

If I amn wrong and the file-license as I formulated it is not necessary or
correct, I would hereby kindly ask for your help to find the appropriate
licensing for package flowdex.

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

