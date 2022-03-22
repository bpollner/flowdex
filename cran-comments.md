# Results from rhub::check_for_cran()

## Windows Server 2022

[https://builder.r-hub.io/status/flowdex_0.4.2.tar.gz-94433876e57049f2bff4ed2815cb234a](https://builder.r-hub.io/status/flowdex_0.4.2.tar.gz-94433876e57049f2bff4ed2815cb234a)

Here, R-CMD-CHECK comes back with errors. As I understand it, the runner 
was not able to download the content required to run the tests, hence the 
errors.

But please not that devtools::check_win_devel() goes through without a problem, 
see below.

Also the run on Windows 2019 as provided by Github is perfectly ok:
[https://github.com/bpollner/flowdex/actions/runs/2021219945](https://github.com/bpollner/flowdex/actions/runs/2021219945)




## Ubuntu Linux

Build ID:
flowdex_0.4.2.tar.gz-803b1af37ca94abe95cdb73e128f3c60

[https://builder.r-hub.io/status/flowdex_0.4.2.tar.gz-803b1af37ca94abe95cdb73e128f3c60](https://builder.r-hub.io/status/flowdex_0.4.2.tar.gz-803b1af37ca94abe95cdb73e128f3c60)

On the results I got emailed back it says "PREPERROR". but when I look at the 
file R-CMD-CHECK goes through as follows, and the final line is:

 Finished: SUCCESS

### output from ubuntu linux


19704#> About to run xvfb-run R CMD check --as-cran flowdex_0.4.2.tar.gz
19705#> 'getOption("repos")' replaces Bioconductor standard repositories, see
19706#> '?repositories' for details
19707#> replacement repositories:
19708#> CRAN: https://cloud.r-project.org
19709#> * using log directory ‘/home/docker/flowdex.Rcheck’
19710#> * using R version 4.1.2 (2021-11-01)
19711#> * using platform: x86_64-pc-linux-gnu (64-bit)
19712#> * using session charset: UTF-8
19713#> * using option ‘--as-cran’
19714#> * checking for file ‘flowdex/DESCRIPTION’ ... OK
19715#> * checking extension type ... Package
19716#> * this is package ‘flowdex’ version ‘0.4.2’
19717#> * package encoding: UTF-8
19718#> * checking CRAN incoming feasibility ... NOTE
19719#> Maintainer: ‘Bernhard Pollner ’
19720#> New submission
19721#> Non-FOSS package license (file LICENSE)
19722#> Possibly mis-spelled words in DESCRIPTION:
19723#> fcs (8:58, 11:40)
19724#> FCS (3:52)
19725#> flowdex (11:27)
19726#> Package has a VignetteBuilder field but no prebuilt vignette index.
19727#> * checking package namespace information ... OK
19728#> * checking package dependencies ... OK
19729#> * checking if this is a source package ... OK
19730#> * checking if there is a namespace ... OK
19731#> * checking for executable files ... OK
19732#> * checking for hidden files and directories ... OK
19733#> * checking for portable file names ... OK
19734#> * checking for sufficient/correct file permissions ... OK
19735#> * checking whether package ‘flowdex’ can be installed ... OK
19736#> * checking installed package size ... OK
19737#> * checking package directory ... OK
19738#> * checking for future file timestamps ... OK
19739#> * checking DESCRIPTION meta-information ... OK
19740#> * checking top-level files ... OK
19741#> * checking for left-over files ... OK
19742#> * checking index information ... OK
19743#> * checking package subdirectories ... OK
19744#> * checking R files for non-ASCII characters ... OK
19745#> * checking R files for syntax errors ... OK
19746#> * checking whether the package can be loaded ... OK
19747#> * checking whether the package can be loaded with stated dependencies ... OK
19748#> * checking whether the package can be unloaded cleanly ... OK
19749#> * checking whether the namespace can be loaded with stated dependencies ... OK
19750#> * checking whether the namespace can be unloaded cleanly ... OK
19751#> * checking loading without being on the library search path ... OK
19752#> * checking use of S3 registration ... OK
19753#> * checking dependencies in R code ... OK
19754#> * checking S3 generic/method consistency ... OK
19755#> * checking replacement functions ... OK
19756#> * checking foreign function calls ... OK
19757#> * checking R code for possible problems ... NOTE
19758#> Found the following calls to attach():
19759#> File ‘flowdex/R/zzz.R’:
19760#> attach(what = NULL, name = nsp)
19761#> See section ‘Good practice’ in ‘?attach’.
19762#> * checking Rd files ... OK
19763#> * checking Rd metadata ... OK
19764#> * checking Rd line widths ... OK
19765#> * checking Rd cross-references ... OK
19766#> * checking for missing documentation entries ... OK
19767#> * checking for code/documentation mismatches ... OK
19768#> * checking Rd \usage sections ... OK
19769#> * checking Rd contents ... OK
19770#> * checking for unstated dependencies in examples ... OK
19771#> * checking examples ... NOTE
19772#> Examples with CPU (user + system) or elapsed time > 5s
19773#> user system elapsed
19774#> plotgates 30.356 0.287 24.541
19775#> * checking examples with --run-donttest ... OK
19776#> * checking for unstated dependencies in ‘tests’ ... OK
19777#> * checking tests ...
19778#> Running ‘testthat.R’ [43s/41s]
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
flowdex_0.4.2.tar.gz-ae1175f5f1d54f9bbd2672f6cb2cb880

[https://builder.r-hub.io/status/flowdex_0.4.2.tar.gz-ae1175f5f1d54f9bbd2672f6cb2cb880](https://builder.r-hub.io/status/flowdex_0.4.2.tar.gz-ae1175f5f1d54f9bbd2672f6cb2cb880)


On the results I got emailed back it says "PREPERROR". but when I look at the 
file R-CMD-CHECK goes through as follows, and the final line is:

 Finished: SUCCESS

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
53894#> plotgates 27.355 0.654 24.531
53895#> * checking examples with --run-donttest ... OK
53896#> * checking for unstated dependencies in ‘tests’ ... OK
53897#> * checking tests ...
53898#> Running ‘testthat.R’ [46s/43s]
53899#> OK
53900#> * checking PDF version of manual ... OK
53901#> * checking for non-standard things in the check directory ... OK
53902#> * checking for detritus in the temp directory ... OK
53903#> * DONE
53904#> Status: 3 NOTEs
53905#> See
53906#> ‘/home/docker/flowdex.Rcheck/00check.log’
53907#> for details.

*****


# Results from devtools::check_win_devel()

[https://win-builder.r-project.org/K0tBS3UFz28z](https://win-builder.r-project.org/K0tBS3UFz28z)

* using log directory 'd:/RCompile/CRANguest/R-devel/flowdex.Rcheck'
* using R Under development (unstable) (2022-03-21 r81954 ucrt)
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
* checking R code for possible problems ... [34s] NOTE
Found the following calls to attach():
File 'flowdex/R/zzz.R':
  attach(what = NULL, name = nsp)
See section 'Good practice' in '?attach'.
* checking Rd files ... [1s] OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking examples ... [195s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
                 user system elapsed
plotgates       42.97   1.84   54.08
addGates         7.41  11.96   21.10
repairVolumes    5.06  11.73   17.19
exportFdmatData  8.08   1.01   17.02
flowdexit        6.89   0.95   10.77
* checking for unstated dependencies in 'tests' ... OK
* checking tests ... [160s] OK
  Running 'testthat.R' [160s]
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

