% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen-functions.R
\name{addGates}
\alias{addGates}
\title{Add Polygon Gates}
\usage{
addGates(
  gs,
  gateStrat = ".",
  foN.gateStrat = ".",
  type.gateStrat = ".",
  verbose = "."
)
}
\arguments{
\item{gs}{A gating set as produced by \code{\link{makeGatingSet}}.}

\item{gateStrat}{Character length one. The name of the file defining the
gating strategy. If left at the default '.', the name as defined in the
settings file (key: 'fiN_gateStrat') will be used.}

\item{foN.gateStrat}{Character length one. The name of the folder where the
file defining the gating strategy and the gate definitions reside. If left
at the default '.', the name as defined in the settings file
(key: 'foN_gating') will be used.}

\item{type.gateStrat}{Character length one, can be either 'csv' or 'xlsx'.
The type of file defining the gating strategy. Currently, csv and xlsx
files are supported. If left at the default '.', the filetype as defined in
the settings (key: 'dV_gateStratInputType') file will be used.}

\item{verbose}{Logical. If status messages should be displayed. If left at
the default '.', the value as defined in the settings file (key 'dV_verbose')
will be used.}
}
\value{
An object of \code{\link{class-GatingSet_fd}}.
}
\description{
Load the predefined gating strategy (as .csv or .xlsx file) and
apply the gates as defined in the file.
}
\details{
The gating strategy file can hold one or more gates. One row in the
files represents one gate. In order to see a schematic representation of
'parent' and 'child' gates, simply use 'plot'.
}
\examples{
td <- tempdir()
data_source <- "https://github.com/bpollner/data/raw/main/flowdex_examples/flowdex_examples.zip"
check_download_data(td, data_source)
exp_home <- paste0(td, "/flowdex_examples")
old_wd <- getwd()
setwd(exp_home)
#
assign("get_settings_from_flowdex_package_root", TRUE, pos=.GlobalEnv)
# only required to make the examples run automatically
# you should not call 'assign' if you run the examples manually
# the effect of setting 'get_settings_from_flowdex_package_root' to TRUE
# is that the file 'flowdex_settings.R' in 'root' of the installed package
# 'flowdex' will be sourced instead of the one in the user-defined location.
#
gs <- makeGatingSet()
gs
gs <- addGates(gs)
gs
#
setwd(old_wd)
}
\seealso{
Other Extraction functions: 
\code{\link{makeAddGatingSet}()},
\code{\link{makeGatingSet}()},
\code{\link{makefdmat}()}
}
\concept{Extraction functions}
