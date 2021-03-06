% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen-functions.R
\name{cutFdmatToGate}
\alias{cutFdmatToGate}
\title{Cut 'fdmat' Object to Gate}
\usage{
cutFdmatToGate(fdmat, gate = NULL)
}
\arguments{
\item{fdmat}{An object of \code{\link{class-fdmat}} as produced by
\code{\link{makefdmat}}.}

\item{gate}{Numeric or Character length one. The designator for the gate to
keep, as defined in the gating strategy (from those gates where 'keepData' is
set to TRUE.}
}
\value{
An object of \code{\link{class-fdmat}} containing only the data for
the gate as specified in \code{gate}.
}
\description{
Cut an object of class `fdmat` down to only a single gate
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
fdmat <- flowdexit(gateStrat = "gateStrat_2")
fdmat_cut <- cutFdmatToGate(fdmat, 2)
#
setwd(old_wd)
}
\seealso{
Other Accessory functions: 
\code{\link{applyBandpass}()},
\code{\link{checkRepairFcsFiles}()},
\code{\link{exportFdmatData}()},
\code{\link{fd_load}()},
\code{\link{fd_save}()},
\code{\link{genfs}()},
\code{\link{repairSID}()},
\code{\link{repairVolumes}()}
}
\concept{Accessory functions}
