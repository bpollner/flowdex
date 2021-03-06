% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen-functions.R
\name{exportFdmatData}
\alias{exportFdmatData}
\title{Export Fluorescence Distributions}
\usage{
exportFdmatData(
  fdmat,
  expo.gate = ".",
  expo.name = ".",
  expo.type = ".",
  expo.folder = ".",
  verbose = "."
)
}
\arguments{
\item{fdmat}{An object of \code{\link{class-fdmat}} as produced by
\code{\link{makefdmat}}.}

\item{expo.gate}{Which gate to export. NULL or numeric or character length
one. Set to NULL to export data from all those gates defined in the gating
strategy where 'keepData' is set to TRUE. Provide a character length one
with a gate name or the number of that gate as defined in the gating strategy
to export data from this gate only. If left at the default '.', the value as
defined in the settings file (key 'dE_exportGate') will be used.}

\item{expo.name}{Character length one. The name of the file holding the
exported fluorescence distribution(s). If left at the default '.', the value
as defined in the settings file (key 'fiN_dataExport') will be used.}

\item{expo.type}{Character length one. The filetype of the data export.
Possible values are 'csv' and 'xlsx'.  If left at the default '.', the value
as defined in the settings file (key 'dE_exportType') will be used.}

\item{expo.folder}{Character length one. The name of the folder where
exported data should reside. If left at the default '.', the value as
defined in the settings file (key 'foN_rawData') will be used.}

\item{verbose}{Logical. If status messages should be displayed. If left at
the default '.', the value as defined in the settings file (key 'dV_verbose')
will be used.}
}
\value{
Invisible NULL; used for its side effects, i.e. to export the data
contained in 'fdmat' to file.
}
\description{
Export fluorescence distributions contained in the 'fdmat'
object to file.
}
\details{
If data are exported to xlsx, additional data like the metadata
describing the parameters that lead to the calculation of the fluorescence
distribution, the cyTags and the gating strategy are saved in an extra sheet
as well. If exporting to csv, only the fluorescence data from one single gate
can be exported.
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
fdmat <- flowdexit()
exportFdmatData(fdmat)
#
fdmat <- flowdexit(gateStrat = "gateStrat_2")
exportFdmatData(fdmat, expo.gate = 2, expo.name="data_gate_highSSC_only")
#
setwd(old_wd)
}
\seealso{
Other Accessory functions: 
\code{\link{applyBandpass}()},
\code{\link{checkRepairFcsFiles}()},
\code{\link{cutFdmatToGate}()},
\code{\link{fd_load}()},
\code{\link{fd_save}()},
\code{\link{genfs}()},
\code{\link{repairSID}()},
\code{\link{repairVolumes}()}
}
\concept{Accessory functions}
