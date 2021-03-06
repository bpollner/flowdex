% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen-functions.R
\name{plotgates}
\alias{plotgates}
\title{Plot Gates on All flowFrames in a Gating Set}
\usage{
plotgates(
  gs,
  ti = "",
  spl = NULL,
  fns = NULL,
  plotAll = FALSE,
  toPdf = TRUE,
  x = NULL,
  y = NULL,
  name.dict = ".",
  foN.dict = ".",
  type.dict = ".",
  foN.plots = "."
)
}
\arguments{
\item{gs}{A gating set.}

\item{ti}{Character length one, a possible character added to the title of
the gate-plot.}

\item{spl}{Character length one. The name of the column in the cyTags that
should be used to split by before plotting. If left at the default
\code{NULL}, no splitting is performed. Possible values for 'spl' are the
column names of the cyTags saved in the object of class 'fdmat' as produced
by \code{\link{makefdmat}}.}

\item{fns}{Character length one. The filename suffix of the possible pdf.}

\item{plotAll}{Logical. If left at the default \code{FALSE}, only the gates
where the parameter \code{keepData} in the gating strategy is set to
\code{TRUE} are plotted. If set to \code{TRUE}, all gates within the gating
strategy file will be plotted.}

\item{toPdf}{Logical. If the plots should be saved in a pdf. Defaults to
TRUE}

\item{x}{Character length one. The name of channel where data was acquired to
be displayed on the x-axis. Only applies if a gating set without applied gate
is provided to the argument \code{gs}.}

\item{y}{Character length one. The name of channel where data was acquired to
be displayed on the y-axis. Only applies if a gating set without applied gate
is provided to the argument \code{gs}.}

\item{name.dict}{Character length one. The name of the dictionary. If left
at the default '.', the value as defined in the settings file (key
'dD_dict_name') will be used.}

\item{foN.dict}{Character length one. The name of the folder where the
dictionary resides. If left at the default '.', the value as defined in the
settings file (key 'foN_dictionary') will be used.}

\item{type.dict}{Character length one. The filetype of the dictionary. Can
be one of 'csv' or 'xlsx'. If left at the default '.', the value as defined
in the settings file (key 'dD_dict_type') will be used.}

\item{foN.plots}{Character length one. The name of the folder where possible
PDFs should be saved in. If left at the default '.', the value as defined in
the settings file (key 'foN_plots') will be used.}
}
\value{
(Invisible) NULL. Is used for its side effect, i.e. to plot gated
data resp. to visualize the gating strategy.
}
\description{
Plot all available gates on all flowFrames in a gating set and
add layers for the number of events. (In raw format, i.e. *not*
re-calculated to volume!)
}
\details{
Plotting is performed by the function \code{\link[ggcyto]{ggcyto}}.
If a gating set without applied gates is provided to the first argument,
parameters \code{plotAll}, \code{spl} and \code{toPdf} do not apply.
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
gs <- makeAddGatingSet(gateStrat = "gateStrat_2")
plotgates(gs, toPdf = FALSE)
plotgates(gs, spl = "C_treatment", toPdf = FALSE)
plotgates(gs, spl = "C_treatment", plotAll = TRUE, fns = "_allGates")
#
setwd(old_wd)
}
\seealso{
Other Plotting functions: 
\code{\link{drawGate}()},
\code{\link{plotFlscDist}()}
}
\concept{Plotting functions}
