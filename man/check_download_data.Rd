% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen-functions.R
\name{check_download_data}
\alias{check_download_data}
\title{Check and Download Example Dataset}
\usage{
check_download_data(
  where,
  data_source,
  force_download = FALSE,
  dsname = "flowdex_examples"
)
}
\arguments{
\item{where}{Character length one. The path where the example dataset should
be looked for.}

\item{data_source}{The path to the remote .zip file.}

\item{force_download}{Logical. If data should be downloaded anyway.
Defaults to FALSE.}

\item{dsname}{Character length one. The name of the folder containing the 
data. Defaults to 'flowdex_examples'.}
}
\value{
Logical. FALSE if the remote .zip file was downloaded, TRUE if the 
example dataset was present and no download was necessary.
}
\description{
Checks if the example dataset is present. If not, the example
dataset is downloaded.
}
\details{
Intended to be used within the examples; is downloading and
unzipping the folder 'dsname' in the folder specified at argument
'where'.
}
\examples{
td <- tempdir()
data_source <- "https://github.com/bpollner/data/raw/main/flowdex_examples/flowdex_examples.zip"
check_download_data(td, data_source)
}
