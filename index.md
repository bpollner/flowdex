<!-- badges: start -->

[![R-CMD-check](https://github.com/bpollner/flowdex/workflows/R-CMD-check/badge.svg)](https://github.com/bpollner/flowdex/actions) [![codecov](https://codecov.io/gh/bpollner/flowdex/branch/main/graph/badge.svg?token=aZFS02SMwz)](https://app.codecov.io/gh/bpollner/flowdex) [![CRAN status](https://www.r-pkg.org/badges/version/flowdex)](https://CRAN.R-project.org/package=flowdex) [![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/flowdex)](https://cran.r-project.org/package=flowdex)

<!-- badges: end -->

# flowdex

Extract Fluorescence Distribution Data from FCS Files and Recalculate to Events per Volume

## Description

Package flowdex can:

-   **Extract fluorescence distribution** data from fcs files and **recalculate** them **to events per volume unit**,

-   Apply any custom-built gating strategy to the fcs files,

-   Generate variables describing the dataset by using a structured character string in the sample-Id field of the sample at the time of data acquisition, and

-   **Visualise fluorescence distributions** and **export them to file**, along with data denoting the overall events per volume unit in each sample in each gate.

To (meaningfully) use flowdex, the fcs files have to contain volumetric measurement data denoting the acquired sample volume.

## Installation

Install release version from CRAN via

    install.packages("flowdex") 

Or download from github:

    library(devtools)
    install_github(repo="bpollner/flowdex", ref="main")
