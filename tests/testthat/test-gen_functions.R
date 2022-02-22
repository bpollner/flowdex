library(testthat)

# for manual only
devtools::load_all("~/Documents/RPS/flowdex_R/flowdex")
# delete all in tempdir !!
# setwd("~/Documents/RPS/flowdex_R/flowdex")
rm(list=ls(all.names = TRUE))


###### Prepare #####
ptp <- path.package("flowdex")
# set up the stn object
if (dir.exists(paste0(ptp, "/inst"))) {
        ptpInst <- paste0(ptp, "/inst")
    } else {
        ptpInst <- ptp
} # end else

stn <- source(paste0(ptpInst, "/flowdex_settings.R"))$value

assign("onFdT", TRUE, pos=.GlobalEnv)



tmpdir <- tempdir()
homeDir <- "fld_Home"
pathToHome <- paste0(tmpdir, "/", homeDir)
noFo <- paste0(pathToHome, "/blabla")
if (!dir.exists(pathToHome)) {
	dir.create(pathToHome) # simulating the users experiment home directory
}


##### simple helpers #######

test_that("checkCh1", {
    a <- "a"; b <- "a"
    expect_null(checkCh1(a,b))
    a <- c("a", "b"); b <- "a"
    expect_error(checkCh1(a,b))
    a <- 1; b <- "a"
    expect_error(checkCh1(a,b))
}) # EOT

fnf <- "folderName_fcsFiles"
test_that("checkDefToSetVal", {
    expect_error(checkDefToSetVal(1, fnf, "argN", stn))
    expect_identical(checkDefToSetVal("aaa", fnf, "argN", stn), "aaa")
    expect_match(checkDefToSetVal(".", fnf, "argN", stn), "fcsFiles")
}) # EOT




###### Setting up the folder structure ########
test_that("checkPath", {
    expect_true(checkPath(pathToHome))
    expect_error(checkPath(noFo))
}) # EOT

aaa <- "aaa"
test_that("createSingleFolder", {
    expect_true(createSingleFolder(tmpdir, aaa))
    expect_false(createSingleFolder(tmpdir, aaa))
}) # EOT
unlink(paste0(tmpdir, "/aaa"), TRUE)

test_that("createFolders", {
    expect_true(createFolders(pathToHome, stn))
    expect_false(createFolders(pathToHome, stn))
}) # EOT

test_that("copyAllTemplates", {
    expect_true(copyAllTemplates(pathToHome, stn))
}) # EOT

unlink(pathToHome, TRUE)
dir.create(pathToHome)

test_that("genfs", {
    expect_null(genfs(pathToHome))
}) # EOT


###### Read in FCS Files; Repair Volume and SID ############

# first we have to copy some fcs files into the folder
ptTeHe <- paste0(ptpInst, "/testHelpers")
fcsNames <- list.files(paste0(ptTeHe, "/fcsFiles"))
from <- paste0(ptTeHe, "/fcsFiles/", fcsNames)
to <- paste0(pathToHome, "/fcsFiles/", fcsNames)
file.copy(from, to, overwrite = TRUE)

pa <- paste0(pathToHome, "/fcsFiles")
test_that("readInFlowSet", {
    expect_s4_class(readInFlowSet(folderName=pa), "flowSet")
}) # EOT

# now delete two volume data
repairVolumes(patt="b7", NA, fn=pa, includeAll = TRUE, confirm = FALSE, verbose = FALSE)
test_that("readInFlowSet", {
    expect_error(readInFlowSet(folderName=pa))
}) # EOT

# and should be good again
repairVolumes(patt=NULL, 50000, fn=pa, includeAll = FALSE, confirm = FALSE, verbose=FALSE)
test_that("readInFlowSet", {
    expect_s4_class(readInFlowSet(folderName=pa), "flowSet")
}) # EOT

test_that("repairVolumes", {
    expect_output(repairVolumes(patt=NULL, vol=30000, fn=pa, includeAll=FALSE, confirm=FALSE), "All volume values are present")
    expect_output(repairVolumes(patt=NULL, vol=30000, fn=pa, includeAll=TRUE, confirm=FALSE), "Re-writing volume data of 6 FCS files")
    expect_output(repairVolumes(patt="b7", vol=NA, fn=pa, includeAll=TRUE, confirm=FALSE), "using `NA` to replace")
    expect_output(repairVolumes(patt=NULL, vol=30000, fn=pa, includeAll=FALSE, confirm=FALSE), "Re-writing volume data of 2 FCS files")
    expect_output(repairVolumes(patt=NULL, vol=30000, fn=pa, includeAll=FALSE, confirm=FALSE), "All volume values are present")
}) # EOT
file.copy(from, to, overwrite = TRUE) # get back original fcs files





