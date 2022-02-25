library(testthat)

# for manual line by line only
# devtools::load_all("~/Documents/RPS/flowdex_R/flowdex")
# delete all in tempdir !!
# rm(list=ls(all.names = TRUE))


###### Prepare #####

# the path to the function for this project to calculate code coverage
myCodeCovPath <- "/Users/bernhard/Documents/RPS/flowdex_R/misc/func_codecov.R"
assign(myCodeCovPath, "myCodeCovPath", pos=.GlobalEnv)

ptp <- path.package("flowdex")
# set up the stn object
if (dir.exists(paste0(ptp, "/inst"))) {
        ptpInst <- paste0(ptp, "/inst")
    } else {
        ptpInst <- ptp
} # end else

stn <- source(paste0(ptpInst, "/flowdex_settings.R"))$value

test_that("checkOnTest", {
    expect_false(checkOnTest())
}) # EOT
assign("onFdT", TRUE, pos=.GlobalEnv) ## !!!! here assigning the onFdT variable !!! ##
test_that("checkOnTest", {
    expect_true(checkOnTest())
}) # EOT

tmpdir <- tempdir()
homeDir <- "fld_Home"
pathToHome <- paste0(tmpdir, "/", homeDir)
noFo <- paste0(pathToHome, "/blabla")
if (!dir.exists(pathToHome)) {
	dir.create(pathToHome) # simulating the users experiment home directory
}
#

##### simple helpers #######
test_that("checkCh1", {
    a <- "a"; b <- "a"
    expect_null(checkCh1(a,b))
    a <- c("a", "b"); b <- "a"
    expect_error(checkCh1(a,b))
    a <- 1; b <- "a"
    expect_error(checkCh1(a,b))
}) # EOT

test_that("checkLogi", {
    a <- TRUE; b <- "a"
    expect_null(checkLogi(a,b))
    a <- c(TRUE, FALSE); b <- "a"
    expect_error(checkLogi(a,b))
    a <- 1; b <- "a"
    expect_error(checkLogi(a,b))
}) # EOT

test_that("checkNum1", {
    a <- 1; b <- "a"
    expect_null(checkNum1(a,b))
    a <- c(2, 3); b <- "a"
    expect_error(checkNum1(a,b))
    a <- "blabla"; b <- "a"
    expect_error(checkNum1(a,b))
}) # EOT

test_that("haveDefDot", {
    expect_true(haveDefDot("."))
    expect_false(haveDefDot(123))
}) # EOT


test_that("getDefValFromStn", {
    expect_match(getDefValFromStn(".", "foN_fcsFiles", stn, "."), "fcsFiles")
    expect_match(getDefValFromStn("aaa", "foN_fcsFiles", stn, "."), "aaa")
}) # EOT

fnf <- "foN_fcsFiles"
test_that("checkDefToSetVal", {
    expect_error(checkDefToSetVal(1, fnf, "argN", stn, "char"))
    expect_identical(checkDefToSetVal("aaa", fnf, "argN", stn,  "char"), "aaa")
    expect_match(checkDefToSetVal(".", fnf, "argN", stn,  "char"), "fcsFiles")
    expect_true(checkDefToSetVal(TRUE, "dV_comp", "comp", stn, "logi"))
    expect_identical(checkDefToSetVal(1, "dV_volFac", "argN", stn,  "num"), 1)
}) # EOT

test_that("devGetLocalStn", {
    expect_type(devGetLocalStn(), "list")
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
test_that("checkConsolidateFcsFiles", {
    expect_error(checkConsolidateFcsFiles(folderName=pa, igTeOff = FALSE))
    expect_output(checkConsolidateFcsFiles(folderName=pa, igTeOff = TRUE), "have been re-written to disc")
    expect_null(checkConsolidateFcsFiles(folderName=pa, igTeOff = FALSE))
}) # EOT

file.copy(from, to, overwrite = TRUE)
test_that("readInFlowSet", {
    expect_error(readInFlowSet(folderName=pa, igTeOff = FALSE))
    expect_s4_class(readInFlowSet(folderName=pa, igTeOff = TRUE), "flowSet")
}) # EOT

# now delete two volume data
repairVolumes(patt="th1", NA, fn=pa, includeAll = TRUE, confirm = FALSE, verbose = FALSE)
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
    expect_output(repairVolumes(patt="th1", vol=NA, fn=pa, includeAll=TRUE, confirm=FALSE), "using `NA` to replace")
    expect_error(repairVolumes(patt=NULL, vol=NULL, fn=pa, includeAll=TRUE, confirm=FALSE))
    expect_output(repairVolumes(patt=NULL, vol=30000, fn=pa, includeAll=FALSE, confirm=FALSE), "Re-writing volume data of 2 FCS files")
    expect_output(repairVolumes(patt=NULL, vol=30000, fn=pa, includeAll=FALSE, confirm=FALSE), "All volume values are present")
}) # EOT
file.copy(from, to, overwrite = TRUE) # get back original fcs files

# repairVolumes(patt=NULL, 50000, fn=pa, includeAll = TRUE, confirm = FALSE, verbose=FALSE)
siName <- list.files(pa)[2]
test_that("repairSID", {
    expect_error(repairSID(fs=NULL, name=NULL, newSID=NULL, patt=NULL, fn=pa, confirm=FALSE, ignore.text.offset = FALSE))
    expect_s4_class(repairSID(fs=NULL, name=NULL, newSID=NULL, patt=NULL, fn=pa, confirm=FALSE, ignore.text.offset = TRUE), "flowSet")
    fs <- repairSID(fn=pa)
    expect_error(repairSID(fs=fs, name="aaa", newSID=NULL, patt=NULL, fn=pa, confirm=FALSE), "Please provide a value")
    expect_error(repairSID(fs=fs, name="aaa", newSID="newSID_bbb", patt=NULL, fn=pa, confirm=FALSE), "seems not to be present")
    expect_output(repairSID(fs=fs, name=siName, newSID="newSID_HaHaHa", patt=NULL, fn=pa, confirm=FALSE), "has been rewritten")
#    fs <- repairSID(fn=pa)
#    print(fs@phenoData@data)
}) # EOT
file.copy(from, to, overwrite = TRUE) # get back original fcs files
# fs <- readInFlowSet(pa)

##### Make Gating Set etc. #########
test_that("makeGatingSet", {
    expect_s4_class(makeGatingSet(fn=pa, ignore.text.offset = TRUE), "GatingSet")
    expect_s4_class(makeGatingSet(fn=pa, comp=TRUE, ignore.text.offset = FALSE), "GatingSet")
}) # EOT

gs <- makeGatingSet(fn=pa, verbose = FALSE)

# now add the gates









































