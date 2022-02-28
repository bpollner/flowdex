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
assign("onFdT_bbp", TRUE, pos=.GlobalEnv) ## !!!! here assigning the onFdT_bbp variable !!! ##
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
test_that("checkCharX", {
    a <- "a"; b <- "a"
    expect_null(checkCharX(a,b))
    a <- c("a", "b"); b <- "a"
    expect_error(checkCharX(a,b))
    a <- 1; b <- "a"
    expect_error(checkCharX(a,b))
}) # EOT

test_that("checkCharX_null", {
    a <- "a"; b <- "a"
    expect_null(checkCharX_null(a,b))
    a <- c("a", "b"); b <- "a"
    expect_error(checkCharX_null(a,b))
    a <- 1; b <- "a"
    expect_error(checkCharX_null(a,b))
    a <- NULL; b <- "a"
    expect_null(checkCharX_null(a,b))
    a <- 1; b <- "a"
    expect_error(checkCharX_null(a,b))
    a <- c("a", "b"); b <- "a"
    expect_null(checkCharX_null(a,b, len=2))
}) # EOT

test_that("checkLogi", {
    a <- TRUE; b <- "a"
    expect_null(checkLogi(a,b))
    a <- c(TRUE, FALSE); b <- "a"
    expect_error(checkLogi(a,b))
    a <- 1; b <- "a"
    expect_error(checkLogi(a,b))
}) # EOT

test_that("checkNumX", {
    a <- 1; b <- "a"
    expect_null(checkNumX(a,b))
    a <- c(2, 3); b <- "a"
    expect_error(checkNumX(a,b))
    a <- "blabla"; b <- "a"
    expect_error(checkNumX(a,b))
}) # EOT

mat <- matrix(rnorm(20), ncol=2)
colnames(mat) <- c("a", "b")
mat[1,2] <- Inf
matC <- mat[-1,]
test_that("cleanUpInfinites", {
    expect_identical(cleanUpInfinites(mat), matC)
}) # EOT

al <- list(a=1, b=2)
colnames(matC) <- c("b", "c")
test_that("checkShowGateChannels", {
    expect_true(checkShowGateChannels(al, mat))
    expect_error(checkShowGateChannels(al, matC), "does not contain the required channels")
}) # EOT

test_that("haveDefDot", {
    expect_true(haveDefDot("."))
    expect_false(haveDefDot(123))
    expect_false(haveDefDot(NULL))
}) # EOT

test_that("getDefValFromStn", {
    expect_match(getDefValFromStn(".", "foN_fcsFiles", stn, defValue="."), "fcsFiles")
    expect_match(getDefValFromStn("aaa", "foN_fcsFiles", stn, defValue="."), "aaa")
    expect_error(getDefValFromStn(".", "aaa", stn, "blabla", defValue=NULL), "no default value defined")
    expect_identical(getDefValFromStn(NULL, "aaa", stn, "blabla", defValue=NULL), NULL)
    expect_identical(getDefValFromStn(1, "aaa", stn, "blabla", defValue=NULL), 1)
    #
    expect_identical(getDefValFromStn(NULL, "aaa", stn, "blabla", defValue="."), NULL)
}) # EOT

fnf <- "foN_fcsFiles"
test_that("checkDefToSetVal", {
    expect_error(checkDefToSetVal(1, fnf, "argN", stn, "char"))
    expect_identical(checkDefToSetVal("aaa", fnf, "argN", stn,  "char"), "aaa")
    expect_match(checkDefToSetVal(".", fnf, "argN", stn,  "char"), "fcsFiles")
    expect_true(checkDefToSetVal(TRUE, "dV_comp", "comp", stn, "logi"))
    expect_identical(checkDefToSetVal(1, "dV_volFac", "argN", stn,  "num"), 1)
    expect_identical(checkDefToSetVal(NULL, "..x..", "argN", stn,  "charNull", defValue=NULL), NULL)
    expect_identical(checkDefToSetVal("blabla", "..x..", "argN", stn,  "charNull", defValue=NULL), "blabla")
    expect_error(checkDefToSetVal(1, "..x..", "argN", stn,  "charNull", defValue=NULL), NULL)
    expect_error(checkDefToSetVal(".", "..x..", "argN", stn,  "charNull", defValue=NULL), "no default value defined")
    #
    expect_identical(checkDefToSetVal(NULL, "dV_channelBoundaries", "bnd", stn, checkFor="numNull", len=4), NULL)
    expect_length(checkDefToSetVal(".", "dV_channelBoundaries", "bnd", stn, checkFor="numNull", len=4), 4)
    expect_identical(checkDefToSetVal(c(1,2,3,4), "dV_channelBoundaries", "bnd", stn, checkFor="numNull", len=4), c(1,2,3,4))
    expect_error(checkDefToSetVal(c(1,2,3,4,5), "dV_channelBoundaries", "bnd", stn, checkFor="numNull", len=4), "provide a numeric length 4")
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
repairVolumes(patt="th1", vol=NA, fn=pa, includeAll = TRUE, confirm = FALSE, verbose = FALSE)
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








##### Gating Set & add & draw gates etc. #########
test_that("makeGatingSet", {
    expect_s4_class(makeGatingSet(fn=pa, ignore.text.offset = TRUE), "GatingSet")
    expect_s4_class(makeGatingSet(fn=pa, comp=TRUE, ignore.text.offset = FALSE), "GatingSet")
}) # EOT

gs <- makeGatingSet(fn=pa, verbose = FALSE)

## now add the gates ####
# first copy material from the testHelpers/gates folder
ptGaFo <- paste0(ptpInst, "/testHelpers/gates")
aaa <- list.files(ptGaFo)
gaFrom <- paste0(ptGaFo, "/", aaa)
gaTo <- paste(pathToHome, stn$foN_gating, aaa, sep="/")
file.copy(gaFrom, gaTo, overwrite = TRUE)
#
stn <- source(paste0(ptpInst, "/flowdex_settings.R"))$value # conveniently here again

foN_gating <- paste0(pathToHome, "/gating")
fiN_gateStrat <- stn$fiN_gateStrat
test_that("checkFileExistence", {
    expect_error(checkFileExistence(foN_gating, "blabla", typE=".csv"))
    expect_true(checkFileExistence(foN_gating, fiN_gateStrat, typE=".csv"))
}) # EOT

test_that("loadGaXFile", {
    expect_s3_class(loadGaXFile(foN_gating, fiN_gateStrat, "csv"), "data.frame")
    expect_s3_class(loadGaXFile(foN_gating, fiN_gateStrat, "xlsx"), "data.frame")
    expect_type(loadGaXFile(foN_gating, "Select", "pgg"), "list")
}) # EOT

test_that("importCheckGatingStrategy", {
    expect_error(importCheckGatingStrategy(fiN_gateStrat, stn, gsType="bla", foN_gating))
    expect_s4_class(importCheckGatingStrategy(fiN_gateStrat, stn, gsType=".", foN_gating), "gatingStrategy_fd")
    expect_s4_class(importCheckGatingStrategy(fiN_gateStrat, stn, gsType="csv", foN_gating), "gatingStrategy_fd")
    expect_s4_class(importCheckGatingStrategy(fiN_gateStrat, stn, gsType="xlsx", foN_gating), "gatingStrategy_fd")
    expect_error(importCheckGatingStrategy("gateStrat_wrongColnames", stn, gsType="csv", foN_gating), "does not contain the required column names")
}) # EOT

gsdf <- importCheckGatingStrategy(fiN_gateStrat, stn, gsType="csv", foN_gating)

test_that("checkPggExistence", {
    expect_true(checkPggExistence(gsdf, foN_gating, fiN_gateStrat))
    file.remove(paste0(pathToHome, "/gating/Select"))
    expect_error(checkPggExistence(gsdf, foN_gating, fiN_gateStrat), "'Select' seems not to exist")
    file.remove(paste0(pathToHome, "/gating/BactStain_pn3"))
    expect_error(checkPggExistence(gsdf, foN_gating, fiN_gateStrat), "'Select', 'BactStain_pn3' seem not to exist")
}) # EOT
file.copy(gaFrom, gaTo, overwrite = TRUE) # and restore again

test_that("addGates", {
    expect_s4_class(addGates(gs=gs, foN.gateStrat = foN_gating), "GatingSet_fd")
    expect_message(addGates(gs=gs, foN.gateStrat = foN_gating), "already contains")
}) # EOT
# by now, we have a gating-set 'gs' with two added gates ??, check via plotting plot(gs)

# and all together
test_that("makeAddGatingSet", {
    expect_s4_class(makeAddGatingSet(fn=pa, foN.gateStrat=foN_gating, verbose=FALSE), "GatingSet_fd")
}) # EOT

gsA <- makeAddGatingSet(fn=pa, foN.gateStrat=foN_gating, verbose=FALSE)

test_that("checkObjClass", {
    expect_true(checkObjClass(gs, "GatingSet", "argA"))
    expect_error(checkObjClass(gs, "GatingSet_fd", "argA"), "Please provide")
}) # EOT

## now draw gates
pathToPgg <- paste0(foN_gating, "/polyGate")
assign("pathToPgg", pathToPgg, pos=.GlobalEnv)
test_that("getLocMat_TS", {
    expect_type(getLocMat_TS(locN=512), "list")
}) # EOT

test_that("drawGate", {
    expect_null(drawGate(gs, 1, bnd=".", foN.gateStrat = foN_gating, showGate = "polyGate", useLoc = FALSE))
    expect_type(drawGate(gs, 1, pggId="pggTest", bnd=".", foN.gateStrat = foN_gating, showGate = "polyGate", useLoc = TRUE), "list")
}) # EOT






















