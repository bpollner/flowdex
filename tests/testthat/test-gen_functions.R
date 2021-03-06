library(testthat)

# for manual line by line only
#devtools::load_all("~/Documents/RPS/flowdex_R/flowdex")
# delete all im tempdir !!
# rm(list=ls(all.names = TRUE))




###### Prepare #####

rm(list=ls(all.names = TRUE))

# the path to the function for this project to calculate code coverage
myCodeCovPath <- "/Users/bernhard/Documents/RPS/flowdex_R/misc/func_codecov.R"
assign("myCodeCovPath", myCodeCovPath, pos=.GlobalEnv)

ptp <- path.package("flowdex")
# set up the stn object
if (dir.exists(paste0(ptp, "/inst"))) {
        ptpInst <- paste0(ptp, "/inst")
    } else {
        ptpInst <- ptp
} # end else
#
ptSeOrig <- paste0(ptpInst, "/testHelpers/settingsFile/flowdex_settings_original.R")
ptSeTarget <- paste0(ptpInst, "/flowdex_settings.R")
file.copy(ptSeOrig, ptSeTarget, overwrite=TRUE)
#
stn <- source(paste0(ptpInst, "/flowdex_settings.R"))$value

test_that("checkOnTest", {
    expect_false(checkOnTest())
}) # EOT
#
assign("get_settings_from_flowdex_package_root", TRUE, pos=.GlobalEnv) ## !!!!
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
data_source <- "https://github.com/bpollner/data/raw/main/flowdex_examples/flowdex_examples.zip"
test_that("check_download_data", {
    expect_false(check_download_data(pathToHome, data_source))
    expect_true(check_download_data(pathToHome, data_source))
}) # EOT


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

test_that("checkCharX_null_Num", {
    a <- "bla"; b <- "argA"
    expect_null(checkCharX_null_Num(a, b))
    a <- 1 ; b <- "argA"
    expect_null(checkCharX_null_Num(a, b))
    a <- NULL; b <- "argA"
    expect_null(checkCharX_null_Num(a, b))
    a <- c(1,2,3); b <- "argA"
    expect_error(checkCharX_null_Num(a, b), "Please provide a numeric or character length 1")
    a <- c("a", "b", "c"); b <- "argA"
    expect_error(checkCharX_null_Num(a, b), "Please provide a numeric or character length 1")
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
    #
    expect_identical(checkDefToSetVal("aaa", "dE_exportGate", "argN", stn,  checkFor="charNullNum"), "aaa")
    expect_identical(checkDefToSetVal(1, "dE_exportGate", "argN", stn,  checkFor="charNullNum"), 1)
    expect_identical(checkDefToSetVal(NULL, "dE_exportGate", "argN", stn,  checkFor="charNullNum"), NULL)
    expect_identical(checkDefToSetVal(".", "dE_exportGate", "argN", stn,  checkFor="charNullNum"), NULL)
    expect_error(checkDefToSetVal(c(1,2,3), "dE_exportGate", "argN", stn,  checkFor="charNullNum"))
}) # EOT

test_that("devGetLocalStn", {
    expect_type(devGetLocalStn(), "list")
}) # EOT

test_that("ignoreEdge", {
    expect_equal(length(ignoreEdge(1:100, perc=0)), 100)
    expect_equal(length(ignoreEdge(1:100, perc=5)), 90)
    expect_equal(length(ignoreEdge(1:100, perc=5, minLe=200)), 100)
}) # EOT






###### Setting up the folder structure ########
test_that("check_path", {
    expect_true(check_path(pathToHome))
    expect_error(check_path(noFo))
}) # EOT

aaa <- "aaa"
test_that("create_single_folder", {
    expect_true(create_single_folder(tmpdir, aaa))
    expect_false(create_single_folder(tmpdir, aaa))
}) # EOT
unlink(paste0(tmpdir, "/aaa"), TRUE)

test_that("create_folders", {
    expect_true(create_folders(pathToHome, stn))
    expect_false(create_folders(pathToHome, stn))
}) # EOT

test_that("copy_all_templates", {
    expect_true(copy_all_templates(pathToHome, stn))
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
restore <- function(){file.copy(from, to, overwrite = TRUE)}
restore()

pa <- paste0(pathToHome, "/fcsFiles")

test_that("checkRepairFcsFiles", {
    restore()
    expect_error(checkRepairFcsFiles(fn=pa, fcsRepair=FALSE, confirm=TRUE, showMultiples=TRUE, keepLast=TRUE), "Consider setting 'fcsRepair' to TRUE")
    expect_error(checkRepairFcsFiles(fn=pa, fcsRepair=FALSE, confirm=TRUE, keepLast=TRUE), "Consider setting 'fcsRepair' to TRUE")
    expect_output(checkRepairFcsFiles(fn=pa, fcsRepair=TRUE, confirm=FALSE, keepLast=TRUE), "All except the last")
    restore()
    expect_output(checkRepairFcsFiles(fn=pa, fcsRepair=TRUE, confirm=FALSE, keepLast=FALSE), "All except the first")
    restore()
    expect_error(checkRepairFcsFiles(fn=pa, fcsRepair=TRUE, confirm=FALSE, keepLast=4), "provide a number ranging from 1 to 3 ")
    restore()
    expect_output(checkRepairFcsFiles(fn=pa, fcsRepair=TRUE, confirm=FALSE, keepLast=2), "All except #2")
    restore()
    expect_output(checkRepairFcsFiles(fn=pa, fcsRepair=TRUE, confirm=FALSE, keepLast=1), "All except the first")
    restore()
    expect_output(checkRepairFcsFiles(fn=pa, fcsRepair=TRUE, confirm=FALSE, keepLast=3), "All except the last")
    expect_output(checkRepairFcsFiles(fn=pa), "seem to be ok")
    expect_null(checkRepairFcsFiles(fn=pa, verbose = FALSE))
    restore()
    expect_true(checkRepairFcsFiles(fn=pa, fcsRepair=TRUE, confirm=FALSE))
    expect_null(checkRepairFcsFiles(fn=pa, verbose = FALSE))
}) # EOT
# checkRepairFcsFiles(fn=pa, fcsRepair=TRUE, confirm=TRUE, showMultiples=FALSE, keepLast=TRUE)

restore()
test_that("readInFlowSet", {
    expect_error(readInFlowSet(folderName=pa, fcsRepair = FALSE), "Consider setting 'fcsRepair' to TRUE")
    expect_s4_class(readInFlowSet(folderName=pa, fcsRepair = TRUE), "flowSet")
}) # EOT
# readInFlowSet(folderName=pa, fcsRepair=FALSE)

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
    expect_error(repairSID(fs=NULL, name=NULL, newSID=NULL, patt=NULL, fn=pa, confirm=FALSE, fcsRepair = FALSE))
    expect_s4_class(repairSID(fs=NULL, name=NULL, newSID=NULL, patt=NULL, fn=pa, confirm=FALSE, fcsRepair = TRUE), "flowSet")
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
    expect_s4_class(makeGatingSet(fn=pa, fcsRepair = TRUE), "GatingSet")
    expect_s4_class(makeGatingSet(fn=pa, comp=TRUE, fcsRepair = FALSE), "GatingSet")
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
    expect_error(importCheckGatingStrategy("gateStrat_all_FALSE", stn, gsType="csv", foN_gating), "need to keep data from at least one gate")
}) # EOT
# importCheckGatingStrategy("gateStrat_all_FALSE", stn, gsType="csv", foN_gating)


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

test_that("assignGatingSetToEnv", {
  #  gsenv$gatingSet <- NULL
    expect_null(assignGatingSetToEnv(gsA))
    expect_s4_class(gsenv$gatingSet, "GatingSet_fd")
}) # EOT

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






#### make fdmat and plot gates ####
#
# first copy material from the dictionary folder in testHelpers
stn <- source(paste0(ptpInst, "/flowdex_settings.R"))$value # conveniently here again
ptDic <- paste0(ptpInst, "/testHelpers/dictionary")
aaa <- list.files(ptDic)
dictFrom <- paste0(ptDic, "/", aaa)
dictTo <- paste(pathToHome, stn$foN_dictionary, aaa, sep="/")
file.copy(dictFrom, dictTo, overwrite = TRUE)
#
foN_dict <- paste0(pathToHome, "/", stn$foN_dictionary)
nameDict <- stn$dD_dict_name
typeDict <- stn$dD_dict_type
dictTypeE <- paste0(".", typeDict)
dicGood <- loadGaXFile(foN_dict, "dictionary", typeDict)
dicMiss <- loadGaXFile(foN_dict, "dictionary_miss", typeDict)
dicDouble <- loadGaXFile(foN_dict, "dictionary_double", typeDict)
#
test_that("makeCyTags_inner", {
    expect_s3_class(makeCyTags_inner(gsA, dicGood, stn), "data.frame")
    expect_error(makeCyTags_inner(gsA, dicMiss, stn), "is not present")
    expect_error(makeCyTags_inner(gsA, dicDouble, stn), "more than one translation")
    cns <- colnames(flowWorkspace::pData(gsA))
    ind <- which(cns == "sampleId")
    colnames(flowWorkspace::pData(gsA))[ind] <- "blabla"
    expect_error(makeCyTags_inner(gsA, dicGood, stn), "the required column 'sampleId' is not available")
}) # EOT

gsA <- makeAddGatingSet(fn=pa, foN.gateStrat=foN_gating, verbose=FALSE) # and restore
gsDouble <- makeAddGatingSet(fn=pa, foN.gateStrat=foN_gating, gateStrat="gateStrat_keep", verbose=FALSE)

test_that("makeCyTags", {
    expect_s4_class(makeCyTags(gsA, dicGood, stn), "cyTags")
    expect_equal(nrow(makeCyTags(gsA, dicGood, stn)), 6)
    expect_equal(nrow(makeCyTags(gsDouble, dicGood, stn)), 6)
}) # EOT

test_that("assignHereStnValues", {
    expect_true(assignHereStnValues(stn))
}) # EOT

test_that("getEventsPerVolume_single", {
    expect_s4_class(getEventsPerVolume_single(gsA, gateName="DNA+", chName="FITC.A", volUnit="ml", apc=TRUE, coV=125), "eventsPV")
    expect_equal(ncol(getEventsPerVolume_single(gsA, gateName="DNA+", chName="FITC.A", volUnit="ml", apc=TRUE, coV=125)), 4)
    expect_equal(ncol(getEventsPerVolume_single(gsA, gateName="DNA+", chName="FITC.A", volUnit="ml", apc=FALSE, coV=125)), 3)
    expect_error(getEventsPerVolume_single(gsA, gateName="DNA+", chName="blabla", volUnit="ml", apc=FALSE, coV=125), "seems not to exist")
}) # EOT

test_that("makeEmptyEvPVDataFrame", {
    expect_type(makeEmptyEvPVDataFrame(10), "list")
    expect_equal(length(makeEmptyEvPVDataFrame(10)), 10)
    expect_s4_class(makeEmptyEvPVDataFrame(10)[[7]], "eventsPV")
}) # EOT

test_that("getEventsPerVolume", {
    expect_equal(length(getEventsPerVolume(gsA)), 1)
    expect_equal(length(getEventsPerVolume(gsDouble)), 2)
    expect_s4_class(getEventsPerVolume(gsDouble)[[2]], "eventsPV")
}) # EOT
# getEventsPerVolume(gsDouble)


flowWorkspace::pData(gs)[,"volume"][1] <- NA
test_that("checkForVolumeData", {
    expect_error(checkForVolumeData(gs, stn), "no volume-data available")
    expect_true(checkForVolumeData(gsA, stn))
    s2 <- stn
    s2$dV_use_volumeData <- FALSE
    expect_false(checkForVolumeData(gsA, s2))
}) # EOT

# fdm <- makefdmat(gsA, foN.dict = foN_dict) # does not work with the little data
# hmm. It looks like we need a fatter set of example files
remPathZip <- "https://github.com/bpollner/data/raw/main/fcsFiles/fcs_orb4.zip"
targZip <- paste0(pathToHome, "/fcs_orb4.zip")
download.file(remPathZip, targZip, mode="wb") ## DOWNLOAD ##  ## DOWNLOAD ##  ## DOWNLOAD ##  ## DOWNLOAD ##
aa <- unzip(targZip, exdir = pathToHome)
#
ptOrb4_fcs <- paste0(pathToHome, "/fcs_orb4")
ptOrb4 <- paste0(pathToHome, "/orb4")
foNPlots <- paste0(pathToHome, "/plots")
# now copy all the peripheral files for Orb4
ptOrb4_pa <- paste0(ptpInst, "/testHelpers/orb4")
aaa <- list.files(ptOrb4_pa)
orbFrom <- paste0(ptOrb4_pa, "/", aaa)
orbTo <- paste(pathToHome, "orb4", aaa, sep="/")
dir.create(paste0(pathToHome, "/orb4"), showWarnings = FALSE)
file.copy(orbFrom, orbTo, overwrite = TRUE)


# now make nice fat gating set
gsF <- makeAddGatingSet(fn=ptOrb4_fcs, foN.gateStrat = ptOrb4, type.gateStrat = "xlsx") # but have the right dictionary, gateStrat and gateDefinitions



fdm <- makefdmat(gsF, type.dict="xlsx", foN.dict = ptOrb4, expo=FALSE)


test_that("plotgates", {
    expect_output(plotgates(gsF, foN.plots = foNPlots))
    expect_null(plotgates(gsF, foN.plots = foNPlots))
    expect_null(plotgates(gsF, spl="C_treatment", foN.plots = foNPlots, foN.dict = ptOrb4, type.dict="xlsx"))
    expect_error(plotgates(1, foN.plots = foNPlots), "Please provide a gating set")
    expect_error(plotgates(gs, foN.plots = foNPlots), "Please provide valid channel names")
}) # EOT

gsP <- makeGatingSet(fn=ptOrb4_fcs, verbose = FALSE)
test_that("plotgates#2", {
    expect_null(plotgates(gsP, foN.plots = foNPlots, x="FSC.A", y="SSC.A"))
}) # EOT

#   plotgates(gsF, foN.plots = foNPlots) # gives the coordinate system error -- but it works.
#   plotgates(gsP, foN.plots = foNPlots, x="FSC.A", y="SSC.A", toPdf = F)
#   plotgates(gsP, foN.plots = foNPlots, x="FITC.A", y="PerCP.A", toPdf = F)

# now also test the "show" methods
test_that("show methods", {
    expect_output(show(gs))
    expect_output(show(gsA))
    expect_output(show(fdm)) # showing fdmat
    expect_output(show(fdm[[1]])) # showing fdmat_single
}) # EOT


ptRaw <- paste0(pathToHome, "/rawdata")

test_that("checkForGateNr", {
    expect_equal(checkForGateNr(fdm, 2), 2)
    expect_equal(checkForGateNr(fdm, "DNA+"), 1)
    expect_error(checkForGateNr(fdm, "blabla+"), "does not seem to exist")
    expect_error(checkForGateNr(fdm, 4), "does not exist")
}) # EOT
# checkForGateNr(fdm, 1)

test_that("cutFdmatToGate", {
    expect_error(cutFdmatToGate(fdm, gate=NULL), "Please provide a gate name or a number")
    aaa <- cutFdmatToGate(fdm, 1)
    expect_equal(length(cutFdmatToGate(aaa)), 1) # gets back without cutting down
    expect_error(cutFdmatToGate(fdm, gate="bla"), "Sorry, the gate 'bla' does not seem to exist")
    expect_error(cutFdmatToGate(fdm, gate=3), "gate nr 3 does not exist")
    expect_s4_class(cutFdmatToGate(fdm, gate=1), "fdmat")
}) # EOT

test_that("exportFdmatData", {
    aaa <- cutFdmatToGate(fdm, 1)
    expect_null(exportFdmatData(fdm, expo.gate=NULL, expo.name=".", expo.type="xlsx", expo.folder=ptRaw))
    expect_null(exportFdmatData(aaa, expo.gate=NULL, expo.name=".", expo.type="csv", expo.folder=ptRaw))
    expect_error(exportFdmatData(fdm, expo.gate=NULL, expo.name=".", expo.type="blabla", expo.folder=ptRaw), "provide either 'csv' or 'xlsx'")
    expect_null(exportFdmatData(fdm, expo.gate=1, expo.name=".", expo.type="xlsx", expo.folder=ptRaw))
    expect_message(exportFdmatData(fdm, expo.gate=NULL, expo.name=".", expo.type="csv", expo.folder=ptRaw), "input object has been cut down")
}) # EOT
# exportFdmatData(fdm, expo.gate=NULL, expo.name=".", expo.type="csv", expo.folder=ptRaw)


test_that("makefdmat", {
    expect_s4_class(makefdmat(gsF, type.dict="xlsx", foN.dict = ptOrb4, expo=FALSE), "fdmat")
    expect_s4_class(makefdmat(gsF, type.dict="xlsx", foN.dict = ptOrb4, expo=FALSE, dev=TRUE), "fdmat")
    expect_s4_class(makefdmat(gsF, type.dict="xlsx", foN.dict = ptOrb4, expo.folder=ptRaw), "fdmat")
}) # EOT
# makefdmat(gsF, type.dict="xlsx", foN.dict = ptOrb4, expo.folder=ptRaw)

test_that("fd_save", {
    expect_null(fd_save(fdm, fns=NULL, expo.folder=ptRaw))
    expect_null(fd_save(fdm, fns="yea", expo.folder=ptRaw))
}) # EOT

test_that("fd_load", {
    expect_s4_class(fd_load(fn=NULL, expo.folder=ptRaw), "fdmat")
    cnfn <- "flscData_gateStrat.xlsx_fdmatObj_yea"
    expect_s4_class(fd_load(fn=cnfn, expo.folder=ptRaw), "fdmat")
    expect_error(fd_load(fn="blabla", expo.folder=ptRaw), "fdmat-object 'blabla' does not seem to exist")
}) # EOT
# fd_load(fn=NULL, expo.folder=ptRaw)



#### now flowdexit. Yea. ####
test_that("flowdexit", {
    expect_s4_class(flowdexit(fn=ptOrb4_fcs, foN.gateStrat = ptOrb4, type.gateStrat = "xlsx", type.dict="xlsx", foN.dict = ptOrb4, expo.folder=ptRaw), "fdmat")
}) # EOT
# flowdexit(fn=ptOrb4_fcs, foN.gateStrat = ptOrb4, type.gateStrat = "xlsx", type.dict="xlsx", foN.dict = ptOrb4, expo.folder=ptRaw)





stn <- source(paste0(ptpInst, "/flowdex_settings.R"))$value # conveniently here again

#### Accessory Functions ####

## apply bandpass
test_that("getFlscX", {
    expect_type(getFlscX(fdm[[1]]), "double")
    expect_equal(length(getFlscX(fdm[[1]])), 219)
}) # EOT

test_that("checkBandpass", {
    expect_null(checkBandpass(c(300, 600)))
    expect_error(checkBandpass(c(900, 600)), "has to be smaller")
    expect_error(checkBandpass(c(900, 600, 120)), "numeric length two")
}) # EOT

bapa <- c(1600, 2400)
test_that("applyBandpass", {
    expect_s4_class(applyBandpass(fdm, bapa, 1), "fdmat")
    expect_s4_class(applyBandpass(fdm, bapa, 2)[[2]], "fdmat_single")
}) # EOT
# applyBandpass(fdm, bapa, 1)


## plotFlscDist
test_that("makeColors", {
    expect_type(makeColors(8, stn), "character")
    expect_equal(length(makeColors(8, stn)), 8)
}) # EOT

test_that("plotFlscDist", {
    expect_null(plotFlscDist(fdm, foN.plots=foNPlots))
    expect_null(plotFlscDist(fdm, gate=NULL, toPdf=FALSE, foN.plots=foNPlots))
    expect_null(plotFlscDist(fdm, gate=1, toPdf=FALSE, foN.plots=foNPlots))
    expect_null(plotFlscDist(fdm, gate=2, toPdf=FALSE, foN.plots=foNPlots))
    expect_null(plotFlscDist(fdm, gate=1, spl="C_treatment", toPdf=FALSE, foN.plots=foNPlots))
    expect_null(plotFlscDist(fdm, gate=1, toPdf=FALSE, foN.plots=foNPlots, clt=c(1,1,1,2,2,2), ccol=c(1,2,3)))
    expect_error(plotFlscDist(fdm, gate=1, spl="C_treatmentXX", toPdf=FALSE, foN.plots=foNPlots), "is not present")
    expect_null(plotFlscDist(fdm, gate=1, spl=NULL, toPdf=FALSE, ylog=TRUE, foN.plots=foNPlots))
    expect_null(plotFlscDist(fdm, gate=1, toPdf=TRUE, foN.plots=foNPlots))
    expect_null(plotFlscDist(fdm, gate=1, toPdf=TRUE, spl="C_treatment", foN.plots=foNPlots))
}) # EOT
# plotFlscDist(fdm, gate=1, toPdf=TRUE, spl="C_treatment", foN.plots=foNPlots)






#### Switching out the Original flowdex_settings.R file ####
# the purpose is to test with the following keys changed:
# dV_ignoreEdgePercent: from FALSE to 20
# dV_use_volumeData: from TRUE to FALSE
# dD_useDictionary: from TRUE to FALSE

# first we have to replace the original settings.R file with an other one
ptSeOrig <- paste0(ptpInst, "/testHelpers/settingsFile/flowdex_settings_original.R")
ptSeManip_vol <- paste0(ptpInst, "/testHelpers/settingsFile/flowdex_settings_manip_vol_FALSE.R")
ptSeManip_dic <- paste0(ptpInst, "/testHelpers/settingsFile/flowdex_settings_manip_dic_FALSE.R")
ptSeManip_dic_vol <- paste0(ptpInst, "/testHelpers/settingsFile/flowdex_settings_manip_dic_vol_FALSE.R")
ptSeManip_igp <- paste0(ptpInst, "/testHelpers/settingsFile/flowdex_settings_manip_igp_20.R")
#
ptSeTarget <- paste0(ptpInst, "/flowdex_settings.R")
#
copySettings <- function(origin) {
    file.copy(origin, ptSeTarget, overwrite=TRUE)
} # EOF


### test for igp = 20 ###
test_that("makefdmat", {
    copySettings(ptSeManip_igp)
    expect_s4_class(makefdmat(gsF, type.dict="xlsx", foN.dict = ptOrb4, expo=FALSE, dev=TRUE), "fdmat")
    copySettings(ptSeOrig)
}) # EOT
# makefdmat(gsF, type.dict="xlsx", foN.dict = ptOrb4, expo=FALSE, dev = TRUE)



### test fpr dV_use_volumeData = FALSE ###
test_that("makefdmat not useing volume data", {
    copySettings(ptSeOrig)
    expect_null(plotFlscDist(makefdmat(gsF,  type.dict="xlsx", foN.dict = ptOrb4, expo=FALSE), gate=1, toPdf=F))
    copySettings(ptSeManip_vol)
    expect_null(plotFlscDist(makefdmat(gsF,  type.dict="xlsx", foN.dict = ptOrb4, expo=FALSE), gate=1, toPdf=F))
    copySettings(ptSeOrig)
}) # EOT

## but now we also have to simulate that we do NOT have volume data in the fcs files
# first copy them, then remove all volume data in the orb4 fcs files
allOrb4 <- list.files(ptOrb4_fcs, full.names = TRUE)
ptFcsOrb4_noVol <- paste0(pathToHome, "/fcs_orb4_noVol")
dir.create(ptFcsOrb4_noVol)
file.copy(allOrb4, ptFcsOrb4_noVol)
# now remove all volume data in these fcs files
repairVolumes(fn = ptFcsOrb4_noVol, vol = NA, includeAll = TRUE, confirm = FALSE)

test_that("flowdexit with no volume data in the fcs files", {
    copySettings(ptSeManip_vol)
    expect_null(plotFlscDist(flowdexit(fn=ptFcsOrb4_noVol, foN.gateStrat = ptOrb4, foN.dict = ptOrb4, expo = FALSE, stf = FALSE), 1, toPdf = FALSE, ti="No Volume Data in the FCS Files"))
    copySettings(ptSeOrig)
}) # EOT


### test fpr dD_useDictionary = FALSE ###
# first NOT use the dictionary on a set of fcs files having a sample ID

test_that("noDic WITH sample ID present", {
    copySettings(ptSeManip_dic)
    expect_s4_class(flowdexit(fn=ptOrb4_fcs, foN.gateStrat = ptOrb4, foN.dict = ptOrb4, expo = TRUE, expo.folder=ptRaw), "fdmat")
    fdm_noDic <- flowdexit(fn=ptOrb4_fcs, foN.gateStrat = ptOrb4, foN.dict = ptOrb4, expo = TRUE, expo.folder=ptRaw)
    gsDic <- gsenv$gatingSet
    expect_error(plotgates(gsDic, spl="C_treatment", toPdf = FALSE ), "it is not possible to use 'spl'")
    copySettings(ptSeOrig)
}) # EOT

# now remove all sample IDs of the orb4 fcs files
allOrb4 <- list.files(ptOrb4_fcs, full.names = TRUE)
ptFcsOrb4_noSID <- paste0(pathToHome, "/fcs_orb4_noSID")
dir.create(ptFcsOrb4_noSID)
file.copy(allOrb4, ptFcsOrb4_noSID, overwrite = TRUE)

finas <- list.files(ptFcsOrb4_noSID)
fs <- repairSID(fn = ptFcsOrb4_noSID)
for (i in seq_along(finas)) {
    repairSID(fs, fn = ptFcsOrb4_noSID, name = finas[i], newSID = "", confirm = FALSE)
} # end for i
# fs <- repairSID(fn = ptFcsOrb4_noSID)
# flowWorkspace::pData(fs)
#
test_that("noDic withOUT sample ID present", {
    copySettings(ptSeManip_dic)
    expect_s4_class(flowdexit(fn=ptFcsOrb4_noSID, foN.gateStrat = ptOrb4, foN.dict = ptOrb4, expo = TRUE, expo.folder=ptRaw), "fdmat")
    copySettings(ptSeOrig)
}) # EOT

## and now, just to be sure, all with noDic and noVol
# first delete all volume data in the noSID fcs files
repairVolumes(fn = ptFcsOrb4_noSID, vol = NA, includeAll = TRUE, confirm = FALSE)

test_that("noDic noVolume in the fcs files", {
    copySettings(ptSeManip_dic_vol)
    expect_s4_class(flowdexit(fn=ptFcsOrb4_noSID, foN.gateStrat = ptOrb4, foN.dict = ptOrb4, expo = TRUE, expo.folder=ptRaw), "fdmat")
    copySettings(ptSeOrig)
}) # EOT

copySettings(ptSeOrig)
