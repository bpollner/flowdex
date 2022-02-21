library(testthat)

# for manual only
devtools::load_all("~/Documents/RPS/flowdex_R/flowdex")
# delete all in tempdir !!
setwd("~/Documents/RPS/flowdex_R/flowdex")
rm(list=ls(all.names = TRUE))

# set up the stn object
if (dir.exists("inst")) {
	stn <- source("inst/flowdex_settings.R")$value
} else {
	stn <- source("flowdex_settings.R")$value
}

tmpdir <- tempdir()
homeDir <- "fld@home"
pathToHome <- paste0(tmpdir, "/", homeDir)
noFo <- paste0(pathToHome, "/blabla")
if (!dir.exists(pathToHome)) {
	dir.create(pathToHome) # simulating the users experiment home directory
}

##############
test_that("checkPath", {
    expect_true(checkPath(pathToHome))
    expect_error(checkPath(noFo))
}) # EOT

aaa <- "aaa"
test_that("createSingleFolder", {
    expect_true(createSingleFolder(tmpdir, aaa))
    expect_false(createSingleFolder(tmpdir, aaa))
}) # EOT

test_that("createFolders", {
    expect_true(createFolders(pathToHome, stn))
    expect_false(createFolders(pathToHome, stn))
}) # EOT

test_that("copyAllTemplates", {
    expect_true(copyAllTemplates(pathToHome, stn, onTest=TRUE))
 #   expect_false(copyAllTemplates(home=paste0(tmpdir, "/blabla"), stn, onTest=TRUE))
}) # EOT



