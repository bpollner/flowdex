#' @examples
#' td <- tempdir()
#' data_source <- "https://github.com/bpollner/data/raw/main/flowdex_examples/flowdex_examples.zip"
#' check_download_data(td, data_source)
#' exp_home <- paste0(td, "/flowdex_examples")
#' old_wd <- getwd()
#' setwd(exp_home)
#' #
