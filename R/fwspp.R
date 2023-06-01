#' \code{fwspp} package
#'
#' See the \href{https://github.com/adamdsmith/fwspp}{README} on GitHub
#'
#' @docType package
#' @name fwspp
#' @import dplyr
#' @import sf
#' @importFrom rlang .data
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines this is a test
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
