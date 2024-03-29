#' Install the most current USFWS cadastral geodatabase
#'
#' This function must be run prior to first use of \code{fws_occ} (which will prompt you
#' if necessary).  This function can also be used to update to the most recent
#' cadastral geodatabase if the \code{fwspp} package hasn't yet incorporated it.  However,
#' the cadastral geodatabase is updated rather infrequently, and updating will take
#' several minutes (the file is ~ 100 MB), so it may be best to check if such an update is
#' necessary prior to running this function.  Visit
#' \url{https://catalog.data.gov/dataset?q="FWS+Cadastral+Database"} and check the date.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_fws_cadastral()
#' }

install_fws_cadastral <- function() {

  if (!(curl::has_internet()))
    stop(wrap_message(
      paste("No internet connection detected.  Please try again later when a stable",
            "connection is available. Sorry for the inconvenience.")))

  tmp <- tempfile(fileext = ".zip")
  dl <- try(httr::GET(fws_url(),
                      httr::write_disk(tmp),
                      httr::progress()), silent = TRUE)
  if (is_error(dl)) {
    utils::browseURL(fws_url())
    stop(wrap_message(
      paste("Automatic cadastral installation failed. Unzip the contents of",
            "the currently downloading file (i.e., 'FWSCadastral.gdb') into",
            system.file("extdata", package = "fwspp"), "and then run",
            "`prep_cadastral`. Sorry for the inconvenience.")))
  } else {
    utils::unzip(tmp, overwrite = TRUE,
                 exdir = system.file("extdata", package = "fwspp"))
    message("USFWS Cadastral Database downloaded and installed successfully.")
  }

  suppressWarnings(prep_cadastral())

  # Update list of properties
  fws_interest <- system.file("extdata", "fws_interest.rds", package = "fwspp")
  r <- readRDS(fws_interest) %>% as.data.frame()
  saveRDS(r, file = file.path(system.file("extdata", package = "fwspp"),
                              "fws_info.rds"))
  invisible()
}
