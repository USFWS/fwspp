#' Install the most current (2017-08-24) USFWS National Wildlife Refuge cadastral geodatabase.
#'
#' This function must be run prior to first use of \code{fw_spp} (which will prompt you
#'  if necessary).  This function can also be used to update to the most recent
#'  cadastral geodatabase if the \code{fwspp} package hasn't yet incorporated it.  However,
#'  the cadastral geodatabase is updated rather infrequently, and updating will take
#'  several minutes (the file is ~ 100 MB), so it may be best to check if such an update is
#'  necessary prior to running this function.  Visit
#'  \url{https://catalog.data.gov/dataset?q="FWS+Cadastral+Database"} and check the date.
#' @export
#' @examples
#' \dontrun{
#' install_fws_cadastral()
#' }

install_fws_cadastral <- function() {

  if (!(curl::has_internet())) {
    stop(paste(strwrap(
      paste("No internet connection detected.  Please try again later when a stable",
            "connection is available.  Sorry for the inconvenience.")),
      collapse = "\n"))
  } else {
    tmp <- tempfile(fileext = ".zip")
    utils::download.file("https://ecos.fws.gov/ServCat/DownloadFile/126665", tmp,
                         mode = "wb", cacheOK = FALSE)
    utils::unzip(tmp, overwrite = TRUE,
                 exdir = system.file("extdata", package = "fwspp"))

    # Update list of refuges
    gdb <- system.file("extdata", "FWSCadastral.gdb", package = "fwspp")
    r <- sf::st_read(gdb, "FWSInterest", stringsAsFactors = FALSE, quiet = TRUE) %>%
      as.data.frame() %>%
      dplyr::select(.data$ORGNAME, .data$FWSREGION, .data$RSL_TYPE) %>%
      dplyr::mutate(ORGNAME = gsub(" '", "'", .data$ORGNAME),
                    ORGNAME = gsub("([A-Z])(\\.)([A-Z])", "\\1\\2 \\3", .data$ORGNAME)) %>%
      unique()
    saveRDS(r, file = file.path(system.file("extdata", package = "fwspp"),
                                "refuge_info.rds"))
    message("USFWS Cadastral Database installed/updated successfully")

    invisible(NULL)

  }
}
