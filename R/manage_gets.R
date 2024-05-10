#' Runs get occurrence functions for all the biodiversity databases
#'
#' @param prop a FWS property boundary returned by \code{\link{find_fws}}
#' @param timeout numeric; if specified, serves as a multiplier for the timeout
#'  value calculated internally (e.g., \code{timeout = 2} doubles the amount of
#'  time to allow for HTTP requests to process. By default (\code{timeout = NULL}),
#'  the query timeout is set programmatically and conservatively.
#'
#' @return data frame of consolidated and standardized occurrence records from biodiversity databases
#'
#' @noRd
manage_gets <- function(prop, timeout, start_date) {

  # TEST IF FLEXIBILITY IN THESE REQUIREMENTS
  stopifnot(nrow(prop) == 1 &&
              sf::st_geometry_type(prop) %in% c("POLYGON", "MULTIPOLYGON"))

  # HTTP requests by latitude/longitude range, or radius around central loc
  bb <- matrix(sf::st_bbox(prop), 2)
  # Ensure very small properties are queried
  lat_range <- bb[2, ] + c(-0.00006, 0.00006)
  lon_range <- bb[1, ] + c(-0.00006, 0.00006)
  radius <- geosphere::distVincentyEllipsoid(rowMeans(bb), t(bb))  # **Depends on sp package (2023-09-06)**
  radius <- ceiling(max(radius) / 100) * 100

  # GBIF record count used to determine the HTTP request timeout
  try_gbif_count <- try_verb_n(gbif_count)
  #q_recs <- try_gbif_count(prop)
  today <- as.POSIXlt(Sys.time())

  q_recs <- try_gbif_count(prop,lastInterpreted = paste0(format(start_date, format="%Y"),"-",
                                                         format(start_date, format="%m"),"-",
                                                         format(start_date, format="%d"),",",
                                                         format(today, format="%Y"),"-",
                                                         format(today, format="%m"),"-",
                                                         format(today, format="%d")))

  # Compare and set timeout programmatically, if not specified by user
  tox <- timeout
  timeout <- est_timeout(q_recs)
  if (!is.null(tox)) timeout <- timeout * tox
  message("Server request timeout set to ", timeout, " seconds (x4 for GBIF).")
  prog_recs <- est_nrecs(timeout)
  if (prog_recs < q_recs)
    message("Your timeout setting may be too short. Watch for repeated HTTP ",
            "timeout\nerrors and adjust the timeout parameter accordingly.")
  timeout <- timeout + 1 # Add one to ensure timeout is not zero

  #############################################################################
  ## Retrieve and standardize occurrence records from biodiversity databases ##
  #############################################################################

  # GBIF
  gbif_recs <- get_GBIF(prop, timeout, start_date = start_date)
  if (is.null(gbif_recs))
    gbif_recs <- NULL
  else
    gbif_recs <- clean_GBIF(gbif_recs)

  # iDigBio
  idb_recs <- get_iDigBio(lat_range, lon_range, timeout, start_date = start_date)
  if (!is.null(idb_recs))
    idb_recs <- clean_iDigBio(idb_recs)

  # VertNet
  vn_recs <- get_VertNet(rowMeans(bb), radius, timeout, prop = prop, start_date = start_date)
  if (!is.null(vn_recs))
    vn_recs <- clean_VertNet(vn_recs)

  # Berkeley 'Ecoinformatics' Engine (EcoEngine R package is depricated)
  # ee_recs <- get_EcoEngine(lat_range, lon_range, timeout)
  # if (!is.null(ee_recs))
  #   ee_recs <- clean_EcoEngine(ee_recs)

  # ServCat
  ServCat_recs <- get_ServCat(prop, start_date = start_date)
  if (!is.null(ServCat_recs))
   ServCat_recs <- suppressMessages({clean_ServCat(ServCat_recs, prop = prop)})

  ## AntWeb  (not working for Alaska, so commented out)
  # aw_recs <- get_AntWeb(lat_range, lon_range, timeout)
  # if (!is.null(aw_recs))
  #   aw_recs <- clean_AntWeb(aw_recs)

  # Consolidate standardized occurrence records from biodiversity databases
  if(is.null(gbif_recs) & is.null(idb_recs) & is.null(vn_recs)){
    return(NULL)
  } else {
    bind_rows(gbif_recs,
              idb_recs,
              vn_recs,
              # ee_recs,
              ServCat_recs
              # aw_recs  # Drop AntWeb, doesn't work for AK
    ) %>%
      # Drop records with no species ID or monomials (e.g., genus only)
      filter(!is.na(sci_name),
             vapply(strsplit(sci_name, "\\W+"), length, integer(1)) == 2)
  }
}
