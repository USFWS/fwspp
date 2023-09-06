manage_gets <- function(prop, timeout,start_yr) {
  start_yr<-start_yr

  # TEST IF FLEXIBILITY IN THESE REQUIREMENTS
  stopifnot(nrow(prop) == 1 &&
              sf::st_geometry_type(prop) %in% c("POLYGON", "MULTIPOLYGON"))

  # HTTP requests by latitude/longitude range, or radius around central loc
  bb <- matrix(sf::st_bbox(prop), 2)
  # Ensure very small properties are queried
  lat_range <- bb[2, ] + c(-0.00006, 0.00006)
  lon_range <- bb[1, ] + c(-0.00006, 0.00006)
  radius <- geosphere::distVincentyEllipsoid(rowMeans(bb), t(bb))
  radius <- ceiling(max(radius) /100) * 100

  # GBIF record count used to determine the HTTP request timeout
  try_gbif_count <- try_verb_n(gbif_count)
  q_recs <- try_gbif_count(prop)

  # Compare and set timeout programmatically, if not specified by user
  # Timeout is based on BSION queries as they are typically the largest
  tox <- timeout
  timeout <- est_timeout(q_recs)
  if (!is.null(tox)) timeout <- timeout * tox
  message("Server request timeout set to ", timeout, " seconds (x4 for GBIF).")
  prog_recs <- est_nrecs(timeout)
  if (prog_recs < q_recs)
    message("Your timeout setting may be too short. Watch for repeated HTTP ",
            "timeout\nerrors and adjust the timeout parameter accordingly.")

  #############################################################################
  ## Retrieve and standardize occurrence records from biodiversity databases ##
  #############################################################################

  ## GBIF
  gbif_recs <- get_GBIF(prop, timeout,start_yr=start_yr)
  if (is.null(gbif_recs))
    gbif_recs <- NULL
  else
    gbif_recs <- clean_GBIF(gbif_recs)

  # iDigBio
  idb_recs <- get_iDigBio(lat_range, lon_range, timeout)
  #if (nrow(idb_recs) > 0)
  if (is.null(idb_recs))
    idb_recs <- NULL
  else
    idb_recs <- clean_iDigBio(idb_recs)

  ## VertNet
  vn_recs <- get_VertNet(rowMeans(bb), radius, timeout, prop = prop)
  if (!is.null(vn_recs))
    vn_recs <- clean_VertNet(vn_recs)

  ## Berkeley 'Ecoinformatics' Engine

  ee_recs <- get_EcoEngine(lat_range, lon_range, timeout)
  if (!is.null(ee_recs))
    ee_recs <- clean_EcoEngine(ee_recs)

  ##ServCat
  ServCat_recs <- get_ServCat(prop)
  if (!is.null(ServCat_recs))
    ServCat_recs <- suppressMessages({clean_ServCat(ServCat_recs, prop = prop)})

  ## AntWeb  (not working for Alaska, so commented out)
  # aw_recs <- get_AntWeb(lat_range, lon_range, timeout)
  # if (!is.null(aw_recs))
  #   aw_recs <- clean_AntWeb(aw_recs)

  #############################################################################
  ## Consolidate standardized occurrence records from biodiversity databases ##
  #############################################################################

  bind_rows(gbif_recs,
            idb_recs,
            vn_recs,
            ee_recs,
            ServCat_recs
            # aw_recs
  ) %>%
    # Drop records with no species ID or monomials (e.g., genus only)
    filter(!is.na(sci_name),
           vapply(strsplit(sci_name, "\\W+"), length, integer(1)) == 2)

}
