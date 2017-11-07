manage_gets <- function(prop, timeout) {

  # TEST IF FLEXIBILITY IN THESE REQUIREMENTS
  stopifnot(nrow(prop) == 1 &&
              sf::st_geometry_type(prop) %in% c("POLYGON", "MULTIPOLYGON"))

  # HTTP requests by latitude/longitude range, or radius around central loc
  prop_bb <- matrix(sf::st_bbox(prop), 2)
  # Ensure very small properties are queried
  lat_range <- prop_bb[2, ] + c(-0.00006, 0.00006)
  lon_range <- prop_bb[1, ] + c(-0.00006, 0.00006)
  radius <- geosphere::distVincentyEllipsoid(rowMeans(prop_bb), t(prop_bb)) %>%
    ceiling() %>% max()

  # BISON record count may be used to determine the HTTP request timeout
  try_bison_count <- try_verb_n(bison_count)
  q_recs <- try_bison_count(lat_range, lon_range)

  # Compare and set timeout programmatically, if not specified by user
  # Timeout is based on BISON queries as they are typically the largest
  # contiguous downloads
  if (!is.null(timeout)) {
    prog_recs <- est_nrecs(timeout)
    if (prog_recs < q_recs)
      message("Your timeout setting may be too short. Watch for repeated ",
              "HTTP timeout errors and adjust accordingly.")
  } else timeout <- est_timeout(min(125000, q_recs))

  #############################################################################
  ## Retrieve and standardize occurrence records from biodiversity databases ##
  #############################################################################

  # GBIF
  gbif_recs <- get_GBIF(prop, timeout)
  if (gbif_recs$meta$count > 0)
    gbif_recs <- clean_GBIF(gbif_recs)
  else
    gbif_recs <- NULL

  ## BISON
  bison_recs <- get_BISON(lat_range, lon_range, timeout)
  if (!is.null(bison_recs))
    bison_recs <- clean_BISON(bison_recs)

  ## iDigBio
  idb_recs <- get_iDigBio(lat_range, lon_range, timeout)
  if (nrow(idb_recs) > 0)
    idb_recs <- clean_iDigBio(idb_recs)
  else
    idb_recs <- NULL

  ## VertNet
  vn_recs <- get_VertNet(rowMeans(prop_bb), radius, timeout)
  if (!is.null(vn_recs))
    vn_recs <- clean_VertNet(vn_recs)

  ## Berkeley 'Ecoinformatics' Eengine
  ee_recs <- get_EcoEngine(lat_range, lon_range, timeout)
  if (!is.null(ee_recs))
    ee_recs <- clean_EcoEngine(ee_recs)

  ## AntWeb
  aw_recs <- get_AntWeb(lat_range, lon_range, timeout)
  if (!is.null(aw_recs))
    aw_recs <- clean_AntWeb(aw_recs)

  #############################################################################
  ## Consolidate standardized occurrence records from biodiversity databases ##
  #############################################################################
  bind_rows(gbif_recs, bison_recs, idb_recs, vn_recs, ee_recs, aw_recs) %>%
    # Drop records with no species ID or monomials (e.g., genus only)
    filter(!is.na(.data$sci_name),
           vapply(strsplit(.data$sci_name, "\\W+"), length, integer(1)) == 2)

}
