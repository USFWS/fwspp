manage_gets <- function(prop, grbio, timeout = NULL) {

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

  #############################################################################
  ## Retrieve and standardize occurrence records from biodiversity databases ##
  #############################################################################

  # Compare and set timeout programmatically, if not specified by user
  # Timeout is based on GBIF queries, which are typically slowest
  try_gbif_count <- try_verb_n(gbif_count)
  q_recs <- try_gbif_count(prop)
  if (is_error(q_recs))  {
    warning("GBIF record count failed.")
    return(q_recs)
  }
  if (!is.null(timeout)) {
    prog_recs <- est_nrecs(timeout)
    if (prog_recs < q_recs)
      warning("Your timeout setting may be too short. Watch for repeated ",
              "HTTP timeout errors and adjust accordingly.")
  } else timeout <- est_timeout(min(125000, q_recs))

  # GBIF
  gbif_recs <- get_GBIF(prop, q_recs, timeout)
  if (is_error(gbif_recs)) return(gbif_recs)
  if (gbif_recs$meta$count > 0) {
    try_clean_GBIF <- try_verb_n(clean_GBIF, 1)
    gbif_recs <- try_clean_GBIF(gbif_recs, grbio)
    if (is_error(gbif_recs)) return(gbif_recs)
  } else gbif_recs <- NULL

  ## BISON
  bison_recs <- get_BISON(lat_range, lon_range, timeout)
  if (is_error(bison_recs)) return(bison_recs)
  if (!is.null(bison_recs)) {
    try_clean_BISON <- try_verb_n(clean_BISON, 1)
    bison_recs <- try_clean_BISON(bison_recs)
    if (is_error(bison_recs)) return(bison_recs)
  }

  ## iDigBio
  idb_recs <- get_iDigBio(lat_range, lon_range, timeout)
  if (is_error(idb_recs)) return(idb_recs)
  if (nrow(idb_recs) > 0) {
    try_clean_iDigBio <- try_verb_n(clean_iDigBio, 1)
    idb_recs <- try_clean_iDigBio(idb_recs)
    if (is_error(idb_recs)) return(idb_recs)
  } else idb_recs <- NULL

  ## VertNet
  vn_recs <- get_VertNet(rowMeans(prop_bb), radius, timeout)
  if (is_error(vn_recs)) return(vn_recs)
  if (!is.null(vn_recs)) {
    try_clean_VertNet <- try_verb_n(clean_VertNet, 1)
    vn_recs <- try_clean_VertNet(vn_recs, grbio)
    if (is_error(vn_recs)) return(vn_recs)
  }

  ## Berkeley 'Ecoinformatics' Eengine
  ee_recs <- get_EcoEngine(lat_range, lon_range, timeout)
  if (is_error(ee_recs)) return(ee_recs)
  if (!is.null(ee_recs)) {
    try_clean_EcoEngine <- try_verb_n(clean_EcoEngine, 1)
    ee_recs <- try_clean_EcoEngine(ee_recs, grbio)
    if (is_error(ee_recs)) return(ee_recs)
  }

  ## AntWeb
  aw_recs <- get_AntWeb(lat_range, lon_range, timeout)
  if (is_error(aw_recs)) return(aw_recs)
  if (!is.null(aw_recs)) {
    try_clean_AntWeb <- try_verb_n(clean_AntWeb, 1)
    aw_recs <- try_clean_AntWeb(aw_recs)
    if (is_error(aw_recs)) return(aw_recs)
  }

  #############################################################################
  ## Consolidate standardized occurrence records from biodiversity databases ##
  #############################################################################
  bind_rows(gbif_recs, bison_recs, idb_recs, vn_recs, ee_recs, aw_recs) %>%
    # Drop records with no species ID or monomials (e.g., genus only)
    filter(!is.na(.data$sci_name),
           vapply(strsplit(.data$sci_name, "\\W+"), length, integer(1)) == 2)

}
