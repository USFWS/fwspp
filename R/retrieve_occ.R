retrieve_occ <- function(props, prop, buffer, scrub,
                         timeout = NULL) {

  org_name <- prop
  short_org <- Cap(org_name) %>% shorten_orgnames()
  prop <- props[props$ORGNAME == prop, ]

  # Consider buffer
  if (buffer) prop <- buffer_prop(prop, buffer)

  message("\nProcessing ", short_org)

  # Maximum of GBIF and BISON record count is used to determine whether
  # a property is divided into smaller pieces
  try_gbif_count <- try_verb_n(gbif_count)
  try_bison_count <- try_verb_n(bison_count)

  # Split property if it spans International Date Line
  # Check is likely not foolproof, but seems safe for USFWS props
  prop_ch <- sf::st_convex_hull(prop)
  if (diff(range(sf::st_bbox(prop_ch))) > 350)
    prop <- split_at_idl(prop)

  # If substantial # of records, check if the area ratio of a property
  # to its bounding box is small enough to warrant further division
  # for efficiency
  if (max(try_gbif_count(prop), try_bison_count(prop)) > 100000)
    prop <- split_prop(prop)

  occ_recs <- vector(nrow(prop), mode = "list")
  safe_gets <- purrr::safely(manage_gets)
  for (i in seq_along(occ_recs)) {
    i_recs <- safe_gets(prop[i, ], timeout)
    if (is_error(i_recs)) {
      occ_recs[[i]] <- i_recs$error
      break
    }
    i_recs <- i_recs$result
    if (nrow(i_recs) == 0) i_recs <- NULL
    occ_recs[[i]] <- i_recs
  }

  errs <- sapply(occ_recs, is_error)
  if (any(errs))
    return(occ_recs[[min(which(errs))]])
  occ_recs <- bind_rows(occ_recs)
  if (nrow(occ_recs) == 0) return(NULL)

  # Filter to boundaries of interest
  occ_recs <- clip_occ(occ_recs, prop)
  if (nrow(occ_recs) == 0) return(NULL)

  # Scrubbing, if requested
  if (scrub != "none") {
    safe_scrub <- purrr::safely(scrub_occ)
    occ_recs <- safe_scrub(occ_recs, scrub)
    if (is_error(occ_recs)) return(occ_recs$error)
    occ_recs <- occ_recs$result
  }

  occ_recs %>%
    mutate(org_name = org_name) %>%
    select(.data$org_name, everything(), -.data$media_url, -.data$cat_no) %>%
    arrange(.data$sci_name, .data$year, .data$month, .data$day)

}
