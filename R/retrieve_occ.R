retrieve_occ <- function(props, prop, buffer, scrub, itis,
                         timeout = NULL) {

  org_name <- prop
  short_org <- Cap(org_name) %>% shorten_orgnames()
  prop <- props[props$ORGNAME == prop, ]

  # Consider buffer
  if (buffer) prop <- buffer_prop(prop, buffer)

  message("\nProcessing ", short_org)

  # GBIF record count is used to determine whether a property is
  # divided into smaller pieces
  try_gbif_count <- try_verb_n(gbif_count)

  # Split property if it spans International Date Line
  # and check if the area ratio of a property to its bounding
  # box, in combination with the number of records, warrants
  # further division for efficiency
  prop <- split_prop(prop, try_gbif_count)
  if (is_error(prop))  {
    warning("Property splitting failed.")
    return(prop_split)
  }
  if (nrow(occ_recs) == 0) return(NULL)

  # Filter to boundaries of interest
  occ_recs <- clip_occ(occ_recs, prop)
  if (nrow(occ_recs) == 0) return(NULL)

  # ITIS joining and scrubbing, if requested
  if (itis) {
    occ_recs <- join_itis(occ_recs)
    if (is_error(occ_recs)) return(occ_recs)
  }

  if (scrub != "none") {
    try_scrub <- try_verb_n(scrub_occ, 1)
    occ_recs <- try_scrub(occ_recs, scrub)
    if (is_error(occ_recs)) return(occ_recs$error)
    occ_recs <- occ_recs$result
  }

  occ_recs %>%
    mutate(org_name = org_name) %>%
    select(.data$org_name, everything(), -.data$media_url, -.data$cat_no) %>%
    arrange(.data$class, .data$sci_name, .data$com_name, .data$year,
            .data$month, .data$day)

}
