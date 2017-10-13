retrieve_occ <- function(props, prop, buffer, scrub, itis, grbio,
                         timeout) {

  org_name <- prop
  short_org <- Cap(org_name) %>% shorten_orgnames()
  prop <- props[props$ORGNAME == prop, ]

  # Consider buffer
  if (buffer) prop <- buffer_prop(prop, buffer)

  msg <- c("\nProcessing", short_org)

  occ_recs <- manage_gets(prop, grbio, timeout)
  if (is_error(occ_recs)) return(occ_recs)
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
    if (is_error(occ_recs)) return(occ_recs)
  }

  occ_recs %>%
    mutate(org_name = org_name) %>%
    select(.data$org_name, everything(), -.data$media_url, -.data$cat_no) %>%
    arrange(.data$class, .data$sci_name, .data$com_name, .data$year,
            .data$month, .data$day)

}
