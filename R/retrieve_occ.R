retrieve_occ <- function(props, prop, buffer, scrub, itis, grbio,
                         plot, timeout, area_cutoff) {

  org_name <- prop
  short_org <- Cap(org_name) %>% shorten_orgnames()
  prop <- props[props$ORGNAME == prop, ]

  # Consider buffer
  if (buffer) prop <- buffer_prop(prop, buffer)

  # Consider splitting property to facilitate queries
  prop <- split_prop(prop, area_cutoff)

  n_polys <- nrow(prop)

  msg <- c("\nProcessing", short_org)
  if (n_polys > 0)
    msg <- c(msg, "in", n_polys, "parts...")
  message(paste(msg, collapse = " "))
  if (plot) plot(prop, col = "#d9d9d9", main = short_org)

  occ_recs <- vector(length = n_polys, mode = "list")
  for (i in seq_len(n_polys)) {
    poly <- prop[i, ]
    if (plot) {
      Sys.sleep(0.1)
      plot(poly, col = "#74c476", add = TRUE)
    }
    tmp <- manage_gets(poly, grbio, timeout)
    occ_recs[[i]] <- tmp
    if (is_error(tmp)) break
    if (plot) plot(poly, col = "#006d2c", add = TRUE)
  }

  if (any(sapply(occ_recs, is_error))) return(occ_recs)
  occ_recs <- bind_rows(occ_recs)
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
