#' Retrieve species occurrence records from databases
#'
#' @param props a list of FWS properties
#' @param prop a character value indicating the FWS organizational name for a FWS property
#' @param buffer a numeric value indicating the distance in meters to buffer each FWS property
#' @param scrub character; one of "strict" (default), "moderate", or "none",
#'  indicating the extent to which to reduce the number of records returned for
#'  a given \code{fws}.
#' @param timeout numeric; if specified, serves as a multiplier for the timeout
#'  value calculated internally (e.g., \code{timeout = 2} doubles the amount of
#'  time to allow for HTTP requests to process. By default (\code{timeout = NULL}),
#'  the query timeout is set programmatically and conservatively.
#'
#' @importFrom magrittr %>%
#' @import sf
#' @importFrom purrr safely
#' @import dplyr
#'
#' @return a data frame of species occurrence records
retrieve_occ <- function(props, prop, buffer, scrub, timeout = NULL,start_date) {

  org_name <- prop
  short_org <- Cap(org_name) %>% shorten_orgnames()
  prop <- props[props$ORGNAME == prop, ]
  start_yr <- start_yr

  # Consider buffer
  if (buffer) prop <- buffer_prop(prop, buffer)

  message("\nProcessing ", short_org)

  # Maximum of GBIF and BISON record count is used to determine whether
  # a property is divided into smaller pieces#
  try_gbif_count <- try_verb_n(gbif_count)

  # Split property if it spans International Date Line
  # Check is likely not foolproof, but seems safe for USFWS props
  prop_ch <- sf::st_convex_hull(prop)
  if (diff(range(sf::st_bbox(prop_ch))) > 350)
    prop <- split_at_idl(prop)

  # If substantial # of records, check if the area ratio of a property
  # to its bounding box is small enough to warrant further division
  # for efficiency
  # [Turning this off until we can fix error, 2023-09]
  if (try_gbif_count(prop) > 100000){
    prop <- split_prop(prop)
  }

  occ_recs <- vector(nrow(prop), mode = "list")
  safe_gets <- purrr::safely(manage_gets)
  for (i in seq_along(occ_recs)) {
    i_recs <- safe_gets(prop[i, ], timeout,start_date)
    if (is_error(i_recs)) {
      occ_recs[[i]] <- i_recs$error
      break
    }
    i_recs <- i_recs$result
    if (!is.null(i_recs)){
      if (nrow(i_recs) == 0) i_recs <- NULL
      }
    occ_recs[[i]] <- i_recs
  }

  errs <- sapply(occ_recs, is_error)
  if (any(errs))
    return(occ_recs[[min(which(errs))]])
  occ_recs <- bind_rows(occ_recs)
  if (nrow(occ_recs) == 0) return(NULL)

  # Take out ServCat data because those data do not have coordinates
  ServCat_df <- occ_recs[occ_recs$bio_repo == "ServCat", ]
  occ_recs <- occ_recs[occ_recs$bio_repo != "ServCat", ]

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
  #add ServCat records if any
  ServCat_df <- get_ServCat(prop[1, ], start_date = start_date)
  if (!is.null(ServCat_df))
    ServCat_df <- suppressMessages({clean_ServCat(ServCat_df, prop = prop[1, ])})

  occ_recs %>%
    dplyr::mutate(org_name = org_name) %>%
    select(org_name, everything(), -media_url, -cat_no) %>%
    arrange(sci_name, year, month, day) %>%
    bind_rows(ServCat_df)
}
