# Helper to run function up a function a user-specified number of times with increasing
# backoff times (modified from `VERB_n` in googlesheets package); errors (after n
# attempts) are kept and passed along.  Currently retrying on all errors until timeouts,
# brief server errors, and non-negotiable errors can be better differentiated. This
# function is also used to capture errors for non-HTTP related functions (e.g., cleaning
# and scrubbing)
try_verb_n <- function(verb, n = 3) {
  function(...) {
    for (i in seq_len(n)) {
      if (i == n)
        out <- verb(...)
      else
        out <- try(verb(...), silent = TRUE)
      if (!is_error(out) || i == n) break
      wait <- stats::runif(1, min(5 ^ i, 120), min(5 ^ (i + 1), 240))
      mess <- "Timeout or error on attempt %d. Retrying in %0.0f s."
      message(sprintf(mess, i, wait))
      Sys.sleep(wait)
    }
    out
  }
}

is_error <- function(obj) {
  if (inherits(obj, "list") &&
      identical(names(obj), c("result", "error")))
    !is.null(obj$error)
  else inherits(obj, "error") | inherits(obj, "try-error")
}

fws_url <- function() "https://ecos.fws.gov/ServCat/DownloadFile/126665"

gbif_count <- function(prop, ...) {
  n <- rgbif::occ_search(limit = 0, ...,
                         geometry = get_wkt(prop))
  n$meta$count
}

bison_count <- function(prop) {
  prop_bb <- matrix(sf::st_bbox(prop), 2)
  lat_range <- prop_bb[2, ] + c(-0.00006, 0.00006)
  lon_range <- prop_bb[1, ] + c(-0.00006, 0.00006)
  con <- solrium::SolrClient$new(host = "bison.usgs.gov", scheme = "https",
                                 path = "solr/occurrences/select", port = NULL)
  q_recs <- solrium::solr_search(
    conn = con,
    params = list(q = '*:*',
                  fq = list(paste0("decimalLatitude:[",
                                   paste(lat_range, collapse = " TO "), "]"),
                            paste0("decimalLongitude:[",
                                   paste(lon_range, collapse = " TO "), "]"))),
    rows = 1, callopts = list(timeout = 10))
  attr(q_recs, "numFound")
}

est_timeout <- function(n_recs) {
  ceiling(0.005 * n_recs)
}

est_nrecs <- function(timeout) {
  recs <- round(timeout / 0.005)
  100 * (recs %/% 100 + as.logical(recs %% 100))
}

get_ee_metadata <- function(url) {
  ee_meta <- jsonlite::fromJSON(url)
  ee_meta[["metadataurl"]]
}

get_unit_codes <- function(orgnames = NULL) {
  base_url <- "https://ecos.fws.gov/primr/api/refuge/geo.json"
  try_JSON <- try_verb_n(jsonlite::fromJSON, 2)
  prop_info <- try_JSON(base_url)
  prop_info <- prop_info$features$properties %>%
    mutate(org_name = toupper(.data$orgnameabbr)) %>%
    select(.data$org_name, UnitCode = .data$costCenterCode)
  if (is.null(orgnames)) return(prop_info)
  filter(prop_info,
         grepl(paste(orgnames, collapse = "|"), .data$org_name, ignore.case = TRUE))
}
