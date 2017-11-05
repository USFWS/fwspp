# Helper to run function up to 3 times with increasing backoff times (unless wait
# is set) if request produces any error (modified from `VERB_n` in googlesheets package);
# errors (after n attempts) are kept and passed along.  Currently retrying on all
# errors until timeouts, brief server errors, and non-negotiable errors can be better
# differentiated. Note that this function is also used to capture errors for
# non-HTTP related functions (e.g., cleaning and scrubbing)
try_verb_n <- function(verb, n = 3, wait = NULL) {
  function(...) {
    safe_verb <- purrr::safely(verb)
    for (i in seq_len(n)) {
      out <- safe_verb(...)
      if (!is.null(out$result) || i == n) break
      if (is.null(wait))
        wait <- stats::runif(1, min(5 ^ i, 120), min(5 ^ (i + 1), 180))
      mess <- paste("HTTP timeout or error on attempt %d.",
                    "Retrying in %0.0f s.")
      message(sprintf(mess, i, wait))
      Sys.sleep(wait)
    }
    out
  }
}

is_error <- function(obj) {
  if (inherits(obj, "list") ||
      identical(names(response), c("result", "error")))
    !is.null(obj$error)
  else inherits(obj, "error")
}

fws_url <- function() "https://ecos.fws.gov/ServCat/DownloadFile/126665"

gbif_count <- function(prop, ...) {
  n <- rgbif::occ_search(limit = 0, ...,
                         geometry = get_wkt(prop),
                         return = "meta")
  pull(n, count)
}

bison_count <- function(lat_range, lon_range) {
  con <- solrium::solr_connect("https://bison.usgs.gov/solr/occurrences/select/",
                               verbose = FALSE)
  q_recs <- solrium::solr_search(
    fq = list(paste0("decimalLatitude:[",
                     paste(lat_range, collapse = " TO "), "]"),
              paste0("decimalLongitude:[",
                     paste(lon_range, collapse = " TO "), "]")),
    rows = 1, parsetype = "list", callopts = httr::timeout(10))
  attr(q_recs, "numFound")
}

est_timeout <- function(n_recs) {
  ceiling(5 + 0.0027 * n_recs)
}

est_nrecs <- function(timeout) {
  recs <- round((timeout - 5) / 0.0027)
  100 * (recs %/% 100 + as.logical(recs %% 100))
}

get_ee_metadata <- function(url) {
  ee_meta <- jsonlite::fromJSON(url)
  ee_meta[["metadataurl"]]
}
