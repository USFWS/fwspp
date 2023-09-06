################################################################################
# Helper to run function up a function a user-specified number of times with increasing
# backoff times (modified from `VERB_n` in googlesheets package); errors (after n
# attempts) are kept and passed along.  Currently retrying on all errors until timeouts,
# brief server errors, and non-negotiable errors can be better differentiated. This
# function is also used to capture errors for non-HTTP related functions (e.g., cleaning
# and scrubbing)
################################################################################

#' @noRd
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


#' @noRd
is_error <- function(obj) {
  if (inherits(obj, "list") &&
      identical(names(obj), c("result", "error")))
    !is.null(obj$error)
  else inherits(obj, "error") | inherits(obj, "try-error")
}


#' @noRd
fws_url <- function() "https://ecos.fws.gov/ServCat/DownloadFile/126665"


#' @noRd
gbif_count <- function(prop, ...) {
  n <- rgbif::occ_search(limit = 0, ...,
                         geometry = get_wkt(prop))
  n$meta$count
}


# bison_count <- function(prop) {
#   prop_bb <- matrix(sf::st_bbox(prop), 2)
#   aoi_bb <- as.vector(prop_bb + matrix(rep(c(-0.00006, 0.00006), 2), nrow = 2, byrow = TRUE)) %>%
#     paste(collapse = ",")
#   url <- paste0("https://bison.usgs.gov/api/search.json?count=1&aoibbox=", aoi_bb)
#   try_JSON <- try_verb_n(jsonlite::fromJSON, 2)
#   prop_info <- try_JSON(url)
#   prop_info$total
# }


#' @noRd
vertnet_count <- function(center, radius) {
  args <- list(lat = center[2], long = center[1], radius = radius)
  cli <- crul::HttpClient$new(url = "http://api.vertnet-portal.appspot.com", opts = list())
  tt <- cli$get("api/search",
                query = list(q = rvertnet:::make_q("spatialsearch", args, limit = 0)))
  tt$raise_for_status()
  txt <- tt$parse("UTF-8")
  out <- jsonlite::fromJSON(txt)
  avail <- out$matching_records
  as.numeric(regmatches(avail, regexpr("[0-9]+", avail)))
}


#' @noRd
est_timeout <- function(n_recs) {
  ceiling(0.005 * n_recs)
}


#' @noRd
est_nrecs <- function(timeout) {
  recs <- round(timeout / 0.005)
  100 * (recs %/% 100 + as.logical(recs %% 100))
}


#' @noRd
get_ee_metadata <- function(url) {
  ee_meta <- jsonlite::fromJSON(url)
  ee_meta[["metadataurl"]]
}


#' @noRd
get_unit_codes <- function(orgnames = NULL) {
  base_url <- "https://ecos.fws.gov/primr/api/refuge/geo.json"
  try_JSON <- try_verb_n(jsonlite::fromJSON, 2)
  prop_info <- try_JSON(base_url)
  prop_info <- prop_info$features$properties %>%
    mutate(org_name = toupper(.data$orgnameabbr)) %>%
    select(.data$org_name, UnitCode = .data$costCenterCode)
  if (is.null(orgnames)) return(prop_info)
  filter(prop_info,
         grepl(paste(orgnames, collapse = "|"),
               .data$org_name,
               ignore.case = TRUE))
}
