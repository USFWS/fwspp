# Functions to extract occurrence records from each biodiversity database

# All get_* functions will return errors and continue rather than break
# the overall request.  This allows the request of occurrence records to
# to proceed to additional properties

#' @noRd
get_GBIF <- function(prop, timeout, limit = 200000) {

  message("Querying the Global Biodiversity Information Facility (GBIF)...")

  # We want to capture media, if available, so can't use 'spocc' call to GBIF
  try_gbif_count <- try_verb_n(gbif_count)
  try_gbif <- try_verb_n(rgbif::occ_search)

  curr_yr <- as.POSIXlt(Sys.time())$year + 1900

  # Hoop-jumping to retrieve more records, if necessary
  q_recs <- try_gbif_count(prop)
  if (q_recs == 0) {
    message("No records found.")
    return(NULL)
  }
  message("Retrieving ", q_recs, " records.")
  if (q_recs > 50000) {
    message("Splitting the GBIF query temporally to recover all records.")
    # Finding year breaks
    n_grp <- ceiling(q_recs/50000)
    yr_bnd_l <- integer(0)

    for (yr in curr_yr:1776) {
      cutoff <- 50000 * (length(yr_bnd_l) + 1)
      yr_rng <- paste(yr, curr_yr, sep = ",")
      n_recs <- try_gbif_count(prop,
                               year = yr_rng)

      ## TO DO: ADD MESSAGE IF SINGLE YEAR RECORDS EXCEED 200K

      if (n_recs > cutoff - 25000)
        yr_bnd_l <- c(yr_bnd_l, yr)
      if (length(yr_bnd_l) == (n_grp - 1)) break
    }

    yr_bnd_h <- c(rev(yr_bnd_l - 1), curr_yr)
    yr_bnd_l <- c(1776, rev(yr_bnd_l))

    # Set up receptacle for actual data queries
    gbif_recs <- list(media = list(),
                      data = tibble(),
                      meta = list())

    for (i in seq_along(yr_bnd_l)) {
      yr_rng <- paste(yr_bnd_l[i], yr_bnd_h[i], sep = ",")
      message("  Processing occurrence records from ", sub(",", " - ", yr_rng))
      tmp <- try_gbif(limit = limit, year = yr_rng,
                      geometry = get_wkt(prop),
                      curlopts = list(timeout = timeout))
      gbif_recs$media <- c(gbif_recs$media, tmp$media)
      gbif_recs$data <- bind_rows(gbif_recs$data, tmp$data)
    }
  } else {
    gbif_recs <- try_gbif(limit = min(limit, q_recs),
                          geometry = get_wkt(prop),
                          curlopts = list(timeout = timeout))
  }
  gbif_recs$meta$count <- q_recs
  class(gbif_recs) <- "gbif"
  gbif_recs
}

#' @noRd
get_iDigBio <- function(lat_range, lon_range, timeout) {

  message("Querying Integrated Digitized Biocollections (iDigBio)...")

  rq <- list(geopoint = list(type = "geo_bounding_box",
                             top_left = list(
                               lat = lat_range[2], lon = lon_range[1]),
                             bottom_right = list(
                               lat = lat_range[1], lon = lon_range[2])))

  try_idb_count <- try_verb_n(ridigbio::idig_count_records)
  try_idb <- try_verb_n(ridigbio::idig_search)
  q_recs <- try_idb_count(rq = rq)
  if (q_recs == 0) {
    message("No records found.")
    return(NULL)
  }
  message("Retrieving ", q_recs, " records.")
  idb_recs <- try_idb(type = "records", mq = FALSE, rq = rq, fields = "all",
                      max_items = 100000, limit = 0, offset = 0, sort = FALSE,
                      httr::config(timeout = timeout))
  idb_recs
}

#' @noRd
get_VertNet <- function(center, radius, timeout, limit = 10000, prop) {

  message("Querying VertNet...")
  try_vertnet_count <- try_verb_n(vertnet_count)
  # VertNet balks on large requests sometimes, returning no matches for a good query
  # Try up to three times to be sure...
  i <- 1
  q_recs <- 0
  while (i <= 3 | q_recs == 0) {
    q_recs <- try_vertnet_count(center, radius)
    i <- i + 1
  }
  if (q_recs == 0) {
    message("No records found.")
    return(NULL)
  }
  message("Retrieving ", q_recs, " records.")

  # `spocc` doesn't currently allow geographic searches with VertNet while `rvertnet` does
  try_vn <- try_verb_n(rvertnet::spatialsearch)

  if (q_recs < 5000) {
    vn_recs <- try_vn(center[2], center[1], radius, limit, messages = FALSE,
                      callopts = list(timeout = timeout),
                      only_dwc = FALSE)
    if (!is.null(vn_recs)) vn_recs <- vn_recs$data
  } else {
    # Chop it up
    diced <- dice_prop(prop)
    vn_recs <- pbapply::pblapply(seq_len(nrow(diced)), function(d) {
      tmp <- diced[d, ]
      tmp_bb <- matrix(sf::st_bbox(tmp), 2)
      tmp_cent <- rowMeans(tmp_bb)
      tmp_radius <- geosphere::distVincentyEllipsoid(tmp_cent, t(tmp_bb))
      tmp_radius <- ceiling(max(tmp_radius))
      tmp_recs <- try_vn(tmp_cent[2], tmp_cent[1], tmp_radius, messages = FALSE,
                         callopts = list(timeout = timeout),
                         only_dwc = FALSE)
      if (!is.null(tmp_recs)) tmp_recs <- tmp_recs$data
      tmp_recs
    })
    vn_recs <- bind_rows(vn_recs) %>% distinct()
  }
  vn_recs
}


#' @noRd
get_EcoEngine <- function(lat_range, lon_range, timeout) {

  message("Querying the Berkeley Ecoinformatics Engine...")

  # Could use `spocc` but why start now...
  bbox <- paste(c(lon_range[1], lat_range[1],
                  lon_range[2], lat_range[2]),
                collapse = ",")

  # EcoEngine 'errors' when no results so approach slightly differently
  safe_ee <-purrr::safely(ecoengine::ee_observations)
  for (i in 1:3) {
    suppressMessages(
      ee_recs <- safe_ee(page_size = 10000, bbox = bbox,
                         georeferenced = TRUE, quiet = TRUE,
                         foptions = httr::timeout(timeout))
    )
    if (!is_error(ee_recs) || i == 3) break
    if (grepl("count not greater than 0", ee_recs$error$message)) {
      message("No records found.")
      return(NULL)
    }
    wait <- stats::runif(1, min(5 ^ i, 120), min(5 ^ (i + 1), 180))
    mess <- paste("HTTP timeout or error on attempt %d.",
                  "Retrying in %0.0f s.")
    message(sprintf(mess, i, wait))
    Sys.sleep(wait)
  }
  if (is_error(ee_recs))
    stop(ee_recs$error$message)
  message("Retrieving ", ee_recs$result$results, " records.")
  ee_recs$result
}

#' @noRd
get_AntWeb <- function(lat_range, lon_range, timeout) {

  message("Querying AntWeb...")

  # The `spocc` package has a relevant non-exported function (spocc:::aw_data2)
  # that can query AntWeb with geometry, but it tosses any links to documenting images
  # This function borrows (steals) heavily from that function but saves the links
  bbox <- paste(c(lat_range[2], lon_range[2], lat_range[1], lon_range[1]), collapse = ",")
  base_url <- "https://antweb.org/v3.1/specimens"
  try_GET <- try_verb_n(httr::GET)
  res <- try_GET(base_url, query = list(bbox = bbox, up = 1), httr::timeout(timeout))
  res <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), FALSE)  # problem lies here...

  if (res$metaData$count == 0) {
    message("No records found.")
    return(NULL)
  }
  if (res$metaData$count > 10000) message("Only first 10000 matching AntWeb records returned.")
  message("Retrieving ", min(res$metaData$count, 10000), " records.")

  aw_recs <- lapply(res$specimens, function(x) {
    has_image <- "images" %in% names(x)
    if (has_image) x$images <- NULL
    aw_recs <- data.frame(t(unlist(x)), stringsAsFactors=FALSE)
    aw_recs$evidence <- paste0("https://www.antweb.org/specimen/", aw_recs$catalogNumber)
    aw_recs
  })
  bind_rows(aw_recs)
}
