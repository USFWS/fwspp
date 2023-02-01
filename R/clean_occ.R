# Functions to wrangle returned occurrence records into a consistent structure

#' @noRd
clean_GBIF <- function(gbif_recs) {

  stopifnot(inherits(gbif_recs, "gbif"))

  # If necessary, associate media information into data frame
  media <- Filter(length, gbif_recs$media)
  if (length(media)) {
    keys <- vapply(media, function(i) i[[1]][["key"]], integer(1))
    urls <- sapply(media, function(i) {
      id <- i[[1]][[1]][["identifier"]]
      id <- ifelse(is.null(id), NA_character_, id)
    })
    media <- data.frame(key = keys, media_url = urls, stringsAsFactors = FALSE)
    gbif_recs <- left_join(gbif_recs$data, media, by = "key")
  } else gbif_recs <- gbif_recs$data
  gbif_recs <- gbif_recs %>%
    # Add *missing* columns if necessary
    bind_rows(data.frame(media_url = character(0),
                         catalogNumber = character(0),
                         institutionCode = character(0),
                         bibliographicCitation = character(0),
                         references = character(0),
                         coordinateUncertaintyInMeters = integer(0),
                         stringsAsFactors = FALSE))

  gbif_recs <- gbif_recs %>%
    mutate(
      evidence = case_when(
        #!is_missing(bibliographicCitation) ~ bibliographicCitation,
        !is_missing(references) ~ references,
        #!is_missing(occurrenceID) ~ occurrenceID,
        TRUE ~ paste0("https://www.gbif.org/occurrence/", gbifID)),
        # TRUE ~ paste0("www.gbif.org/dataset/", datasetKey,
                      # "; catalog# ", catalogNumber)),
      media_url = ifelse(!is_missing(media_url),
                         paste(evidence, media_url, sep = ", "),
                         media_url))

  # Rename relevant columns
  rn <- c("name", "decimalLatitude", "decimalLongitude", "catalogNumber", "coordinateUncertaintyInMeters")
  colnames(gbif_recs)[match(rn, colnames(gbif_recs))] <-
    c("sci_name", "lat", "lon", "cat_no", "loc_unc_m")

  standardize_occ(gbif_recs)

}

#' @noRd
clean_iDigBio <- function(idb_recs) {

  stopifnot(inherits(idb_recs, "data.frame"))

  idb_recs <- idb_recs %>%
    mutate(sci_name = clean_sci_name(scientificname),
           year = strptime(datecollected, format = "%Y-%m-%d")$year + 1900,
           month = strptime(datecollected, format = "%Y-%m-%d")$mon + 1,
           day = strptime(datecollected, format = "%Y-%m-%d")$mday,
           evidence = paste0("https://portal.idigbio.org/portal/records/", uuid))

  # Rename relevant columns
  rn <- c("geopoint.lat", "geopoint.lon", "catalognumber", "coordinateuncertainty")
  colnames(idb_recs)[match(rn, colnames(idb_recs))] <-
    c("lat", "lon", "cat_no", "loc_unc_m")

  standardize_occ(idb_recs)

}

#' @noRd
clean_VertNet <- function(vn_recs) {

  stopifnot(inherits(vn_recs, "list"))

  vn_recs <- vn_recs$data %>%
    # `rvertnet` seems to return everything as a character; guess data types
    mutate_if(is.character, utils::type.convert, as.is = TRUE)

  # Add *missing* columns if necessary
  vn_recs <- vn_recs %>%
    bind_rows(data.frame(coordinateuncertaintyinmeters = integer(0),
                         associatedmedia = character(0),
                         institutioncode = character(0),
                         source_url = character(0),
                         bibliographiccitation = character(0),
                         stringsAsFactors = FALSE))

  # Create genus or species if they do not exist
  if (!(any("genus" %in% colnames(vn_recs), "specificepithet" %in% colnames(vn_recs))))
    vn_recs <- vn_recs %>%
    mutate(genus = sapply(strsplit(scientificname, " "), function(i) i[1]),
           specificepithet = sapply(strsplit(scientificname, " "), function(i) i[2]))

  # Note that some viable link to original records, with media, lie buried
  # in the `references` column, but there's no easy way to get them...
  vn_recs <- vn_recs %>%
    mutate(sci_name = clean_sci_name(scientificname),
           evidence = case_when(
             !is_missing(references) ~ references,
             !is_missing(source_url) ~ paste0(source_url, "; catalog# ",
                                                    catalognumber),
             !is_missing(bibliographiccitation) ~ bibliographiccitation,
             TRUE ~ NA_character_),
           month = suppressWarnings(as.integer(month)),
           month = ifelse(between(month, 1, 12), month, NA_integer_))

  # Rename relevant columns
  rn <- c("decimallatitude", "decimallongitude", "catalognumber",
          "coordinateuncertaintyinmeters")
  colnames(vn_recs)[match(rn, colnames(vn_recs))] <-
    c("lat", "lon", "cat_no", "loc_unc_m")

  standardize_occ(vn_recs)

}

#' @noRd
clean_EcoEngine <- function(ee_recs) {

  stopifnot(inherits(ee_recs, "ecoengine"))

  # Extract institution code
  ee_recs <- ee_recs$data %>%
    mutate(inst_code = sub(":.*$", "", record),
           cat_no = gsub("[^:]+:", "", record))

  # Rename relevant columns
  rn <- c("scientific_name", "latitude", "longitude",
          "coordinate_uncertainty_in_meters")
  colnames(ee_recs)[match(rn, colnames(ee_recs))] <-
    c("sci_name", "lat", "lon", "loc_unc_m")

  # Try to add missing evidence
  ee_meta <- filter(ee_recs, is_missing(remote_resource)) %>%
    select(source) %>% unique()

  # Get metadata url, if present
  if (nrow(ee_meta) > 0)
    ee_meta <- ee_meta %>%
    rowwise() %>%
    mutate(meta_url = get_ee_metadata(source)) %>%
    ungroup()
  else ee_meta <- tibble(source = character(0), meta_url = character(0))

  # Add url and catalog number, if remote source is missing
  ee_recs <- ee_recs %>%
    left_join(ee_meta, by = "source") %>%
    mutate(
      evidence = case_when(
        !is_missing(remote_resource) ~ remote_resource,
        !is_missing(meta_url) ~ paste0(meta_url, "; catalog# ",
                                             cat_no),
        TRUE ~ NA_character_),
      loc_unc_m = as.double(loc_unc_m),
      year = lubridate::year(begin_date),
      month = lubridate::month(begin_date),
      day = lubridate::day(begin_date))

  standardize_occ(ee_recs)

}

#' @noRd
clean_AntWeb <- function(aw_recs) {

  if (!is.data.frame(aw_recs))
    stop("Expected data.frame from AntWeb query.")

  aw_recs <- aw_recs %>%
    mutate(sci_name = clean_sci_name(scientificName),
           year = strptime(dateCollectedStart, format = "%Y-%m-%d")$year + 1900,
           month = strptime(dateCollectedStart, format = "%Y-%m-%d")$mon + 1,
           day = strptime(dateCollectedStart, format = "%Y-%m-%d")$mday)

  # Rename relevant columns
  rn <- c("geojson.coord1", "geojson.coord2", "catalogNumber")
  colnames(aw_recs)[match(rn, colnames(aw_recs))] <-
    c("lat", "lon", "cat_no")

  standardize_occ(aw_recs)

}

#' @noRd
standardize_occ <- function(clean_recs, coord_tol = NULL) {

  if (is.null(clean_recs)) return(NULL)

  nm <- deparse(substitute(clean_recs))
  src <- case_when(
    sub("_.+$", "", nm) == "gbif" ~ "GBIF",
    sub("_.+$", "", nm) == "bison" ~ "BISON",
    sub("_.+$", "", nm) == "idb" ~ "iDigBio",
    sub("_.+$", "", nm) == "vn" ~ "VertNet",
    sub("_.+$", "", nm) == "ee" ~ "EcoEngine",
    TRUE ~ "AntWeb"
  )

  ## Set column names/order of output data frame
  out_df <- utils::read.csv(text = paste(c("sci_name", "lon", "lat", "loc_unc_m", "year",
                                           "month", "day", "cat_no", "media_url", "evidence"),
                                         collapse = ", "))
  out_df <- bind_rows(out_df, clean_recs)

  # coord_tol not yet accessible to user
  if (!is.null(coord_tol))
    out_df <- filter(out_df, loc_unc_m <= coord_tol)

  # Final field standardizations / conversions
  out_df <- out_df %>%
    mutate(sci_name = clean_sci_name(sci_name),
           lon = as.numeric(lon),
           lat = as.numeric(lat),
           loc_unc_m = clean_loc_unc(loc_unc_m),
           cat_no = tolower(cat_no),
           year = as.integer(year),
           # Try and salvage multiple possible entries for month, from integers to
           # various character incarnations (e.g., "02", "Jun", "August")
           month = suppressWarnings(as.integer(
             ifelse(nchar(month) < 3, month,
                    match(substr(tolower(month), 1, 3), tolower(month.abb))))),
           day = as.integer(day),
           evidence = gsub("; catalog# NA", "",
                           ifelse(!is_missing(media_url), media_url,
                                  evidence)),
           bio_repo = src,
           media_url = as.character(media_url))

  # Thin fields
  out_df[, c("sci_name", "lon", "lat", "loc_unc_m", "cat_no", "year", "month",
             "day", "evidence", "media_url", "bio_repo")]

}

