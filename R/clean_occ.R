# Functions to wrangle returned occurrence records into a consistent structure

#' @noRd
clean_GBIF <- function(gbif_recs) {

  stopifnot(inherits(gbif_recs, "gbif"))

  # If necessary, associate media information into data frame
  media <- Filter(length, gbif_recs$media)
  if (length(media)) {
    keys <- vapply(media, function(i) i[[1]][["key"]], character(1))
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
        !is_missing(.data$bibliographicCitation) ~ .data$bibliographicCitation,
        !is_missing(.data$references) ~ .data$references,
        TRUE ~ paste0("www.gbif.org/dataset/", .data$datasetKey,
                      "; catalog# ", .data$catalogNumber)),
      media_url = ifelse(!is_missing(.data$media_url),
                         paste(.data$evidence, .data$media_url, sep = ", "),
                         .data$media_url))

  # Rename relevant columns
  rn <- c("name", "decimalLatitude", "decimalLongitude", "catalogNumber", "coordinateUncertaintyInMeters")
  colnames(gbif_recs)[match(rn, colnames(gbif_recs))] <-
    c("sci_name", "lat", "lon", "cat_no", "loc_unc_m")

  standardize_occ(gbif_recs)

}

#' @noRd
clean_BISON <- function(bison_recs) {

  stopifnot(inherits(bison_recs, "tbl_df"))

  # Add *missing* columns if necessary
  bison_recs <- bison_recs %>%
    bind_rows(data.frame(catalogNumber = character(0),
                         coordinateUncertaintyInMeters = character(0),
                         collectionID = character(0),
                         eventDate = character(0),
                         stringsAsFactors = FALSE)) %>%
    mutate(cat_no = ifelse(!is_missing(.data$catalogNumber),
                           .data$catalogNumber, NA_character_),
           month = strptime(.data$eventDate, format = "%Y-%m-%d")$mon + 1,
           day = strptime(.data$eventDate, format = "%Y-%m-%d")$mday,
           evidence = case_when(
             !is_missing(.data$collectionID) ~ paste0(sub("/$", "", .data$collectionID),
                                                      "; catalog# ", .data$cat_no),
             !is_missing(.data$institutionID) ~ paste0(sub("/$", "", .data$institutionID),
                                                       "; catalog# ", .data$cat_no),
             !is_missing(.data$ownerInstitutionCollectionCode) ~
               paste0(sub("/$", "", .data$ownerInstitutionCollectionCode),
                      "; catalog# ", .data$cat_no),
             TRUE ~ NA_character_),
           coordinateUncertaintyInMeters = as.double(coordinateUncertaintyInMeters))

  # Rename relevant columns
  rn <- c("scientificName", "decimalLatitude", "decimalLongitude",
          "ownerInstitutionCollectionCode", "coordinateUncertaintyInMeters")
  colnames(bison_recs)[match(rn, colnames(bison_recs))] <-
    c("sci_name", "lat", "lon", "inst_coll", "loc_unc_m")

  standardize_occ(bison_recs)

}

#' @noRd
clean_iDigBio <- function(idb_recs) {

  stopifnot(inherits(idb_recs, "data.frame"))

  idb_recs <- idb_recs %>%
    mutate(sci_name = clean_sci_name(.data$scientificname),
           year = strptime(.data$datecollected, format = "%Y-%m-%d")$year + 1900,
           month = strptime(.data$datecollected, format = "%Y-%m-%d")$mon + 1,
           day = strptime(.data$datecollected, format = "%Y-%m-%d")$mday,
           evidence = paste0("portal.idigbio.org/portal/records/", .data$uuid))

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
    mutate(genus = sapply(strsplit(.data$scientificname, " "), function(i) i[1]),
           specificepithet = sapply(strsplit(.data$scientificname, " "), function(i) i[2]))

  # Note that some viable link to original records, with media, lie buried
  # in the `references` column, but there's no easy way to get them...
  vn_recs <- vn_recs %>%
    mutate(sci_name = clean_sci_name(.data$scientificname),
           evidence = case_when(
             !is_missing(.data$references) ~ .data$references,
             !is_missing(.data$source_url) ~ paste0(.data$source_url, "; catalog# ",
                                                    .data$catalognumber),
             !is_missing(.data$bibliographiccitation) ~ .data$bibliographiccitation,
             TRUE ~ NA_character_),
           month = as.integer(month),
           month = case_when(
             (month < 13 & month > 0) ~ month,
             (month > 12 | month < 1)  ~ NA_integer_)
    )

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

  ee_recs <- ee_recs$data %>%
    # Extract institution code
    mutate(inst_code = sub(":.*$", "", .data$record),
           cat_no = gsub("[^:]+:", "", .data$record))

  # Rename relevant columns
  rn <- c("scientific_name", "latitude", "longitude",
          "coordinate_uncertainty_in_meters")
  colnames(ee_recs)[match(rn, colnames(ee_recs))] <-
    c("sci_name", "lat", "lon", "loc_unc_m")

  # Try to add missing evidence
  ee_meta <- filter(ee_recs, is_missing(.data$remote_resource)) %>%
    select(.data$source) %>% unique()

  if (nrow(ee_meta) > 0)
    ee_meta <- ee_meta %>%
    rowwise() %>%
    mutate(meta_url = get_ee_metadata(.data$source)) %>%
    ungroup()
  else ee_meta <- tibble(source = character(0), meta_url = character(0))

  ee_recs <- ee_recs %>%
    left_join(ee_meta, by = "source") %>%
    mutate(evidence = case_when(
      !is_missing(.data$remote_resource) ~ .data$remote_resource,
      !is_missing(.data$meta_url) ~ paste0(meta_url, "; catalog# ",
                                           .data$cat_no),
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
    mutate(sci_name = clean_sci_name(.data$scientific_name),
           year = strptime(.data$dateIdentified, format = "%Y-%m-%d")$year + 1900,
           month = strptime(.data$dateIdentified, format = "%Y-%m-%d")$mon + 1,
           day = strptime(.data$dateIdentified, format = "%Y-%m-%d")$mday)

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
  # out_df <- utils::read.csv(text = paste(c("sci_name", "lon", "lat", "loc_unc_m", "year",
  #                                          "month", "day", "cat_no", "media_url", "evidence"),
  #                                        collapse = ", "))

  out_df <- data.frame(sci_name = character(),
                       lon = numeric(),
                       lat = numeric(),
                       loc_unc_m = double(),
                       year = integer(),
                       month = integer(),
                       day = integer(),
                       cat_no = character(),
                       media_url = character(),
                       evidence = character())
  out_df <- bind_rows(out_df, clean_recs)

  # coord_tol not yet accessible to user
  if (!is.null(coord_tol))
    out_df <- filter(out_df, .data$loc_unc_m <= coord_tol)

  # Final field standardizations / conversions
  out_df <- out_df %>%
    mutate(sci_name = clean_sci_name(.data$sci_name),
           lon = as.numeric(.data$lon),
           lat = as.numeric(.data$lat),
           loc_unc_m = clean_loc_unc(.data$loc_unc_m),
           cat_no = tolower(.data$cat_no),
           year = as.integer(.data$year),
           # Try and salvage multiple possible entries for month, from integers to
           # various character incarnations (e.g., "02", "Jun", "August")
           month = suppressWarnings(as.integer(
             ifelse(nchar(.data$month) < 3, .data$month,
                    match(substr(tolower(.data$month), 1, 3), tolower(month.abb))))),
           day = as.integer(.data$day),
           evidence = gsub("; catalog# NA", "",
                           ifelse(!is_missing(.data$media_url), .data$media_url,
                                  .data$evidence)),
           bio_repo = src)

  # Thin fields
  out_df[, c("sci_name", "lon", "lat", "loc_unc_m", "cat_no", "year", "month",
             "day", "evidence", "media_url", "bio_repo")]

}

