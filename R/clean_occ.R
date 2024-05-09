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
    dplyr::mutate(
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
  rn <- c("species", "decimalLatitude", "decimalLongitude", "catalogNumber", "coordinateUncertaintyInMeters")
  colnames(gbif_recs)[match(rn, colnames(gbif_recs))] <-
   c("sci_name", "lat", "lon", "cat_no", "loc_unc_m")

  standardize_occ(gbif_recs)

}


#' @noRd
clean_iDigBio <- function(idb_recs) {

  stopifnot(inherits(idb_recs, "data.frame"))

  idb_recs <- idb_recs %>%
    dplyr::mutate(sci_name = clean_sci_name(scientificname),
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

  vn_recs <- vn_recs %>%
    # `rvertnet` seems to return everything as a character; guess data types
    mutate_if(is.character, utils::type.convert, as.is = TRUE)
  #edit to ensure column vn_recs$associatedmedia is character
  if("associatedmedia" %in% colnames(vn_recs))
    vn_recs$associatedmedia<-as.character(vn_recs$associatedmedia)
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
    dplyr::mutate(genus = sapply(strsplit(scientificname, " "), function(i) i[1]),
           specificepithet = sapply(strsplit(scientificname, " "), function(i) i[2]))

  # Note that some viable link to original records, with media, lie buried
  # in the `references` column, but there's no easy way to get them...
  vn_recs <- vn_recs %>%
    dplyr::mutate(sci_name = clean_sci_name(scientificname),
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
    dplyr::mutate(inst_code = sub(":.*$", "", record),
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
    dplyr::mutate(meta_url = get_ee_metadata(source)) %>%
    ungroup()
  else ee_meta <- tibble(source = character(0), meta_url = character(0))

  # Add url and catalog number, if remote source is missing
  ee_recs <- ee_recs %>%
    left_join(ee_meta, by = "source") %>%
    dplyr::mutate(
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
    dplyr::mutate(sci_name = clean_sci_name(scientificName),
           year = strptime(dateCollectedStart, format = "%Y-%m-%d")$year + 1900,
           month = strptime(dateCollectedStart, format = "%Y-%m-%d")$mon + 1,
           day = strptime(dateCollectedStart, format = "%Y-%m-%d")$mday)

  # Rename relevant columns
  rn <- c("decimalLatitude", "decimalLongitude", "specimenCode")
  colnames(aw_recs)[match(rn, colnames(aw_recs))] <-
    c("lat", "lon", "cat_no")

  standardize_occ(aw_recs)

}

#' @noRd
#ServCat
clean_ServCat <- function(ServCat_rec, prop){
  try_JSON <- try_verb_n(jsonlite::fromJSON, 4)
  org_name <- prop
  short_org <- Cap(org_name) %>% shorten_orgnames()

  #extract unit codes of USFWS properties
  get_unit_codes <- function(orgnames = NULL) {
    base_url <- "https://ecos.fws.gov/primr/api/refuge/geo.json"

    try_JSON <- try_verb_n(jsonlite::fromJSON, 2)
    prop_info <- try_JSON(base_url)
    prop_info <- prop_info$features$properties %>%
      dplyr::mutate(org_name = toupper(.data$orgnameabbr)) %>%
      select(.data$org_name, UnitCode = .data$costCenterCode)
    if (is.null(orgnames)) return(prop_info)
    filter(prop_info,
           grepl(paste(orgnames, collapse = "|"), .data$org_name, ignore.case = TRUE))
  }
  #obtain unit code of refuge
  orgname_df <- get_unit_codes()

  if (length(orgname_df[orgname_df$org_name == prop$ORGNAME, ]$UnitCode) == 0){
    unit_code<- return (NULL)
  } else {
    unit_code <- orgname_df[orgname_df$org_name == prop$ORGNAME, ]$UnitCode
  }

  #create vector of other refuges to filter out ServCat records
  codes_to_remove <- orgname_df[orgname_df$org_name != prop$ORGNAME, ]$UnitCode

  #create column that will be used to identify
  #sources that are associated with organizations in
  #other regions or other Alaska refuges

  if (nrow(ServCat_rec) == 0) {
    return (NULL)
  } else {
    ServCat_rec$unit_code_only <- NA
  }

  #need to take only specific reference type
  reference_type_vec<-c("Book Chapter",
                        "Conference Proceeding",
                        "Conference Proceeding Paper",
                        "Geospatial Dataset",
                        "Journal Article",
                        "Published Report",
                        "Published Report Section",
                        "Published Report Series",
                        "Resource Brief",
                        "Tabular Dataset",
                        "Unpublished Report")


  if (nrow(ServCat_rec) == 0) {
    ServCat_rec_not_used_ref <- NULL
  } else {
    ServCat_rec_not_used_ref <- ServCat_rec[!ServCat_rec$referenceType %in% reference_type_vec, ]
  }

  #dataframe of sources not used because of reference type

  if (nrow(ServCat_rec) == 0) {
    return (NULL)
  } else {
    ServCat_rec <- ServCat_rec[ServCat_rec$referenceType %in% reference_type_vec, ]
  }

  if (nrow(ServCat_rec) == 0) {
    return (NULL)
  } else {
    for(i in 1:length(ServCat_rec$unit_code_only)){

      unitCode_df <- as.data.frame(try_JSON(rawToChar(httr::GET(paste0("https://ecos.fws.gov/ServCatServices/servcat/v4/rest/Reference/",
                                                                     as.character(ServCat_rec$referenceId[i]), "/Units"), httr::timeout(50000))$content)))

      ServCat_rec$unit_code_only[i] <- ifelse(sum(c((unitCode_df %>% subset(substr(unitCode_df$unitCode, 1, 4) != "FF07") %>% nrow == 0), (unitCode_df$unitCode %>% intersect(codes_to_remove) %>% length==0)))==2,T,F)

      rm(unitCode_df)
    }
  }

  #take only rows that are only associated with refuge of interest

  if (nrow(subset(ServCat_rec,ServCat_rec$unit_code_only == T)) == 0) {
    ServCat_rec<-NULL
  } else {
    ServCat_rec <- subset(ServCat_rec, ServCat_rec$unit_code_only == T)
  }

  #accept only records that have digital files available

  if (is.null(ServCat_rec)) {
    ServCat_rec <- NULL
  } else {
    if (nrow(ServCat_rec) == 0){
      ServCat_rec <- NULL
    }
    else{
      ServCat_rec$DigitalFiles <- NA
    }}


  if (is.null(ServCat_rec)) {
    ServCat_rec <- NULL
  } else {
    if (nrow(ServCat_rec) == 0){
      ServCat_rec <- NULL
    }
    else{
      for(i in 1:length(ServCat_rec$DigitalFiles)){
        ServCat_rec$DigitalFiles[i] <- ifelse(nrow(as.data.frame(try_JSON(rawToChar(httr::GET(paste0("https://ecos.fws.gov/ServCatServices/servcat/v4/rest/Reference/",
                                                                                                   as.character(ServCat_rec$referenceId[i]),"/DigitalFiles"),httr::timeout(50000))$content))))==0,F,T)
      }
    }}

  if (is.null(ServCat_rec)) {
    ServCat_rec <- NULL
  } else {
    if (nrow(ServCat_rec) == 0){
      ServCat_rec <- NULL
    }
    else{
      ServCat_rec <- subset(ServCat_rec, ServCat_rec$DigitalFiles == T)
    }}
  #select only records that have bounding box of refuge

  if (is.null(ServCat_rec)) {
    ServCat_rec <- NULL
  } else {
    if (nrow(ServCat_rec) == 0){
      ServCat_rec <- NULL
    }
    else{
      ServCat_rec$bounding_box <- NA
    }}


  if (is.null(ServCat_rec)) {
    ServCat_rec <- NULL
  } else {
    if (nrow(ServCat_rec) == 0){
      ServCat_rec <- NULL
    }
    else{
      for(i in 1:length(ServCat_rec$bounding_box)){
        bb_test_vec <- as.data.frame(try_JSON(rawToChar(httr::GET(paste0("https://ecos.fws.gov/ServCatServices/servcat/v4/rest/Reference/",
                                                                       as.character(ServCat_rec$referenceId[i]), "/BoundingBoxes"), httr::timeout(50000))$content)))$tag
        ServCat_rec$bounding_box[i] <- ifelse(length(bb_test_vec) == 1 &  paste0("BoundingBox for ", unit_code) %in% bb_test_vec, T, F)
        rm(bb_test_vec)
      }
    }}

  if (is.null(ServCat_rec)) {
    ServCat_rec <- NULL
  } else {
    if (nrow(ServCat_rec) == 0){
      ServCat_rec <- NULL
    }
    else{
      ServCat_rec <- subset(ServCat_rec, ServCat_rec$bounding_box == T)
    }}

  #extract taxa lists from each source

  taxa_list <- list()

  if (is.null(ServCat_rec)) {
    ServCat_rec <- NULL
  } else {
    if (nrow(ServCat_rec) == 0){
      ServCat_rec <- NULL
    }
    else{
      for(i in 1:length(ServCat_rec$referenceId)){

        taxa_list[[i]] <- ifelse(is.null(as.data.frame(try_JSON(rawToChar(httr::GET(paste0("https://ecos.fws.gov/ServCatServices/servcat/v4/rest/Reference/",
                                                                                         as.character(ServCat_rec$referenceId[i]), "/Taxa"), httr::timeout(50000))$content)))$taxonCode),
                               NA,
                               list(try_JSON(rawToChar(httr::GET(paste0("https://ecos.fws.gov/ServCatServices/servcat/v4/rest/Reference/",
                                                                        as.character(ServCat_rec$referenceId[i]), "/Taxa"), httr::timeout(50000))$content))))
      }
    }}



  #give names to list elements that match reference codes

  if (is.null(ServCat_rec)) {
    ServCat_rec <- NULL
  } else {
    if (nrow(ServCat_rec) == 0){
      ServCat_rec <- NULL
    } else{
      names(taxa_list) <- as.character(ServCat_rec$referenceId)

      taxa_list <- taxa_list[!is.na(taxa_list)]

      taxa_df_list <- lapply(taxa_list,as.data.frame)

      #this loop adds the reference ID to each dataframe in the loop
      for(i in 1:length(taxa_df_list)){
        taxa_df_list[[names(taxa_df_list)[i]]]$refID <- names(taxa_df_list)[i]
      }

      taxa_df_combined <- suppressMessages(Reduce(full_join, taxa_df_list))

      #now create the taxa_in_ServCat_rec with the taxon codes in ServCat
      taxa_in_ServCat_rec <- as.data.frame(taxa_df_combined$taxonCode)


      colnames(taxa_in_ServCat_rec) <- "Taxon_Code"

      taxa_in_ServCat_rec$ScientificName <- NA
      taxa_in_ServCat_rec$common_name <- NA


      for(i in 1:length(taxa_in_ServCat_rec$ScientificName)){
        taxa_in_ServCat_rec$ScientificName[i] <- as.data.frame(
          try_JSON(
            rawToChar(
              httr::GET(paste0("https://ecos.fws.gov/ServCatServices/v2/rest/taxonomy/searchByCodes/taxoncode?codes=",
                               as.character(taxa_in_ServCat_rec$Taxon_Code[i])), httr::timeout(50000))$content)))$ScientificName[1]
      }
      #
      for(i in 1:length(taxa_in_ServCat_rec$common_name)){
        taxa_in_ServCat_rec$common_name[i] <- as.data.frame(
          try_JSON(
            rawToChar(
              httr::GET(paste0("https://ecos.fws.gov/ServCatServices/v2/rest/taxonomy/searchByCodes/taxoncode?codes=",
                               as.character(taxa_in_ServCat_rec$Taxon_Code[i])), httr::timeout(50000))$content)))$CommonName[1]
      }
      taxa_in_ServCat_rec$evidence <- paste0("https://ecos.fws.gov/ServCat/Reference/Profile/", taxa_df_combined$refID)
      ServCat_clean <- as.data.frame(taxa_in_ServCat_rec$ScientificName)
      colnames(ServCat_clean) <- "sci_name"
      ServCat_clean$lon <- NA
      ServCat_clean$lat <- NA
      ServCat_clean$loc_unc_m <- NA
      ServCat_clean$cat_no <- NA
      ServCat_clean$year <- NA
      ServCat_clean$month <- NA
      ServCat_clean$day <- NA
      ServCat_clean$evidence <- taxa_in_ServCat_rec$evidence
      ServCat_clean$media_url <- NA
      ServCat_clean$bio_repo <- "ServCat"
      ServCat_clean <- as_tibble(ServCat_clean)

    }}

  if (exists("ServCat_clean")){
    output <- ServCat_clean
  } else {
    output <- ServCat_rec
  }
  output
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
    sub("_.+$", "", nm) == "ServCat" ~ "ServCat",
    TRUE ~ "AntWeb"
  )

  ## Ensure necessary columns are present and, if not, add them
  #Set column names/order of output data frame
  needed_nms <- c("sci_name", "lon", "lat", "loc_unc_m", "year",
                  "month", "day", "cat_no", "media_url", "evidence")
  needed_nms <- needed_nms[!needed_nms %in% names(clean_recs)]
  if (length(needed_nms)) {
    out_df <- utils::read.csv(text = paste(needed_nms, collapse = ", "))
    out_df <- bind_rows(clean_recs, out_df)
  } else out_df <- clean_recs

  # coord_tol not yet accessible to user
  if (!is.null(coord_tol))
    out_df <- filter(out_df, loc_unc_m <= coord_tol)

  # Final field standardizations / conversions
  out_df <- out_df %>%
    dplyr::mutate(sci_name = clean_sci_name(sci_name),
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

