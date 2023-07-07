xlsx_review <- function(org, fwspp, overwrite, verbose, out_dir) {
  dat <- fwspp[[org]]
  if (is.null(dat)) {
    if (verbose) cat("No records for", paste0(org, ". Skipping...\n"))
    return()
  }
  org_dat <- dat %>%
    mutate(occurrence = "Probably present",
           nativeness = NA_character_,
           accept_record = ifelse(is.na(note) | grepl("FWSpecies", note), #changed NPS to FWS
                                  "Yes", "No")) %>%
    select(org_name, category, taxon_code,
           sci_name, com_name,
           occurrence:accept_record,
           evidence, note) %>%
    arrange(category, sci_name)
  org_dat$org_name<-org

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Species List for Review")
  col_widths <- c(10, 15, 13, 29, 29, 16, 12, 9, 70, 40)
  openxlsx::setColWidths(wb, 1, cols = seq_along(org_dat), widths = col_widths)
  openxlsx::freezePane(wb, 1, firstRow = TRUE)

  # Add tags and data validation
  xlsx_review_tags(wb)
  add_review_validation(wb, end_row = nrow(org_dat) + 1)

  # Write and save it
  writeData(wb, 1, org_dat, withFilter = TRUE)
  fn <- file.path(out_dir, xlsx_fn(org))
  if (file.exists(fn) && !overwrite) {
    warning("File exists and overwrite = FALSE. Skipping ", org, call. = FALSE)
  } else {
    saveWorkbook(wb, fn, overwrite = overwrite)
    if (verbose) cat("Exported", paste0(org, "\n"))
  }
  return()
}

#xlsx_submission <- function(org, occ_data, out_dir, overwrite, verbose) {
#  org_dat <- filter(occ_data, ORGNAME == org) %>%
#    select(-ORGNAME)
#
#  wb <- openxlsx::createWorkbook()
#  openxlsx::addWorksheet(wb, "Species List for Import")
#  openxlsx::setColWidths(wb, 1, cols = seq_along(org_dat), widths = "auto")
#  openxlsx::freezePane(wb, 1, firstRow = TRUE)
#
# Write and save it
#  writeData(wb, 1, org_dat, withFilter = TRUE)
#  fn <- file.path(out_dir, xlsx_fn(org))
#  if (file.exists(fn) && !overwrite) {
#    warning("File exists and overwrite = FALSE. Skipping ", org, call. = FALSE)
#  } else {
#    saveWorkbook(wb, fn, overwrite = overwrite)
#    if (verbose) cat("Exported", paste0(org, "\n"))
#  }
#  return()
#}

xlsx_submission <- function(org, occ_data, out_dir, overwrite, verbose) {
  org_dat <- filter(occ_data, ORGNAME == org) %>%
    select(-ORGNAME)

  test_df<-unique(org_dat[,c(1,2,3)])



  #aggregate by common name so each taxa includes all common names from the different data sources
  CommonNames_vec<-sapply(unique(org_dat[,c(1,2,3)])$TaxonCode,
                          function(x){paste(unique(subset(org_dat,
                                                          org_dat$TaxonCode==x)$CommonNames),collapse=', ')})

  CommonNames_list<-sapply(test_df$TaxonCode,
                           function(x){CommonNames_vec[x]}) %>% as.vector %>% strsplit(",") %>% lapply(trimws) %>% lapply(unique)

  CommonNames_vec_update<-sapply(CommonNames_list,function(x){paste(x,collapse=", ")})

  CommonNames_vec_update[CommonNames_vec_update=="NA"]<-NA

  test_df$CommonNames<-CommonNames_vec_update
  test_df$`Refuge Synonyms`<-NA

  evidence_1<-rep(NA,nrow(org_dat))
  unique_taxa_in_org_dat_list_links<-list()

  for(i in 1:length(unique(org_dat$TaxonCode))){
    unique_taxa_in_org_dat_list_links[[i]]<-subset(org_dat,
                                                   org_dat$TaxonCode==unique(org_dat$TaxonCode)[i])$ExternalLinks

  }

  unique_taxa_in_org_dat_list_links<-lapply(unique_taxa_in_org_dat_list_links, function(x){gsub(" ", "",unlist(strsplit(x,", ")))})
  names(unique_taxa_in_org_dat_list_links)<-unique(org_dat$TaxonCode)

  for(i in 1:length(evidence_1)){
    org_dat$ExternalLinks[i]<-gsub(" ", "",unlist(strsplit(org_dat$ExternalLinks[i],", ")))[1]
  }

  unique_taxa_in_org_dat_list<-list()

  for(i in 1:length(unique(org_dat$TaxonCode))){
    unique_taxa_in_org_dat_list[[i]]<-subset(org_dat,
                                             org_dat$TaxonCode==unique(org_dat$TaxonCode)[i])

  }

  test_df$ExternalLinks<-sapply(test_df$TaxonCode,function(x){unique_taxa_in_org_dat_list_links[x][1]}) %>%
    sapply(function(x){gsub(" ", "",unlist(strsplit(x,", ")))[1]})

  test_df$Occurrence<-"Unconfirmed"
  test_df$Seasonality<-NA
  test_df$Origin<-NA
  test_df$Management<-NA
  test_df$Abundance<-NA
  test_df$AbundanceNotes<-NA
  test_df$SpringAbundance<-NA
  test_df$SummerAbundance<-NA
  test_df$FallAbundance<-NA
  test_df$WinterAbundance<-NA
  test_df$RecordStatus<-"Draft"
  test_df$RefugeAccepted<-NA
  test_df$Sensitive<-NA
  test_df$SensitiveNotes<-NA
  test_df$HistoryNotes<-NA
  test_df$Email<-NA

  org_dat<-test_df

  unique_taxa_in_org_dat_list_links_extra<-unique_taxa_in_org_dat_list_links[which(lapply(unique_taxa_in_org_dat_list_links, length)>1)]
  unique_taxa_in_org_dat_list_links_extra<-lapply(names(unique_taxa_in_org_dat_list_links_extra),
                                                  function(x){unlist(unique_taxa_in_org_dat_list_links_extra[x])[-1]})
  extra_links_df_names<-names(unique_taxa_in_org_dat_list_links[which(lapply(unique_taxa_in_org_dat_list_links, length)>1)])
  names(unique_taxa_in_org_dat_list_links_extra)<-extra_links_df_names

  extra_links_df<-as.data.frame(stack(unique_taxa_in_org_dat_list_links_extra))

  colnames(extra_links_df)<-c("Link","TaxonCode")

  ExternalLinks_df<-as.data.frame(rep(unique(org_dat$UnitCode)[1],nrow(extra_links_df)))
  colnames(ExternalLinks_df)<-"UnitCode"
  ExternalLinks_df$TaxonCode<-extra_links_df$TaxonCode
  ExternalLinks_df$ExternalLinks<-extra_links_df$Link
  ExternalLinks_df$dateURLVerified<-NA
  ExternalLinks_df$evidenceType<-NA
  ExternalLinks_df$comments<-NA

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "SpeciesListForImport")
  #added test workbook####
  openxlsx::addWorksheet(wb, "ExternalLinks")
  openxlsx::addWorksheet(wb, "Drop-down values")
  openxlsx::addWorksheet(wb, "FWSpecies")
  openxlsx::setColWidths(wb, 1, cols = seq_along(org_dat), widths = "auto")
  openxlsx::freezePane(wb, 1, firstRow = TRUE)

  refuge_code<-unique(org_dat$UnitCode)[1]
  try_JSON <- try_verb_n(jsonlite::fromJSON, 4)

  FWSpecies_df <-as.data.frame(
    try_JSON(
      rawToChar(
        httr::GET(
          paste0("https://ecos.fws.gov/IRISAPI/SpeciesAPI/API/SpeciesList/items?RefugeCode=",refuge_code,"&RowsPerPage=10000"),httr::timeout(50000))$content)))

  taxoncode_vec<-rep(NA,length(FWSpecies_df$scientificName))
  for(i in 1:length(FWSpecies_df$scientificName)){

    test<-as.data.frame(
      try_JSON(
        rawToChar(
          httr::GET(
            paste0("https://ecos.fws.gov/ServCatServices/v2/rest/taxonomy/searchByScientificName/",
                   FWSpecies_df$scientificName[i] %>% stringr::str_extract( "[^ ]+ [^ ]+") %>% stringr::str_replace( " ", "%20")),httr::timeout(50000))$content)))
    taxoncode_vec[i]<-ifelse(nrow(subset(test, toupper(test$ScientificName)==toupper(FWSpecies_df$scientificName[i])))==0,
                             "<null>",max(subset(test, toupper(test$ScientificName)==toupper(FWSpecies_df$scientificName[i]))$TaxonCode))
    rm(test)
  }

  taxoncode_vec<-taxoncode_vec[taxoncode_vec!="<null>"]

  data_in_FWSpecies<-org_dat[as.character(org_dat$TaxonCode) %in% as.character(taxoncode_vec),]
  links_in_FWSpecies<-ExternalLinks_df[as.character(ExternalLinks_df$TaxonCode) %in% as.character(taxoncode_vec),]
  data_in_FWSpecies<-rbind(data_in_FWSpecies[,c(2,3,6)],links_in_FWSpecies[,c(1,2,3)])
  data_in_FWSpecies<-data_in_FWSpecies[order(data_in_FWSpecies$TaxonCode),]


  org_dat<-org_dat[!as.character(org_dat$TaxonCode) %in% as.character(taxoncode_vec),]
  openxlsx::writeData(wb, sheet = "FWSpecies",
            x = data_in_FWSpecies,
            startCol = 1)

  # Write and save it
  #writeData(wb, 1, org_dat, withFilter = TRUE)
  openxlsx::writeData(wb, sheet = "SpeciesListForImport",
            x = org_dat,
            startCol = 1,withFilter = TRUE)
  #add the extra tabs here
  Occurrence_values_df <- data.frame("Occurrence" = c("Present",
                                                      "Present-Adjacent",
                                                      "Probably Present",
                                                      "Probably Present-Adjacent",
                                                      "Probably Present-Historical",
                                                      "Unconfirmed",
                                                      "Unconfirmed-Adjacent",
                                                      "Unconfirmed-False Report",
                                                      "Unconfirmed-Historical",
                                                      "Not Present In Refuge",
                                                      "Not Present In Refuge-Adjacent",
                                                      "Not Present In Refuge-False Report",
                                                      "Not Present In Refuge-Historical"))
  Seasonality_values_df <- data.frame("Seasonality" = c("Breeding Season",
                                                        "Migratory",
                                                        "Non-breeding Season",
                                                        "Resident",
                                                        "Seasonal Occurrence Uncertain"))
  Origin_values_df <- data.frame("Origin" = c("Native",
                                              "Native-Cultivated",
                                              "Native-Restoration",
                                              "Native-Reintroduced",
                                              "NonNative",
                                              "NonNative-Cultivated",
                                              "NonNative-Invasive",
                                              "NonNative-Noxious",
                                              "NonNative-Introduced",
                                              "Vagrant",
                                              "Vagrant-Invasive",
                                              "Vagrant-Noxious",
                                              "Vagrant-Introduced",
                                              "Origin Uncertain",
                                              "Origin Uncertain-Cultivated",
                                              "Origin Uncertain-Noxious"))
  Management_values_df <- data.frame("Management" = c("Exploitation Concern",
                                                      "Management Priority",
                                                      "Resource of Concern",
                                                      "Priority Resource of Concern"))
  Abundance_values_df <- data.frame("Abundance" = c("Abundant",
                                                    "Common",
                                                    "Uncommon",
                                                    "Occasional",
                                                    "Rare",
                                                    "Unknown"))
  SpringAbundance_values_df <- data.frame("SpringAbundance" = c("Abundant",
                                                                "Common",
                                                                "Uncommon",
                                                                "Occasional",
                                                                "Rare",
                                                                "None",
                                                                "Unknown"))
  SummerAbundance_values_df <- data.frame("SummerAbundance" = c("Abundant",
                                                                "Common",
                                                                "Uncommon",
                                                                "Occasional",
                                                                "Rare",
                                                                "None",
                                                                "Unknown"))
  FallAbundance_values_df <- data.frame("FallAbundance" = c("Abundant",
                                                            "Common",
                                                            "Uncommon",
                                                            "Occasional",
                                                            "Rare",
                                                            "None",
                                                            "Unknown"))
  WinterAbundance_values_df <- data.frame("WinterAbundance" = c("Abundant",
                                                                "Common",
                                                                "Uncommon",
                                                                "Occasional",
                                                                "Rare",
                                                                "None",
                                                                "Unknown"))
  RecordStatus_values_df <- data.frame("RecordStatus" = c("Draft",
                                                          "InReview",
                                                          "Approved"))
  RefugeAccepted_values_df <- data.frame("RefugeAccepted" = c("Yes",
                                                              "No"))
  Sensitive_values_df <- data.frame("Sensitive" = c("Yes",
                                                    "No"))
  openxlsx::writeData(wb, sheet = "Drop-down values", x = Occurrence_values_df, startCol =
              1)
  openxlsx::writeData(wb, sheet = "Drop-down values", x = Seasonality_values_df, startCol =
              2)
  openxlsx::writeData(wb, sheet = "Drop-down values", x = Origin_values_df, startCol =
              3)

  openxlsx::writeData(wb, sheet = "Drop-down values", x = Management_values_df, startCol =
              4)
  openxlsx::writeData(wb, sheet = "Drop-down values", x = Abundance_values_df, startCol =
              5)

  openxlsx::writeData(wb, sheet = "Drop-down values", x = SpringAbundance_values_df, startCol =
              6)
  openxlsx::writeData(wb, sheet = "Drop-down values", x = SummerAbundance_values_df, startCol =
              7)

  openxlsx::writeData(wb, sheet = "Drop-down values", x = FallAbundance_values_df, startCol =
              8)
  openxlsx::writeData(wb, sheet = "Drop-down values", x = WinterAbundance_values_df, startCol =
              9)
  openxlsx::writeData(wb, sheet = "Drop-down values", x = RecordStatus_values_df, startCol =
              10)

  openxlsx::writeData(wb, sheet = "Drop-down values", x = RefugeAccepted_values_df, startCol =
              11)
  openxlsx::writeData(wb, sheet = "Drop-down values", x = Sensitive_values_df, startCol =
              12)
  #add dropdown values
  openxlsx::writeData(wb, sheet = "ExternalLinks", x = ExternalLinks_df, startCol =
              1)
  suppressWarnings({
  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 7, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$A$2:$A$",as.character(nrow(Occurrence_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 8, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$B$2:$B$",as.character(nrow(Seasonality_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 9, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$C$2:$C$",as.character(nrow(Origin_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 10, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$D$2:$D$",as.character(nrow(Management_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 11, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$E$2:$E$",as.character(nrow(Abundance_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 13, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$F$2:$F$",as.character(nrow(SpringAbundance_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 14, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$G$2:$G$",as.character(nrow(SummerAbundance_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 15, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$H$2:$H$",as.character(nrow(FallAbundance_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 16, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$I$2:$I$",as.character(nrow(WinterAbundance_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 17, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$J$2:$J$",as.character(nrow(RecordStatus_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 18, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$K$2:$K$",as.character(nrow(RefugeAccepted_values_df)+1)))

  openxlsx::dataValidation(wb, "SpeciesListForImport", col = 19, rows = 2:(nrow(org_dat)+1), type = "list", value =
                   paste0("'Drop-down values'!$L$2:$L$",as.character(nrow(Sensitive_values_df)+1)))
  })



  fn <- file.path(out_dir, xlsx_fn(org))
  if (file.exists(fn) && !overwrite) {
    warning("File exists and overwrite = FALSE. Skipping ", org, call. = FALSE)
  } else {
    openxlsx::saveWorkbook(wb, fn, overwrite = overwrite)
    if (verbose) cat("Exported", paste0(org, "\n"))
  }
  return()
}


xlsx_review_tags <- function(wb) {
  openxlsx::addWorksheet(wb, "tags")
  occurrence <- c("Present", "Present-Adjacent", "Probably Present",
                  "Probably Present-Adjacent",
                  "Probably Present-Historical")
  nativeness <- c("Native", "Native-Restoration", "Native-Cultivated",
                  "Native-Noxious", "NonNative", "NonNative-Cultivated",
                  "NonNative-Invasive", "NonNative-Noxious",
                  "Unknown", "Unknown-Cultivated", "Unknown-Noxious")
  accept_record = c("Yes", "ModifiedTaxonCode", "No")
  writeData(wb, 2, occurrence, startCol = 1)
  writeData(wb, 2, nativeness, startCol = 2)
  writeData(wb, 2, accept_record, startCol = 3)
}

xlsx_fn <- function(x) {
  x %>% gsub(" ", "_", .) %>%
    gsub("\\.|,|;", "", .) %>%
    paste0(".xlsx")
}
