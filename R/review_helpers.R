add_review_validation <- function(wb, end_row) {
  openxlsx::dataValidation(wb, 1, col = 6, rows = 2:end_row,
                 type = "list", value = "'tags'!$A$1:$A$5")
  openxlsx::dataValidation(wb, 1, col = 7, rows = 2:end_row,
                 type = "list", value = "'tags'!$B$1:$B$11")
  openxlsx::dataValidation(wb, 1, col = 8, rows = 2:end_row,
                 type = "list", value = "'tags'!$C$1:$C$3")
}

is_review <- function(df) {
  rev_cols <- c("org_name", "category", "taxon_code", "sci_name",
                "com_name", "occurrence", "nativeness", "accept_record",
                "evidence", "note")
  identical(names(df), rev_cols)
}

import_review <- function(xlsx, verbose) {
  review <- try(
    readxl::read_excel(xlsx,
                       col_types = c("text", "text", "numeric", "text",
                                     "text", "text", "text", "text",
                                     "text", "text")), silent = TRUE)
  if (is_error(review)) {
    warning(basename(xlsx), " does not match expected format. Skipping.", call. = FALSE)
    return()
  }
  if (!is_review(review)) {
    warning(basename(xlsx), " does not match expected format. Skipping.", call. = FALSE)
    return()
  }
  if (verbose) cat(basename(xlsx), "imported successfully.\n")
  review
}

process_review <- function(df) {
  # Remove unaccepted observations
  df <- filter(df, accept_record != "No")

  # Pull modified records
  acc_recs <- filter(df, accept_record == "Yes")
  mods <- filter(df, grepl("Modif", accept_record) & !is.na(taxon_code))

  revised_codes <- unique(mods$taxon_code)
  message("Retrieving updated taxonomic information.")
  revised_codes <- pbapply::pblapply(revised_codes, nps_taxonomy_by_code) %>%
    bind_rows() %>%
    mutate(acc_sci_name = sci_name) %>%
    select(taxon_code, category, acc_sci_name,
           upd_com_name = com_name)

  # Join updated taxonomy to modified records
  mods <- select(mods, -category) %>%
    left_join(revised_codes, by = "taxon_code") %>%
    rowwise() %>%
    mutate(sci_name = ifelse(is.na(acc_sci_name),
                             sci_name, acc_sci_name),
           com_name = clean_com_name(c(com_name, upd_com_name))) %>%
    ungroup()
  acc_recs <- bind_rows(acc_recs, mods)

  # Add cost center
  cost_centers <- get_unit_codes()
  acc_recs <- left_join(acc_recs, cost_centers, by = "org_name")

  # Rename relevant columns
  acc_recs <- acc_recs %>%
    select(`Scientific Name` = sci_name, TaxonCode = taxon_code,
           UnitCode, CommonNames = com_name,
           ExternalLinks = evidence, Occurrence = occurrence,
           Nativeness = nativeness, ORGNAME = org_name) %>%
    mutate(RecordStatus = "Approved",
           RefugeAccepted = "Yes",
           Nativeness = ifelse(is.na(Nativeness), "Unknown",
                               Nativeness))

  ## Set column names/order of output data frame
  out_df <- utils::read.csv(
    text = paste(c("Scientific Name", "TaxonCode", "ORGNAME", "UnitCode", "CommonNames",
                   "Refuge Synonyms", "ExternalLinks", "Occurrence", "OccurrenceClass",
                   "Nativeness", "Management", "Abundance", "AbundanceNotes", "SpringAbundance",
                   "SummerAbundance", "FallAbundance", "WinterAbundance", "RecordStatus",
                   "RefugeAccepted", "Sensitive", "SensitiveNotes", "HistoryNotes"),
                 collapse = ", "), check.names = FALSE)
  out_df <- bind_rows(out_df, acc_recs)
  out_df
}
