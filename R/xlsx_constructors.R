xlsx_review <- function(org, fwspp, overwrite, verbose, out_dir) {
  dat <- fwspp[[org]]
  if (is.null(dat)) {
    if (verbose) cat("No records for", paste0(org, ". Skipping...\n"))
    return()
  }
  org_dat <- dat %>%
    mutate(occurrence = "Probably present",
           nativeness = NA_character_,
           accept_record = ifelse(is.na(.data$note) | grepl("NPSpecies", .data$note),
                                  "Yes", "No")) %>%
    select(.data$org_name, .data$category, .data$taxon_code,
           .data$sci_name, .data$com_name,
           .data$occurrence:.data$accept_record,
           .data$evidence, .data$note) %>%
    arrange(.data$category, .data$sci_name)

  wb <- createWorkbook()
  addWorksheet(wb, "Species List for Review")
  col_widths <- c(10, 15, 13, 29, 29, 16, 12, 9, 70, 40)
  setColWidths(wb, 1, cols = seq_along(org_dat), widths = col_widths)
  freezePane(wb, 1, firstRow = TRUE)

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

xlsx_submission <- function(org, occ_data, out_dir, overwrite, verbose) {
  org_dat <- filter(occ_data, .data$ORGNAME == org) %>%
    select(-.data$ORGNAME)

  wb <- createWorkbook()
  addWorksheet(wb, "Species List for Import")
  setColWidths(wb, 1, cols = seq_along(org_dat), widths = "auto")
  freezePane(wb, 1, firstRow = TRUE)

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

xlsx_review_tags <- function(wb) {
  addWorksheet(wb, "tags")
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
