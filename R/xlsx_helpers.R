construct_review <- function(org, fwspp, overwrite, verbose, out_dir) {
  org_dat <- fwspp[[org]] %>%
    mutate(occurrence = "Probably present",
           nativeness = NA_character_,
           accept_record = ifelse(is.na(.data$accept_record) |
                                    grepl("NPSpecies", .data$accept_record),
                                  "Yes", "No")) %>%
    select(.data$org_name, .data$category, .data$taxon_code,
           .data$sci_name, .data$com_name,
           .data$occurrence:.data$accept_record,
           .data$evidence, .data$note) %>%
    arrange(.data$category, .data$sci_name)

  wb <- createWorkbook()
  addWorksheet(wb, "fwspp_review")
  col_widths <- c(10, 15, 13, 29, 29, 16, 12, 9, 70, 40)
  setColWidths(wb, 1, cols = seq_along(org_dat), widths = col_widths)
  freezePane(wb, 1, firstRow = TRUE)

  # Add tags and data validation
  create_tag_ws(wb)
  add_validation(wb, end_row = nrow(org_dat) + 1)

  # Write and save it
  writeData(wb, 1, org_dat, withFilter = TRUE)
  fn <- file.path(out_dir, construct_fn(org))
  if (file.exists(fn) && !overwrite) {
    warning("File exists and overwrite = FALSE. Skipping ", org, call. = FALSE)
  } else {
    saveWorkbook(wb, fn, overwrite = overwrite)
    if (verbose) cat("Exported", paste0(org, "\n"))
  }
  return()
}

create_tag_ws <- function(wb) {
  addWorksheet(wb, "tags")
  occurrence <- c("Present", "Present-Adjacent", "Probably Present",
                  "Probably Present-Adjacent",
                  "Probably Present-Historical")
  nativeness <- c("Native", "Native-Restoration", "Native-Cultivated",
                  "Native-Noxious", "NonNative", "NonNative-Cultivated",
                  "NonNative-Invasive", "NonNative-Noxious")
  accept_record = c("Yes", "Modify", "No")
  writeData(wb, 2, occurrence, startCol = 1)
  writeData(wb, 2, nativeness, startCol = 2)
  writeData(wb, 2, accept_record, startCol = 3)
}

add_validation <- function(wb, end_row) {
  dataValidation(wb, 1, col = 6, rows = 2:end_row,
                 type = "list", value = "'tags'!$A$1:$A$5")
  dataValidation(wb, 1, col = 7, rows = 2:end_row,
                 type = "list", value = "'tags'!$B$1:$B$8")
  dataValidation(wb, 1, col = 8, rows = 2:end_row,
                 type = "list", value = "'tags'!$C$1:$C$3")
}

construct_fn <- function(x) {
  x %>% gsub(" ", "_", .) %>%
    gsub("\\.|,|;", "", .) %>%
    paste0(".xlsx")
}
