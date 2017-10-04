#' Retrieve GRBIO institution codes
#'
#' Retrieve institution codes for contributors to the Global Registry of
#' Biodiversity Repositories (\url{http://grbio.org/})
get_grbio <- function() {
  if (!(curl::has_internet()))
    stop(paste(strwrap(
      paste("No internet connection detected.  Please try again later when a stable",
            "connection is available.  Sorry for the inconvenience.")),
      collapse = "\n"))

  # Check if copy from today exists and read, if so
  csv <- system.file("extdata", "grbio.csv", package = "fwspp")

  if (!identical(csv, "")) {
    csv_date <- file.info(csv) %>%
      pull(.data$mtime) %>% as.Date()
    if (!(csv_date < Sys.Date())) {
      csv <- utils::read.csv(csv, stringsAsFactors = FALSE)
      return(csv)
    }
  }

  message("Updating Global Registry of Biodiversity Repositories data")
  csv <- utils::read.csv("http://grbio.org/sites/default/files/export/grbio/biorepositories.csv",
                           stringsAsFactors = FALSE, na.strings = c("", "NA")) %>%
    select(inst_code = .data$Institutional.Code.Acronym,
           inst_cat_url = .data$URL.for.institutional.specimen.catalog,
           inst_url = .data$URL.for.main.institutional.website) %>%
    mutate(inst_url = ifelse(!is.na(.data$inst_cat_url),
                             .data$inst_cat_url,
                             .data$inst_url)) %>%
    select(.data$inst_code, .data$inst_url) %>%
    # Add some missing codes that regularly pop up
    bind_rows(data.frame(inst_code = c("CLO", "EMAP_NCA"),
                         inst_url = c("http://ebird.org",
                                      "https://archive.epa.gov/emap/archive-emap/web/html/index-78.html"),
                         stringsAsFactors = FALSE))

  utils::write.csv(csv, row.names = FALSE,
                   file = file.path(system.file("extdata", package = "fwspp"), "grbio.csv"))
  return(csv)
}
