
#' @noRd
shorten_orgnames <- function(orgnames) {

  old <- c("National Wildlife Refuge", "Waterfowl Production Area",
           "Wildlife Management Area", "National Fish Hatchery",
           "Farm Service Agency")

  new <- abbreviate(old, 3)

  for(i in seq_along(old))
    orgnames <- gsub(old[i], new[i], orgnames, ignore.case = TRUE)
  orgnames
}


#' @noRd
check_dup_orgnames <- function(org_df) {

  if (n_distinct(org_df$ORGNAME) < nrow(org_df)) {
    orgs <- pull(org_df, ORGNAME)
    dups <- orgs[duplicated(orgs)]
    dups <- paste(" * ", dups, "\n") %>% paste(., collapse = "")
    warning(paste0(
      "Your search returned multiple USFWS properties with the same name.\n",
      dups,
      wrap_message(
        paste("To avoid querying an unwanted property, check the output and",
              "perhaps specify the `region` argument to avoid unintended behavior."))),
      call. = FALSE)
  }

  org_df
}


#' @noRd
Cap <- function(string, words = c("all", "first")) {

  words <- match.arg(words)
  isna <- is.na(string)
  string <- tolower(string)

  if (identical(words, "all")) {
    s <- strsplit(string, " ")
    s <- sapply(s, function(i) {
      paste(toupper(substring(i, 1,1)), substring(i, 2), sep="", collapse=" ")
    })
  } else {
    s <- paste0(toupper(substr(string, 1, 1)),
                substr(string, 2, nchar(string)))
  }

  s[isna] <- NA_character_
  s
}


#' @noRd
is_missing <- function(string) {
  is.na(string) | nchar(string) == 0 | grepl("^ +$", string)
}


#' @noRd
wrap_message <- function(msg) {
  paste(strwrap(msg), collapse = "\n")
}


#' @noRd
clean_sci_name <- function(sn_string) {

  sn_miss <- "Undesignated|None|Unknown|Missing"
  spec_epi_miss <- " sp$| sp.$| spp$| spp.$"

  # Trim any leading/trailing blank spaces
  sn_string <- gsub("^\\s+|\\s+$", "", sn_string) %>%
    # Replace any "missing" values with actual missing values
    gsub(sn_miss, NA_character_, ., ignore.case = TRUE) %>%
    # Replace generic species with blanks...
    gsub(spec_epi_miss, "", .) %>%
    # Replace periods (e.g., abbreviated scientific names)
    gsub("\\.","", .) %>%
    # Remove numeric values
    gsub('[[:digit:]]+', '', .) %>%
    # Remove authorities (assumed format: Name, Year)
    gsub(" [A-Z][a-zA-Z]*, \\d{4}", "", .) %>%
    # Remove special characters
    gsub("[[:punct:]]", "", .) %>%
    # Remove tabs
    gsub("\t", "", .) %>%
    # Replace multiply sign for hybrids; assumes no other unicode slips in...
    iconv("UTF-8", "ascii", sub = "x")

  if (identical(sn_string, character(0))) return(sn_string)

  # Drop trinomials, if present, but preserve *properly* formatted hybrids
  # This also drops most authorities...
  # e.g., Aronia X prunifolia, Aronia x prunifolia
   n_words <- sapply(gregexpr("\\S+", sn_string), function(x) sum(x > 0))
  #removing this action due to bugs 9/14/2023
  #if (any(n_words == 3) && any(grepl(" X | x ", sn_string)))
  #  return(Cap(sn_string, "first"))

  sn_string %>% sub("^(\\S*\\s+\\S+).*", "\\1", .) %>%
    Cap("first")
}


#' @noRd
clean_loc_unc <- function(x) {
  x <- iconv(x, "latin1", "ASCII", "-")
  as.integer(round(as.numeric(gsub(" *|.*-|[mM]|meters|Meters|NA", "", x))))
}


#' @noRd
clean_com_name <- function(cn_string) {

  out <- sapply(cn_string, function(i) {
    cnames <- i %>%
      strsplit(", ") %>%
      unlist() %>%
      unique() %>%
      gsub("NA|NA_character_", NA_character_, .) %>%
      stats::na.omit() %>%
      paste(collapse = ", ")
    cnames
  })

  unname(out)
}
