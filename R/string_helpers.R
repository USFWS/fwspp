shorten_orgnames <- function(orgnames) {
  old <- c("National Wildlife Refuge", "Waterfowl Production Area",
           "Wildlife Management Area", "National Fish Hatchery",
           "Farm Service Agency")
  new <- abbreviate(old, 3)
  for(i in seq_along(old))
    orgnames <- gsub(old[i], new[i], orgnames, ignore.case = TRUE)
  orgnames
}

check_dup_orgnames <- function(orgnames) {
  if (n_distinct(orgnames) < length(orgnames)) {
    dups <- orgnames[duplicated(orgnames)] %>% unique()
    dups <- paste(" * ", dups, "\n") %>% paste(., collapse = "")
    stop(
      paste0("Your search returned multiple USFWS properties with the same name.\n",
             dups,
             wrap_message(
               paste("Specify the `region` argument to avoid unintended behavior.",
                     "Properties with the same name will be considered collectively."))))
  }
  orgnames
}

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

is_missing <- function(string) {
  is.na(string) | nchar(string) == 0 | grepl("^ +$", string)
}

wrap_message <- function(msg) {
  paste(strwrap(msg), collapse = "\n")
}

create_sci_name <- function(..., sep = " ") {
  # Thanks http://stackoverflow.com/a/15673180/2726564
  L <- list(...)
  L <- lapply(L, function(x) {x[is.na(x) | x == "sp" |
                                  x == "sp." | x == "None" |
                                  x == "none"] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret == ""
  Cap(ret, "first")
}

clean_sci_name <- function(sn_string) {
  sn_miss <- "Undesignated|None|Unknown|Missing"
  spec_epi_miss <- " sp$| sp.$| spp$| spp.$"
  # Trim any leading/trailing blank spaces
  sn_string <- gsub("^\\s+|\\s+$", "", sn_string) %>%
    # Replace any "missing" values with actual missing values
    gsub(sn_miss, NA_character_, .) %>%
    # Replace generic species with blanks...
    sub(spec_epi_miss, "", .) %>%
    # Drop trinomials, if present
    sub("^(\\S*\\s+\\S+).*", "\\1", .) %>%
    # Drop improper characters (e.g., multiply sign for hybrids)
    iconv(., "UTF-8", "ascii", sub = "") %>%
    # Proper capitalization
    Cap("first")
  sn_string
}

clean_loc_unc <- function(x) {
  x <- iconv(x, "latin1", "ASCII", "-")
  as.integer(round(as.numeric(gsub(" *|.*-|[mM]|meters|Meters|NA", "", x))))
}
