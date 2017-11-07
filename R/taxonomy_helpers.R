#' Retrieve taxonomic information for one or more scientific names
#'
#' Retrieves very basic taxonomic information for a given taxa, if available,
#'  including ITIS Taxonomic Serial Number (\code{tsn}), the \code{taxon_code}
#'  assigned by the National Park Service (and used also by the US Fish &
#'  Wildlife Service), \code{common_name}(s), a generic taxon group, and a
#'  \code{note} if a match is not found
#'
#' @param sci_name character vector (case-insensitive) of scientific names
#'  for which to retrieve basic taxonomic information at the *species* level;
#'  that is, subspecies (trinomials) are currently ignored
#' @return a \code{data.frame} of basic taxonomic infomration
#' @export
#' @examples
#' \dontrun{
#' retrieve_taxonomy(c("GULo gulo", "Lampropeltis getuLA HOLBrookI",
#'                     "Lampropeltis holbrooki", "Pseudemys scripta",
#'                     "Fakus specialus"))
#' }
retrieve_taxonomy <- function(sci_name) {
  try_tax <- try_verb_n(get_taxonomy, wait = 1)
  out <- pbapply::pblapply(sci_name, function(sn) {
    # message(sn)
    acc_sci_name <- sn <- clean_sci_name(sn)
    tax <- suppressMessages(try_tax(acc_sci_name))
    if (is_error(tax)) stop(paste0(tax$error$message), " (", sn, ")")
    tax <- tax$result

    # No match
    if (is.null(tax)) return(empty_tax(sn, "No match found; check spelling?"))

    # Filter to species-level entries
    tax <- filter_taxonomy(tax, sn)
    if (identical(names(tax), names(empty_tax()))) return(tax)

    # Attempt to retrieve valid/accepted scientific name
    if (is.na(tax$Usage) || !(tax$Usage %in% c("accepted", "valid"))) {
      acc_sci_name <- tax$AcceptedTaxa[[1]]$ScientificName %>%
        clean_sci_name()
      if (identical(acc_sci_name, character(0)))
        acc_sci_name <- NA_character_
      else {
        tax <- suppressMessages(try_tax(acc_sci_name))
        if (is_error(tax)) stop(paste0(tax$error$message), " (", acc_sci_name, ")")
        tax <- tax$result %>%
          filter_taxonomy(acc_sci_name)
      }
    }

    # Extract basic ITIS and common name
    taxonomy <- pull(tax, .data$ClassificationSource) %>%
      jsonlite::flatten() %>%
      mutate(tsn = ifelse(.data$Detail.Code < 0, NA_integer_, as.integer(.data$Detail.Code)),
             note = ifelse(is.na(.data$tsn), "Present in NPSpecies, but no ITIS match", NA_character_)) %>%
      select(.data$tsn, .data$note)
    com_name <- paste(unlist(tax$CommonNames), collapse = ", ")
    tax <- tax %>%
      select(.data$TaxonCode:.data$CommonNames) %>%
      mutate(sci_name = clean_sci_name(sn),
             acc_sci_name = acc_sci_name,
             com_name = ifelse(is.null(com_name), NA_character_, com_name),
             taxon_code = as.integer(.data$TaxonCode)) %>%
      bind_cols(taxonomy) %>%
      select(.data$sci_name, .data$acc_sci_name, .data$com_name, rank = .data$Rank,
             category = .data$NPSpeciesCategory, .data$taxon_code, .data$tsn, .data$note)
    tax
  })
  bind_rows(out)
}

# Get NPS taxoncode using Scientific Name
get_taxonomy <- function(sci_name) {
  base_url <- "http://irmaservices.nps.gov/v2/rest/taxonomy/searchByScientificName/"
  q_sci_name <- utils::URLencode(sci_name)
  q_url <- paste0(base_url, q_sci_name, "?format=json")
  tax <- jsonlite::fromJSON(q_url)
  if (identical(tax, list())) return()
  tax
}

filter_taxonomy <- function(tax, sci_name) {
  tax <- tax[which(tax$Rank == "Species" &
                   grepl(paste0(tolower(sci_name), "$"), tolower(tax$ScientificName))), ]
  # None remaining means all were subspecies (generally)
  if (nrow(tax) == 0)
    return(empty_tax(sci_name, "No species rank match found"))
  if (nrow(tax) > 1) {
    # Multiple species in different taxa groups
    if (n_distinct(tax$NPSpeciesCategory) > 1)
      return(empty_tax(sci_name, "Multiple species match"))
    else {
      # Same taxa group may be same species but differnt authorities
      # See if filtering by accepted name solves problem
      tax <- tax[tax$Usage %in% c("valid", "accepted"), ]
      if (nrow(tax) != 1)
        return(empty_tax(sci_name, "Multiple species match"))
    }
  }
  tax
}

empty_tax <- function(sci_name = NA_character_, note = NA_character_) {
  tax <- data.frame(sci_name = clean_sci_name(sci_name),
                    acc_sci_name = NA_character_,
                    com_name = NA_character_,
                    rank = NA_character_,
                    category = NA_character_,
                    taxon_code = NA_integer_,
                    tsn = NA_integer_,
                    note = note,
                    stringsAsFactors = FALSE)
  tax
}

pull_sci_names <- function(fwspp) {
  fwspp %>%
    purrr::modify_if(is_error, as.null) %>%
    purrr::compact() %>%
    purrr::map(pull, .data$sci_name) %>%
    purrr::flatten_chr() %>% unique() %>% sort()
}


join_taxonomy <- function(fwspp, taxonomy) {
  lapply(fwspp, function(i) {
    if (!is.null(i) && !is_error(i))
      left_join(i, taxonomy, by = "sci_name") %>%
      mutate(sci_name = ifelse(is.na(.data$acc_sci_name),
                               .data$sci_name, .data$acc_sci_name))
    else i
  })
}

strip_taxonomy <- function(fwspp) {
  to_drop <- names(empty_tax())[-1]
  lapply(fwspp, function(i) {
    if (!is.null(i) && !is_error(i))
      select(i, -one_of(to_drop))
    else i
  })
}
