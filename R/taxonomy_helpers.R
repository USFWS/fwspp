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
#'                     "Fakus speciesus", "Salsola iberica"))
#' }
retrieve_taxonomy <- function(sci_name) {
  out <- pbapply::pblapply(sci_name, function(sn) {
    # message(sn)
    acc_sci_name <- sn <- clean_sci_name(sn)
    tax <- fws_taxonomy(acc_sci_name)

    # No match
    if (is.null(tax)) return(empty_tax(sn, "No match found; check spelling?"))

    # Filter to species-level entries
    tax <- filter_taxonomy(tax, sn)
    if (identical(names(tax), names(empty_tax()))) return(tax)

    # Running track of common names (useful when tracking down valid taxon)
    cnames <- ifelse(is.na(tax$com_name), NA_character_, strsplit(tax$com_name, ";")[[1]])

    # Attempt to retrieve valid/accepted scientific name
    if (is.na(tax$usage) || !(tax$usage %in% c("accepted", "valid"))) {
      acc_tc <- NA_integer_
      if ("acc_taxon_code" %in% names(tax))
        acc_tc <- tax$acc_taxon_code
      if (is.na(acc_tc))
        acc_sci_name <- NA_character_
      else {
        tax <- fws_taxonomy_by_code(acc_tc)
        acc_sci_name <- tax$sci_name
        if (!is.na(tax$com_name)) {
          # add new unique common names
          new_cn <- strsplit(tax$com_name, ";")[[1]]
          is_new <- !(tolower(new_cn) %in% tolower(cnames))
          cnames <- as.character(stats::na.omit(c(cnames, new_cn[is_new])))
        }
      }
    }

    # Round into final consolidated format
    tax <- tax %>%
      mutate(sci_name = clean_sci_name(sn),
             acc_sci_name = acc_sci_name,
             com_name = ifelse(all(is.na(cnames)), NA_character_,
                               paste(cnames, collapse = ", ")),
             tsn = ifelse(.data$tsn < 0, NA_integer_, .data$tsn),
             note = ifelse(is.na(.data$tsn),
                           "Present in FWSpecies, but no ITIS match", NA_character_)) %>%
      select(.data$sci_name, .data$acc_sci_name, .data$com_name, rank = .data$rank,
             category = .data$category, taxon_code = .data$taxon_code, .data$tsn, .data$note)
    tax
  })
  bind_rows(out)
}

#' Retrieve raw USFWS taxonomic information matching scientific name query
#'
#' Queries U.S. Fish & Wildlife Service taxonomy records by scientific name,
#'  returning all records that match the input \code{sci_name} parameter
#'  (including partial matches; see Details). Returns basic taxonomic
#'  information, if available, including ITIS Taxonomic Serial Number
#'  (\code{tsn}), the \code{taxon_code} assigned by the US Fish & Wildlife Service),
#'  \code{common_name}(s), a generic taxon group, whether the taxon is
#'  valid according to ITIS and, if not, the accepted \code{taxon_code}
#'  if a match is found.
#'
#' It is somewhat unclear how the USFWS records are filtered. The best guess
#'  is that it parses the unique words in the query string (\code{sci_name})
#'  and returns all records that match that set of unique words. The order
#'  of query words is not strictly enforced. See examples.
#'
#' @param sci_name scientific name character scalar (case-insensitive) for
#'  which to retrieve basic taxonomic information. Unlike
#'  \code{\link{retrieve_taxonomy}}, matches are not restricted to the
#'  species level; that is, subspecies may be returned.
#' @return a \code{data.frame} of basic taxonomic infomration or \code{NULL}
#'  if there are no matching records
#' @export
#' @examples
#' \dontrun{
#' fws_taxonomy("GULo gulo")
#' fws_taxonomy("Lampropeltis holbrooki")
#' fws_taxonomy("holBRooki lampropeltis")
#' }

fws_taxonomy <- function(sci_name) {
  base_url <- "https://ecos.fws.gov/ServCatServices/v2/rest/taxonomy/searchByScientificName/"
  q_sci_name <- utils::URLencode(sci_name)
  q_url <- paste0(base_url, q_sci_name, "?format=json")
  tmp <- try(jsonlite::fromJSON(q_url), silent = TRUE)
  if (is_error(tmp)) {
    warning("Taxonomy retrieval failed for ", sci_name, call. = FALSE)
    return()
  }
  if (identical(tmp, list())) return()

  tax <- data.frame(
    taxon_code = as.integer(tmp$TaxonCode),
    rank = tmp$Rank,
    sci_name = tmp$ScientificName,
    com_name = ifelse(is.na(tmp$CommonNames), NA_character_,
                      sapply(tmp$CommonNames, paste, collapse = ";")),
    category = tmp$NPSpeciesCategory,
    usage = tmp$Usage,
    tsn = as.integer(tmp$ClassificationSource$Detail$Code),
    stringsAsFactors = FALSE)

  if ("AcceptedTaxa" %in% names(tmp))
    tax <- mutate(tax,
                  acc_taxon_code = sapply(tmp$AcceptedTaxa, function(i) {
                    ifelse(is.null(i), NA_integer_, as.integer(i$TaxonCode))}))
  tax
}

# Get FWS taxonomy using FWS Taxon Code
fws_taxonomy_by_code <- function(taxon_code) {
  base_url <- "https://ecos.fws.gov/ServCatServices/v2/rest/taxonomy/"
  q_url <- paste0(base_url, taxon_code, "?codeType=taxoncode&format=json")
  tax <- try(jsonlite::fromJSON(q_url), silent = TRUE)
  if (is_error(tax)) {
    warning("Taxonomy retrieval failed for taxon code ", taxon_code, call. = FALSE)
    return()
  }

  tax <- data.frame(
    taxon_code = as.integer(tax$TaxonCode),
    rank = tax$Rank,
    sci_name = tax$ScientificName,
    com_name = ifelse(is.null(tax$CommonNames), NA_character_,
                      paste(tax$CommonNames, collapse = ";")),
    category = tax$NPSpeciesCategory,
    usage = ifelse(is.null(tax$Usage), NA_character_, tax$Usage),
    tsn = ifelse(is.null(tax$ClassificationSource$Detail$Code), NA_integer_,
                 as.integer(tax$ClassificationSource$Detail$Code)),
    stringsAsFactors = FALSE)
  tax
}

filter_taxonomy <- function(tax, sci_name) {
  tax <- tax[tax$rank == "Species" &
               grepl(paste0("^", tolower(sci_name), "$"), tolower(tax$sci_name)), ]
  # None remaining means all were subspecies (generally)
  if (nrow(tax) == 0)
    return(empty_tax(sci_name, "No species rank match found"))
  if (nrow(tax) > 1) {
    # Multiple species in different taxa groups
    if (n_distinct(tax$category) > 1)
      return(empty_tax(sci_name, "Same scientific name for multiple taxa"))
    else {
      # Try to identify accepted taxon if all are invalid/unaccepted
      if (all(!tax$usage %in% c("valid", "accepted")) &&
          "acc_taxon_code" %in% names(tax)) {
        acc_tax <- unique(tax$acc_taxon_code)
        if (length(acc_tax) == 1) {
          # Single accepted taxon, return first match
          tax <- filter(tax, .data$acc_taxon_code == acc_tax)
          return(tax[1, ])
        }
        # Multiple ambiguous taxa, provide them
        return(empty_tax(sci_name, paste("Ambiguous accepted taxon:",
                                         paste(acc_tax, collapse = ", "))))
      }
      # Occasionally filtering by accepted name solves problem
      tax <- tax[tax$usage %in% c("valid", "accepted"), ]
      # but if it doesn't
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
  valid_fwspp <- fwspp[sapply(fwspp, function(i) !is_error(i) && !is.null(i))]
  sn_list <- lapply(valid_fwspp, pull, .data$sci_name)
  sn <- sort(unique(utils::stack(sn_list)$values))
  sn
}

join_taxonomy <- function(fwspp, taxonomy) {
  lapply(fwspp, function(i) {
    if (!is.null(i) && !is_error(i))
      left_join(i, taxonomy, by = "sci_name") %>%
      mutate(sci_name = ifelse(is.na(.data$acc_sci_name),
                               .data$sci_name, .data$acc_sci_name)) %>%
      select(-.data$acc_sci_name)
    else i
  })
}

#' Check if \code{fwspp} object has taxonomic information attached
#'
#' @param fwspp an \code{fwspp} object returned by \code{\link{fws_occ}}
#' @export

has_taxonomy <- function(fwspp) {
  tax_vec <- sapply(fwspp, function(i) "taxon_code" %in% names(i))
  as.logical(sum(tax_vec))
}

strip_taxonomy <- function(fwspp) {
  to_drop <- names(empty_tax())[-1]
  lapply(fwspp, function(i) {
    if (!is.null(i) && !is_error(i))
      select(i, -one_of(to_drop))
    else i
  })
}
