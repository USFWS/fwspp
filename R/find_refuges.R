#' Find USFWS properties available for query.
#'
#' @param refuge character string scalar or vector (i.e., multiple entries allowed) with
#'  which to search and return valid USFWS identifiers (i.e., ORGNAMEs) for species
#'  occurrence queries. Default (`NULL`) returns all available National Wildlife Refuges,
#'  but other property types are available (see \code{ptype} argument).  The search is
#'  case-insensitive and matches partial strings. Regular expressions are also allowed.
#'  See examples.
#' @param ptype character string scalar or vector of types of USFWS properties to search.
#'  Default is to search for National Wildlife Refuges only (\code{ptype = "NWR"}).  Other
#'  options include "WPA" (Waterfowl Production Areas), "WMA" (Wildlife Management Areas),
#'  "FSA" (Farm Service Agency properties), and "NFH" (National Fish Hatcheries).
#' @param region integer indicating which USFWS Region(s) to search (
#' \url{https://www.fws.gov/where}); valid values range from 1 to 8
#' @export
#' @return character vector of organizational names (ORGNAME) of USFWS properties meeting
#'  the search criteria.  This output can be passed directly to \code{\link{fw_spp}} as
#'  the \code{refuge} argument.
#' @examples
#' # Get all National Wildlife Refuges
#' all_refs <- find_refuges()
#'
#' # Search for refuges using a partial name match
#' ml <- find_refuges("longleaf")
#'
#' # Search for refuges matching multiple strings
#' multi <- find_refuges(c("longleaf", "romain"))
#'
#' # Regular expressions also work
#' multi <- find_refuges("longl|romain")
#'
#' # Search for all refuges beginning with "T"
#' ts <- find_refuges("^t")
#'
#' # Return all southeast (region 4) refuges
#' r4 <- find_refuges(region = 4)
#'
#' # Return all mountain-prairie (region 6) refuges and waterfowl production areas
#' r6 <- find_refuges(ptype = c("NWR", "WPA"), region = 6)

find_refuges <- function(refuge = NULL, ptype = "NWR", region = 1:8L)
{

  if (!any(ptype %in% c("NWR", "WPA", "WMA", "FSA", "NFH")))
    stop("Unknown property type (`ptype`).\n",
         "See available options in `?find_refuges`.")

  r <- system.file("extdata", "refuge_info.rds", package = "fwspp") %>%
    readRDS()

  # Filter by region
  if (!all(region %in% 1:8L)) stop("Valid USFWS regions range from 1 to 8.\n",
                                   "See https://www.fws.gov/where for assistance.")
  r <- filter(r,
              .data$FWSREGION %in% region,
              .data$RSL_TYPE %in% ptype)

  if (is.null(refuge)) {
    return(sort(r$ORGNAME))
  } else {
    refuge <- paste(refuge, collapse = "|")
    refs <- filter(r, grepl(refuge, .data$ORGNAME, ignore.case = TRUE)) %>%
      pull(.data$ORGNAME) %>% sort()
    if (identical(refs, character(0)))
      stop("No refuges matched your search criteria.")
    return(refs)
  }
}
