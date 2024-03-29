#' Species occurrence records on U.S. Fish and Wildlife properties
#'
#' Geographic query of biodiversity databases based on U.S. Fish and Wildlife
#'  Service (USFWS) property boundaries
#'
#' **Important usage limitations**: This function exists strictly to extract
#'  occurrence data for a given \code{fws} property. Attempts at estimating
#'  or inferring relative abundance are most strongly discouraged and almost
#'  certainly meaningless.
#'
#' Also note that the extraction of records occurs on a property-by-property
#'  basis so the same record may occur in multiple polygons depending on
#'  buffer specifications.
#'
#' @section General overview:
#' The basic process is to query GBIF, iDigBio, the Berkeley
#'  'Ecoinformatics' Engine, and AntWeb based on the bounding box
#'  associated with each \code{fws} and any requested buffer.  For VertNet,
#'  the API requires spatial searches using a lat/lon coordinate and search
#'  radius.  We calculate the radius needed to fully capture the desired
#'  geometry.  Records are subsequently filtered based on the exact geometry.
#'
#' Properties that occupy a relatively small area compared to the corresponding
#'  bounding box will be split into smaller pieces to avoid unnecessarily large
#'  queries. Likewise, queries (typically GBIF) that contain many
#'  records (> 125000) are split to improve efficiency (both) and recover all
#'  records (GBIF).
#'
#' We provide the options to:
#'  \itemize{
#'    \item scrub records to reduce the number of returned records for each
#'       \code{fws} (details below);
#'    \item attempt to link each observation to some standardized taxonomic information
#'       (details below)
#'  }
#'
#' @section Scrubbing details:
#' By default (\code{scrub = "strict"}), \code{fws_occ} scrubs a lot of records.
#'  Specifically, within a given property, it retains a single record for each
#'  species, giving preference to records with associated media (e.g., photo, audio).
#'  The retained record is typically the most recent with evidence to support the
#'  identification. Records for which evidence was not available (i.e., no associated
#'  collection or catalog number) are removed.  Moderate scrubbing
#'  (\code{scrub = "moderate"}) attempts only to remove duplicate records (i.e., identical
#'  catalog numbers) and redundant observations (i.e., multiple individuals of the same
#'  species recorded on the same date at a single location). All records can be
#'  returned with (\code{scrub = "none"}).
#'
#' @section Taxonomy information details:
#' By default (\code{taxonomy = TRUE}), \code{fwspp} attempts to check the validity of
#'  scientific names against the Integrated Taxonomic Information System (ITIS). It does
#'  this not by connecting to ITIS directly, but by requesting information from a web
#'  service maintained by the U.S. Fish and Wildlife Service as part of their FWSpecies database
#'  Note that this means if taxonomy information
#'  is requested, and an ITIS match found, the scientific name will be converted to the
#'  "accepted" ITIS scientific name, and the corresponding ITIS Taxonomic Serial Number,
#'  an NPS-specific taxon code, a common name used by USFWS, and a general organism
#'  "category" (e.g., Mammals, Birds, Fungi) are returned.
#'
#' @section Additional boundary information:
#' The boundaries specified by the "admin" option to the \code{bnd} argument
#'  delineates those lands and waters administered by the USFWS in North
#'  America, U.S. Trust Territories and Possessions. It may also include
#'  inholdings that are not administered by the USFWS. The primary source
#'  for this information is the USFWS Realty program. See
#'  \url{https://ecos.fws.gov/ServCat/Reference/Profile/60739} for more
#'  information.
#'
#' The boundaries specified by the "acq" option to the \code{bnd} argument
#'  delineates the external boundaries of lands and waters that are approved
#'  for acquisition by the USFWS in North America, U.S. Trust Territories and
#'  Possessions. The primary source for this information is the USFWS Realty
#'  program. See \url{https://ecos.fws.gov/ServCat/Reference/Profile/60738} for
#'  more information.
#'
#' @section Query timeout details:
#' By default, timeout is calculated based on testing of BISON queries on a ~ 20 Mbps
#'  internet connection. BISON queries are nearly always the largest (by # records)
#'  *contiguous* requests (GBIF is slower but requests occur in smaller chunks).  If
#'  timeouts are a recurring problem, however, it may be worth checking your download
#'  speed (e.g. \url{http://www.speedtest.net}) and setting this parameter if your
#'  speeds are considerably below ~ 20 Mbps to allow more time for HTTP requests to
#'  process. For example, if your download speed is estimated at 5 Mbps, you might
#'  consider setting this parameter to \code{timeout = 4} or so. If regular timeout
#'  persist after adjusting this parameter, however, please contact the maintainer
#'  with details or, better yet, file an issue at
#'  \url{https://github.com/USFWS/fwspp/issues}.
#'
#' @param fws \code{data.frame} of organizational names (ORGNAME) and
#'  type (RSL_TYPE) of USFWS properties and their associated USFWS region
#'  (FWSREGION), within and around which the user wishes to extract species
#'  occurrence records. It is strongly recommended to use the results generated
#'  by \code{\link{find_fws}} to ensure proper content and formatting. See examples.
#' @param bnd character scalar indicating the type of property boundary to use.
#'  Default ("admin") uses the current administrative boundary.  The approved
#'  acquisition boundary ("acq") is another option.
#' @param buffer numeric scalar; distance (km) from the \code{fws} \code{bnd} to
#'  include in the search for species occurrence records. Default is no buffer.
#' @param scrub character; one of "strict" (default), "moderate", or "none",
#'  indicating the extent to which to reduce the number of records returned for
#'  a given \code{fws}.  See details.
#' @param taxonomy logical (default TRUE); attempt to link occurrence records to
#'  standardized taxon information? See details.
#' @param verbose logical (default TRUE); print messages during species occurrence
#'  queries?
#' @param timeout numeric; if specified, serves as a multiplier for the timeout
#'  value calculated internally (e.g., \code{timeout = 2} doubles the amount of
#'  time to allow for HTTP requests to process. By default (\code{timeout = NULL}),
#'  the query timeout is set programmatically and conservatively.  See details.
#'
#' @importFrom pbapply pbapply
#' @importFrom purrr safely
#'
#' @export
#'
#' @return an \code{list} of class \code{fwspp} with observations for each property with
#'  the following columns if taxonomic information is requested (\code{taxonomy = TRUE};
#'   default). If \code{taxonomy = FALSE}, only a subset of these columns is returned.
#'  \describe{
#'    \item{org_name}{official organizational name of USFWS property}
#'    \item{sci_name}{Scientific name associated with the observation.}
#'    \item{lon}{Longitude (WGS84) of observation.}
#'    \item{lat}{Latitude (WGS84) of observation.}
#'    \item{loc_unc_m}{Locational uncertainty (m) of observation coordinates; typically
#'          unavailable.}
#'    \item{year}{Year of observation, if available.}
#'    \item{month}{Month of observation, if available.}
#'    \item{day}{Day of month of observation, if available.}
#'    \item{evidence}{Attempt to link observation to a URL that best substantiates the
#'          observation, if available, generally in the following order: (1) URL to
#'          observation with media (photo, audio, video) or the media itself, (2) URL
#'          to the observation in the original collection, (3) URL of the collection,
#'          with catalog number, or (4) URL of the institution housing the collection,
#'          with catalog number.}
#'    \item{bio_repo}{Biodiversity database source of the observation, currently one of
#'          GBIF, BISON, iDigBio, VertNet, EcoEngine, or AntWeb.}
#'    \item{acc_sci_name}{Accepted/valid scientific name from ITIS, if available.}
#'    \item{com_name}{Regularly used vernacular names, if available.}
#'    \item{taxon_rank}{Taxonomic rank of observation.}
#'    \item{category}{Generic taxonomic grouping for taxa used in FWSpecies database.}
#'    \item{tsn}{Accepted/valid Taxonomic Serial Number from ITIS, if available.}
#'    \item{note}{Additional notes on the observation, currently restricted to indicating
#'          that a matching taxon was not found in ITIS or trouble singling out a taxon
#'          from FWSpecies.}
#'  }
#'
#' @examples
#' \dontrun{
#' # Single refuge, administrative boundary, no buffer
#' # By default, records are scrubbed very strictly (see Details)
#' # Mountain Longleaf National Wildlife Refuge
#' ml <- find_fws("longleaf")
#' ml_occ <- fws_occ(fws = ml)
#'
#' # Multiple refuges, acquisition boundary with 5 km buffer, moderate scrubbing,
#' # no taxonomy info added
#' multi <- find_fws(c("longleaf", "romain"))
#' multi_occ <- fws_occ(fws = multi, bnd = "acq", buffer = 5, scrub = "moderate",
#'                      taxonomy = FALSE)
#'
#' # All Region 4 (southeast) refuges, with defaults
#' r4 <- find_fws(region = 4)
#' r4_occ <- fw_spp(r4)
#' }

fws_occ <- function(fws = NULL, bnd = c("admin", "acq"),
                    buffer = 0, scrub = c("strict", "moderate", "none"),
                    taxonomy = TRUE, verbose = TRUE,
                    timeout = NULL,start_day = NULL,
                    start_month= NULL, start_year = NULL) {

  if (is.null(fws) || !is.data.frame(fws))
    stop("You must provide valid property names to query.",
         "\nSee `?find_fws` for examples.")

  if (is.null(start_day))
    start_day<-1

  if (is.null(start_month))
    start_month<-1

  if (is.null(start_year))
    start_year<-1776

  start_date<-as.POSIXlt(paste0(start_year,"-",start_month,"-",start_day))


  bnd <- match.arg(bnd)
  scrub <- match.arg(scrub)

  # Import spatial data for relevant properties
  check_cadastral()
  props <- prep_properties(fws, bnd, verbose)

  start_time <- Sys.time()

  # Cycle through properties
  if (verbose)
    out <- lapply(fws$ORGNAME, function(prop) {
      retrieve_occ(props, prop, buffer, scrub, timeout,start_date)})
  else
    out <- pbapply::pblapply(fws$ORGNAME, function(prop) {
      suppressMessages(
        retrieve_occ(props, prop, buffer, scrub, timeout,start_date))})

  attributes(out) <- list(names = fws$ORGNAME,
                          class = "fwspp",
                          boundary = bnd, scrubbing = scrub,
                          buffer_km = buffer,
                          query_dt = start_time)

  # Taxomony linking, if requested
  if (taxonomy) {
    safe_tax <- purrr::safely(add_taxonomy)
    out_tax <- safe_tax(out)
    if (is_error(out_tax)) {
      message("Taxonomy retrieval failed with the following error:\n   ",
              out_tax$error$message)
      message(
        wrap_message(paste("Skipping taxonomy. Please send the resulting `fwspp` object",
                           "to the maintainer of the `fwspp` package.\n",
                           "You may also try again later using `fwspp::add_taxonomy`.")))
    }
    else out <- out_tax$result
  }
  out
}
