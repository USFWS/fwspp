#' Add or update taxonomic information to a \code{fwspp} object
#'
#' Use this function to add or update basic taxonomic information to an
#'  \code{fwspp} object; see \code{\link{retrieve_taxonomy}} for
#'  a description of the information returned and its source.
#'
#' @param fwspp an \code{fwspp} object returned by \code{\link{fws_occ}}
#' @param taxonomy a \code{data.frame} returned by
#'  \code{\link{retrieve_taxonomy}}; this is meant primarily for internal
#'  \code{fws_occ} use
#'
#' @return a \code{\link{fwspp}} object with added (or updated) basic
#'  taxonomic information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ml <- find_fws("longleaf")
#' ml <- fws_occ(fws = ml)
#' ml <- add_taxonomy(ml)
#' }
add_taxonomy <- function(fwspp, taxonomy = NULL) {
  if(!inherits(fwspp, "fwspp"))
    stop("You must supply a `fwspp` object. See `fwspp::fws_occ`.")
  hold_attr <- attributes(fwspp)
  if (!is.null(taxonomy))
    fwspp <- join_taxonomy(fwspp, taxonomy)
  else {
    if (has_taxonomy(fwspp)) {
      if (!interactive())
        message("Taxonomy is already present. Updating...")
      else {
        message(
          wrap_message(paste("\nIt appears taxonomy is already present.",
                             "Do you wish to update the taxomony?")))
        utils::menu(c("Yes", "No")) -> resp
        if (resp != 1) return(fwspp)
      }
      ## REMOVE ALREADY PRESENT TAXONOMY
      fwspp <- strip_taxonomy(fwspp)
    }
    all_spp <- pull_sci_names(fwspp)
    message("Retrieving taxonomic information for ", length(all_spp), " taxa...")
    tax_info <- retrieve_taxonomy(all_spp)
    fwspp <- join_taxonomy(fwspp, tax_info)
  }
  attributes(fwspp) <- hold_attr
  fwspp
}
