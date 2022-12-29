#' Filter USFWS properties for query
#'
#' Filters USFWS cadastral \code{\link[sf]{sf}} data to the relevant
#'  properties. Exported by not typically called directly by user.
#'
#' @param prop_df \code{data.frame} of organizational names (ORGNAME) and
#'  type (RSL_TYPE) of USFWS properties and their associated USFWS region
#'  (FWSREGION). It is strongly recommended to use the results generated
#'  by \code{\link{find_fws}} to ensure proper content and formatting.
#' @param bnd character scalar; which property boundary to use.  See
#'  \code{\link{fws_occ}}
#' @param verbose logical; suppress messaging? See \code{\link{fws_occ}}
#'  during species occurrence queries
#' @export
prep_properties <- function(prop_df, bnd = "admin", verbose = FALSE) {

  l <- case_when(
    bnd == "admin" ~ "fws_interest.rds",
    TRUE ~ "fws_approved.rds")
  has_sf <- file.exists(system.file("extdata", l, package = "fwspp"))
  if (!has_sf) stop("The necessary USFWS Cadastral files were not located. ",
                    "Please run `install_fws_cadastral()`.", call. = FALSE)

  # Get requested features
  gdb_sf <- system.file("extdata", l, package = "fwspp")
  r <- readRDS(gdb_sf)

  # Filter
  props <- semi_join(r, prop_df, by = c("ORGNAME", "FWSREGION", "RSL_TYPE"))

  # Reduce to properties matching input query
  prop_labels <- props$ORGNAME %>% Cap() %>% shorten_orgnames()
  prop_labels <- paste0(prop_labels, " (R", props$FWSREGION, ")")

  # Display queried properties
  if (verbose) {
    message(paste(c(paste0(length(prop_labels), " properties will be queried:"),
                    strwrap(paste(sort(prop_labels), collapse = ", "),
                            indent = 4, exdent = 4)),
                  collapse = "\n"))
  }
  sf::st_make_valid(props)
}
