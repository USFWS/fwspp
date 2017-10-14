#' Prepare USFWS cadastral spatial data
#'
#' Converts USFWS cadastral geodatabase into \code{\link[sf]{sf}} and
#'  filters to relevant properties, if necessary.  Exported by not
#'  typically called directly by user.
#'
#' @param prop character string; USFWS properties from which to extract
#'  species occurrence records.  See \code{\link{fw_spp}}.
#' @param bnd character scalar; which property boundary to use.  See
#'  \code{\link{fw_spp}}
#' @param verbose logical; suppress messaging? See \code{\link{fw_spp}}
#'  during species occurrence queries
#' @export
prep_cadastral <- function(prop, bnd, verbose) {

  # Get requested features
  gdb <- system.file("extdata", "FWSCadastral.gdb", package = "fwspp")

  if (identical(bnd, "admin"))
    r <- sf::read_sf(gdb, layer = "FWSInterest", stringsAsFactors = FALSE, quiet = TRUE)
  else
    r <- sf::read_sf(gdb, layer = "FWSApproved", stringsAsFactors = FALSE, quiet = TRUE)

  # Accommondate inconsistency with D'Arbonne in Approved vs Interest
  r <- mutate(r,
              ORGNAME = gsub(" '", "'", .data$ORGNAME),
              ORGNAME = gsub("([A-Z])(\\.)([A-Z])", "\\1\\2 \\3", .data$ORGNAME))

  # Check for MULTISURFACE geometry and cast to MULTIPOLYGON
  if ("MULTISURFACE" %in% unique(st_geometry_type(st_geometry(r))))
    r <- sf::st_cast(r, "MULTIPOLYGON")

  # Reduce to properties matching input query
  props <- filter(r, .data$ORGNAME %in% prop) %>%
    select(.data$ORGNAME, .data$FWSREGION)
  prop_labels <- props$ORGNAME %>% unique() %>% Cap() %>% shorten_orgnames()

  # Dissolve into a single multi-part polygon by property
  if (!identical(nrow(props), length(prop_labels))) {
    props <- props %>%
      group_by(.data$ORGNAME) %>%
      summarise()
  }

  # Impose zero width buffer to correct potentially invalid geometries
  # e.g., ring self-intersections...
  props <- suppressWarnings(sf::st_buffer(props, 0))

  # Put in WGS84 even though GRS80 is practically identical
  props <- sf::st_transform(props, 4326)

  # Display queried properties
  if (verbose) {
    message(paste(c(paste0(length(prop_labels), " properties will be queried:"),
                    strwrap(paste(sort(prop_labels), collapse = ", "),
                            indent = 4, exdent = 4)),
                  collapse = "\n"))
  }
  props
}
