#' Prepare USFWS cadastral spatial data
#'
#' Converts USFWS cadastral geodatabase into \code{\link[sf]{sf}} and
#'  filters to relevant properties, if necessary.  Not meant to be called
#'  directly by user.
#'
#' @param refuge character string; USFWS properties from which to extract
#'  species occurrence records.  See \code{\link{fw_spp}}.
#' @param bnd character scalar; which refuge boundary to use.  See
#'  \code{\link{fw_spp}}
#' @param verbose logical; suppress messaging? See \code{\link{fw_spp}}
#'  during species occurrence queries
prep_cadastral <- function(refuge, bnd, verbose)
{

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

  # Reduce to refuges matching input query
  refs <- filter(r, .data$ORGNAME %in% refuge) %>%
    select(.data$ORGNAME, .data$FWSREGION)
  ref_labels <- refs$ORGNAME %>% unique() %>% Cap() %>% shorten_orgnames()

  # Dissolve into a single multi-part polygon by property
  if (!identical(nrow(refs), length(ref_labels))) {
    refs <- refs %>%
      group_by(.data$ORGNAME) %>%
      summarise()
  }

  # Impose zero width buffer to correct potentially invalid geometries
  # ~ 25 had ring self-intersections...
  refs <- suppressWarnings(sf::st_buffer(refs, 0))

  # Put in WGS84 even though GRS80 is practically identical
  refs <- sf::st_transform(refs, 4326)

  # Display queried refuges
  if (verbose) {
    message(paste(c(paste0(length(ref_labels), " properties will be queried:"),
                    strwrap(paste(sort(ref_labels), collapse = ", "),
                            indent = 4, exdent = 4)),
                  collapse = "\n"))
  }

  return(refs)
}
