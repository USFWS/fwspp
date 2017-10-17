#' Prepare USFWS cadastral spatial data
#'
#' Converts USFWS cadastral geodatabase into two \code{\link[sf]{sf}} and
#'  stores them for faster loading on subsequent use by
#'  \code{link{prep_properties}}. Exported but not typically called
#'  directly by user unless \code{\link{install_fws_cadastral}} fails.
#'
#' @export
prep_cadastral <- function() {

  check_cadastral()

  # Get requested features
  gdb <- system.file("extdata", "FWSCadastral.gdb", package = "fwspp")

  message(wrap_message(
    paste("Storing USFWS cadastral geodatabase in a more efficient format.",
          "This will take several additional minutes.")))
  for (l in c("FWSInterest", "FWSApproved")) {
    message("Processing USFWS ", sub("FWS", "", l), " boundaries.")
    props <- sf::read_sf(gdb, layer = l, stringsAsFactors = FALSE, quiet = TRUE)
    l_nm <- paste0("fws_", tolower(sub("FWS", "", l)))

    # Accommondate inconsistency with D'Arbonne in Approved vs Interest
    props <- mutate(props,
                    ORGNAME = gsub(" '", "'", .data$ORGNAME),
                    ORGNAME = gsub("([A-Z])(\\.)([A-Z])", "\\1\\2 \\3", .data$ORGNAME))

    # Cast to MULTIPOLYGON to avoide issues with MULTISURFACE geometries
    props <- sf::st_cast(props, to = "MULTIPOLYGON")

    # Impose zero width buffer to correct potentially invalid geometries
    # e.g., ring self-intersections...
    props <- suppressWarnings(sf::st_buffer(props, 0))

    # Dissolve into a single multi-part polygon by property, region, and type
    props <- props %>%
      group_by(.data$ORGNAME, .data$FWSREGION, .data$RSL_TYPE) %>%
      summarize() %>% ungroup()

    # Put in WGS84 even though GRS80 is practically identical
    props <- sf::st_transform(props, 4326)

    # Again, impose zero width buffer to correct potentially invalid merged geometries
    props <- suppressWarnings(sf::st_buffer(props, 0))

    out_fn <- file.path(system.file("extdata", package = "fwspp"),
                        paste0(l_nm, ".rds"))
    saveRDS(props, file = out_fn)
  }
  message("Finished!")
}
