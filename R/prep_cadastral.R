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

  sf::sf_use_s2(FALSE)  # Turn off s2 processing to prevent the invalid geometry errors

  # Get requested features
  gdb <- system.file("extdata", "FWSCadastral.gdb", package = "fwspp")

  message(wrap_message(
    paste("Storing USFWS cadastral geodatabase in a more efficient format.",
          "This will take several additional minutes.")))
  for (l in c("FWSInterest", "FWSApproved")) {
    message("Processing USFWS ", sub("FWS", "", l), " boundaries.")

    props <- suppressMessages(sf::read_sf(gdb, layer = l, stringsAsFactors = FALSE, quiet = TRUE))
    l_nm <- paste0("fws_", tolower(sub("FWS", "", l)))

    # Accommodate inconsistency with D'Arbonne in Approved vs Interest
    props <- mutate(props,
                    ORGNAME = gsub(" '", "'", ORGNAME),
                    ORGNAME = gsub("([A-Z])(\\.)([A-Z])", "\\1\\2 \\3", ORGNAME))

    # Cast to MULTIPOLYGON to avoide issues with MULTISURFACE geometries

    # props <- suppressMessages(sf::st_cast(props, to = "MULTIPOLYGON", warn = FALSE))

    props <- ensure_multipolygons(props)

    # Impose zero width buffer to correct potentially invalid geometries
    # e.g., ring self-intersections...
    props <- suppressMessages(sf::st_buffer(props, 0))

    # Dissolve into a single multi-part polygon by property, region, and type
    message("Dissolving into a single multi-part polygon")
    props <- props %>%
      group_by(ORGNAME, FWSREGION, RSL_TYPE) %>%
      {suppressMessages(summarize(.))} %>%
      ungroup()

    message("Transforming to WGS84")

    # Put in WGS84 even though GRS80 is practically identical
    props <- suppressMessages(sf::st_transform(props, 4326, quiet = TRUE))

    message("Buffering to correct invalid merged geometries")

    # Again, impose zero width buffer to correct potentially invalid merged geometries
    props <- suppressMessages(sf::st_buffer(props, 0))

    out_fn <- file.path(system.file("extdata", package = "fwspp"),
                        paste0(l_nm, ".rds"))
    saveRDS(props, file = out_fn)
  }

  sf::sf_use_s2(TRUE)  # Turn off s2 processing to prevent the invalid geometry errors

  message("Finished!")
}
