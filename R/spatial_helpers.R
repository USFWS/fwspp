#' Check that necessary USFWS cadastral geodatabase is installed
check_cadastral <- function() {

  gdb <- file.exists(system.file("extdata", "FWSCadastral.gdb", package = "fwspp"))
  info <- file.exists(system.file("extdata", "fws_info.rds", package = "fwspp"))

  if (!all(gdb, info)) {
    message(paste(strwrap(
      paste("\nPrior to using `fwspp` you must install the current USFWS",
            "cadastral geodatabase. To do so, please run",
            "`install_fws_cadastral()`.  This will take several minutes.",
            "\n\nWould you like to install now?")),
      collapse = "\n"))
    utils::menu(c("Yes", "No")) -> resp

    if (resp == 1)
      install_fws_cadastral()
    else {
      message("Very well. Run `install_fws_cadastral()` at your leisure.")
      invisible(NULL)
    }
  }
}

#' Generate buffer around USFWS property
buffer_prop <- function(prop, buffer) {
  zone <- get_UTM_zone(mean(sf::st_bbox(prop)[c(1,3)]))
  prop %>% sf::st_transform(crs = as.integer(paste0(326, zone))) %>%
    sf::st_buffer(buffer * 1000) %>%
    sf::st_transform(crs = sf::st_crs(prop)$proj4string)
}

get_UTM_zone <- function(lon) {
  (floor((lon + 180)/6) %% 60) + 1
}

#' Calculate approximate ratio of latitude:longitude distance
#'
#' Used to try and generate relatively square polygons when chopping
#'  properties
#'
#' @param lat numeric scalar of latitude in decimal degrees
ll_ratio <- function(lat) {
  d_lat <- geosphere::distVincentyEllipsoid(c(0, lat - 0.5), c(0, lat + 0.5))
  d_lon <- geosphere::distVincentyEllipsoid(c(0, lat), c(1, lat))
  d_lat/d_lon
}

clip_occ <- function(occ_recs, prop) {
  occ_pts <- sf::st_multipoint(cbind(occ_recs$lon, occ_recs$lat)) %>%
    sf::st_sfc(crs = sf::st_crs(prop)$proj4string) %>%
    sf::st_cast("POINT")
  suppressMessages(
    keep <- sapply(sf::st_intersects(occ_pts, prop), function(z) {
      if (length(z)==0) FALSE else TRUE})
  )
  occ_recs[keep, ]
}

get_wkt <- function(poly) {
  poly %>% sf::st_convex_hull() %>%
    sf::st_geometry() %>% sf::st_as_text()
}
