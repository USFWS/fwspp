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
      return(invisible())
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

get_wkt <- function(prop) {
  wkt_txt <- prop %>%
    sf::st_convex_hull() %>%
    sf::st_geometry() %>% sf::st_as_text()
  if (nchar(wkt_txt) > 1500)
    wkt_txt <- prop %>% prop_bb() %>%
      sf::st_geometry() %>% sf::st_as_text()
  wkt_txt
}

prop_bb <- function(prop) {
  stopifnot(sf::st_geometry_type(prop) %in% c("POLYGON", "MULTIPOLYGON"))
  bb <- sf::st_bbox(prop)
  lon_range <- unname(bb[c(1, 3)]); lat_range <- unname(bb[c(2, 4)])
  bb_coords <- cbind(sort(rep(lon_range, 2)), c(lat_range, rev(lat_range)))
  prop_bb <- rbind(bb_coords, bb_coords[1, ]) %>% list() %>%
    sf::st_polygon() %>% sf::st_sfc() %>%
    sf::st_set_crs(sf::st_crs(prop)$proj4string)
  prop_bb
}

prop_bb_area <- function(prop) {
  prop_bb(prop) %>%
    sf::st_area() %>% as.numeric()
}
