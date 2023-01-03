#' Check that necessary USFWS cadastral geodatabase is installed
check_cadastral <- function() {

  gdb <- file.exists(system.file("extdata", "FWSCadastral.gdb", package = "fwspp"))
  info <- file.exists(system.file("extdata", "fws_info.rds", package = "fwspp"))

  if (!all(gdb, info)) {
    message(
      wrap_message(paste("\nPrior to using `fwspp` you must install the current USFWS",
                         "cadastral geodatabase. To do so, please run",
                         "`install_fws_cadastral()`. This will take several minutes.",
                         "\n\nWould you like to install now?")))
    utils::menu(c("Yes", "No")) -> resp

    if (resp == 1)
      install_fws_cadastral()
    else {
      message("Very well. Run `install_fws_cadastral()` at your leisure.")
      return(invisible())
    }
  }
}

# Generate buffer around USFWS property
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
    keep <- sapply(sf::st_intersects(occ_pts, prop),
                   function(z) {as.logical(length(z))})
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

split_prop <- function(prop) {
  spl_prop <- lapply(seq_len(nrow(prop)), function(i) {
    tmp_prop <- prop[i, ]
    prop_area <- sf::st_area(tmp_prop) %>% as.numeric()
    bb_area <- prop_bb_area(tmp_prop)
    # If bounding box is mostly occupied by refuge, not much to be done.
    if (prop_area / bb_area > 0.5) return(tmp_prop)
    # slice and dice
    tmp_prop <- dice_prop(tmp_prop)
  })
  spl_prop <- do.call(rbind, spl_prop)
}

split_at_idl <- function(prop) {
  message("Dealing with International Date Line issues")
  west <- list(
    matrix(c(-180, -89.99, -180, 89.99, -90, 89.99,
             -90, -89.99, -180, -89.99), byrow = TRUE,
           nrow = 5)) %>%
    sf::st_polygon() %>% sf::st_sfc() %>%
    sf::st_set_crs(4326)
  east <- list(
    matrix(c(90, -89.99, 90, 89.99, 180, 89.99, 180,
             -89.99, 90, -89.99), byrow = TRUE,
           nrow = 5)) %>%
    sf::st_polygon() %>% sf::st_sfc() %>%
    sf::st_set_crs(4326)
  suppressWarnings({
    west_prop <- st_intersection(prop, west)
    east_prop <- st_intersection(prop, east)
    prop <- rbind(west_prop, east_prop)
  })
}

# Do the actual splitting of large properties into more manageable units
#
# @param prop \code{\link[sf]{sf}} to dice
#
dice_prop <- function(prop){
  message("Splitting property for more efficient queries.")
  stopifnot(all(sf::st_geometry_type(prop) %in% c("POLYGON", "MULTIPOLYGON")))
  bb <- matrix(sf::st_bbox(prop), 2)
  llr <- apply(bb, 1, diff)
  llr[2] <- llr[2] * ll_ratio(mean(bb[2, ]))
  llr <- llr[2] / llr[1]
  if (llr < 1) {
    nr <- ceiling(sqrt(20 / llr))
    nc <- floor(20 / nr)
  } else {
    nc <- floor(sqrt(20 / llr))
    nr <- ceiling(20 / nc)
  }
  overlay <- raster::raster(raster::extent(bb), nrow = nr, ncol = nc) %>%
      raster::rasterToPolygons() %>% sf::st_as_sf()
  sf::st_crs(overlay) <- sf::st_crs(prop)
  diced_prop <- suppressMessages(suppressWarnings(sf::st_intersection(prop, overlay))) %>%
    select(-.data$layer)
  diced_prop
}

# Calculate approximate ratio of latitude:longitude distance	for a given
# latitudinal position on the Earth
# @param lat numeric scalar of latitude in decimal degrees
ll_ratio <- function(lat) {
  d_lat <- geosphere::distVincentyEllipsoid(c(0, lat - 0.5), c(0, lat + 0.5))
  d_lon <- geosphere::distVincentyEllipsoid(c(0, lat), c(1, lat))
  d_lat/d_lon
}
