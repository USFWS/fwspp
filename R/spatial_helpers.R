#' Check that necessary USFWS cadastral geodatabase is installed
#'
#' @noRd
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


#' Ensure the the cadastral data is a multipolygon object.
#'
#' Addresses https://github.com/r-spatial/sf/issues/427#issuecomment-874476218
#'
#' @param prop a FWS property boundary returned by \code{\link{find_fws}}
#'
#' @import sf
#'
#' @return a multipolygon sf object
ensure_multipolygons <- function(prop) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  sf::st_write(prop, tmp1)
  sf::gdal_utils("vectortranslate", tmp1, tmp2, c("-f", "GPKG", "-nlt", "MULTIPOLYGON"))
  prop_multi <- sf::st_read(tmp2)
  sf::st_sf(st_drop_geometry(prop), geom = st_geometry(prop_multi))
}


#' Generate a spatial buffer around a USFWS property
#'
#' @param prop a FWS property boundary returned by \code{\link{find_fws}}
#' @param buffer a numeric scalar indicating the buffer radius in meters
#'
#' @import sf
#'
#' @return A FWS property boundary buffered by the buffer radius
buffer_prop <- function(prop, buffer) {
  zone <- get_UTM_zone(mean(sf::st_bbox(prop)[c(1,3)]))
  prop %>%
    sf::st_transform(crs = as.integer(paste0(326, zone))) %>%
    sf::st_buffer(buffer * 1000) %>%
    sf::st_transform(crs = sf::st_crs(prop)$proj4string)
}


#' Get the UTM zone for a given longitude
#'
#' @param lon numeric scalar of longitude in decimal degrees
#'
#' @return numeric value that is the UTM zone
get_UTM_zone <- function(lon) {
  (floor((lon + 180)/6) %% 60) + 1
}


#' Clip species occurrence records to a FWS property boundary
#'
#' @param occ_recs a data frame of species occurrence records
#' @param prop a FWS property boundary returned by \code{\link{find_fws}}
#'
#' @import sf
#'
#' @return a data frame of species occurrence records clipped to a FWS property boundary
clip_occ <- function(occ_recs, prop) {

  sf::sf_use_s2(FALSE)  # Turn off s2 to avoid errors

  occ_pts <- sf::st_multipoint(cbind(occ_recs$lon, occ_recs$lat)) %>%
    sf::st_sfc(crs = sf::st_crs(prop)$proj4string) %>%
    sf::st_cast("POINT")
  suppressMessages(
    keep <- sapply(sf::st_intersects(occ_pts, prop),
                   function(z) {as.logical(length(z))})
    )

  sf::sf_use_s2(TRUE)  # Turn S2 back on

  occ_recs[keep, ]
}


#' Get WKT (Well-Known Text) representation of an sf multipolygon object
#'
#' @param prop a FWS property boundary returned by \code{\link{find_fws}}
#'
#' @import sf
#'
#' @return A FWS property boundary in WKT format, simplifies to a bounding box if it's complex
get_wkt <- function(prop) {

  wkt_txt <- prop %>%
    sf::st_convex_hull() %>%
    sf::st_geometry() %>%
    sf::st_as_text()

  if (nchar(wkt_txt) > 1500) {
    wkt_txt <- prop %>%
      prop_bb() %>%
      sf::st_geometry() %>%
      sf::st_as_text()
  }
  wkt_txt
}


#' Make property bounding box around a FWS property boundary
#'
#' @param prop a FWS property boundary returned by \code{\link{find_fws}}
#'
#' @importFrom magrittr %>%
#' @import sf
#'
#' @return A bounding box for a FWS property
prop_bb <- function(prop) {

  stopifnot(sf::st_geometry_type(prop) %in% c("POLYGON", "MULTIPOLYGON"))

  bb <- sf::st_bbox(prop)

  lon_range <- unname(bb[c(1, 3)])
  lat_range <- unname(bb[c(2, 4)])

  bb_coords <- cbind(sort(rep(lon_range, 2)), c(lat_range, rev(lat_range)))

  prop_bb <- rbind(bb_coords, bb_coords[1, ]) %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    sf::st_set_crs(sf::st_crs(prop)$proj4string)

  prop_bb
}


#' Calculate the area of a FWS property bounding box
#'
#' @param prop a FWS property boundary returned by \code{\link{find_fws}}
#'
#' @return The area of a FWS property bounding box
prop_bb_area <- function(prop) {
  prop_bb(prop) %>%
    sf::st_area() %>%
    as.numeric()
}


#' Split FWS properties into smaller pieces to avoid timeout errors when
#' querying species occurrence records from databases
#'
#' @param prop a FWS property boundary returned by \code{\link{find_fws}}
#'
#' @importFrom magrittr %>%
#' @import sf
#'
#' @return sf object of diced FWS properties
split_prop <- function(prop) {

  sf::sf_use_s2(FALSE)

  spl_prop <- lapply(seq_len(nrow(prop)), function(i) {
    tmp_prop <- prop[i, ]
    prop_area <- sf::st_area(tmp_prop) %>%
      as.numeric()
    bb_area <- prop_bb_area(tmp_prop)
    # If bounding box is mostly occupied by refuge, not much to be done.
    if (prop_area / bb_area > 0.5) return(tmp_prop)
    # If not, Slice and dice
    tmp_prop <- dice_prop(tmp_prop)
  })

  spl_prop <- do.call(rbind, spl_prop)

  sf::sf_use_s2(TRUE)

  spl_prop
}


#' Split FWS property at International Date Line
#'
#' @param prop a FWS property boundary returned by \code{\link{find_fws}}
#'
#' @importFrom magrittr %>%
#' @import sf
#'
#' @return A FWS property that is split along the international date line
split_at_idl <- function(prop) {

  message("Dealing with International Date Line issues")

  west <- list(
    matrix(c(-180, -89.99, -180, 89.99, -90, 89.99,
             -90, -89.99, -180, -89.99), byrow = TRUE,
           nrow = 5)) %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    sf::st_set_crs(4326)

  east <- list(
    matrix(c(90, -89.99, 90, 89.99, 180, 89.99, 180,
             -89.99, 90, -89.99), byrow = TRUE,
           nrow = 5)) %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    sf::st_set_crs(4326)

  suppressWarnings({
    west_prop <- sf::st_intersection(prop, west)
    east_prop <- sf::st_intersection(prop, east)
    prop <- rbind(west_prop, east_prop)
  })
}


#' Split large FWS property into smaller more manageable units
#'
#' @param prop a FWS property boundary returned by \code{\link{find_fws}}
#'
#' @importFrom magrittr %>%
#' @import sf
#'
#' @return A FWS property split into smaller units
dice_prop <- function(prop){

  message("Splitting property for more efficient queries.")

  stopifnot(all(sf::st_geometry_type(prop) %in% c("POLYGON", "MULTIPOLYGON")))

  sf::sf_use_s2(FALSE)

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
    raster::rasterToPolygons() %>%
    sf::st_as_sf()

  sf::st_crs(overlay) <- sf::st_crs(prop)

  diced_prop <- suppressMessages(suppressWarnings(sf::st_intersection(prop, overlay))) %>%
    select(-layer)

  sf::sf_use_s2(TRUE)

  diced_prop
}


#' Calculate approximate ratio of latitude:longitude distance	for a given
#' latitudinal position on the Earth
#'
#' @param lat numeric scalar of latitude in decimal degrees
#'
#' @importFrom geosphere distVincentyEllipsoid
#'
#' @return numeric ratio of latitude:longitude distance	for a given latitudinal position
ll_ratio <- function(lat) {
  d_lat <- geosphere::distVincentyEllipsoid(c(0, lat - 0.5), c(0, lat + 0.5))
  d_lon <- geosphere::distVincentyEllipsoid(c(0, lat), c(1, lat))
  d_lat / d_lon
}
