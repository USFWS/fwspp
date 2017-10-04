## Functions for deciding whether to split USFWS properties and, if so,
## perform the actual splitting in a hopefully semi-intelligent way

#' If necessary, split large properties into more manageable units for queries
#'
#' Based on some preliminary timing comparisons, we split properties with one
#'  or more polygons occupying a bounding box larger that 100 km^2
#'
#' @param ref \code{\link[sf]{sf}} object for a single USFWS property
#' @param area_cutoff numeric scalar indicating the polygon area (in km^2) above
#'  which to split \code{ref} into smaller, approximately \code{area_cutoff}-sized
#'  polygons for processing
split_ref <- function(ref, area_cutoff = 10^2) {
  area_cutoff <- 1000^2 * area_cutoff
  ref_bb <- matrix(sf::st_bbox(ref), 2)
  # Bounding box area if property queried as a whole
  uni_area <- poly_bb_area(ref)
  ref_split <- sf::st_cast(ref, to = "MULTIPOLYGON") %>%
    sf::st_cast(., to = "POLYGON", warn = FALSE)
  split_area <- poly_bb_area(ref_split)

  if (uni_area < split_area)
    return(ref)
  else if (max(split_area) > area_cutoff)
    return(dice_polys(ref, area_cutoff))
  else
    return(ref_split)
}

#' Calculate bounding box area(s) for \code{sf} object
#'
#' @param polys \code{\link[sf]{sf}} object with POLYGON or MULTIPOLYGON geometry
poly_bb_area <- function(polys) {
  stopifnot(all(sf::st_geometry_type(polys) %in% c("POLYGON", "MULTIPOLYGON")))
  out <- lapply(seq_len(nrow(polys)), function(i) {
    bb <- sf::st_bbox(polys[i, ])
    lon_range <- unname(bb[c(1, 3)]); lat_range <- unname(bb[c(2, 4)])
    poly_coords <- cbind(sort(rep(lon_range, 2)), c(lat_range, rev(lat_range)))
    poly_area <- rbind(poly_coords, poly_coords[1, ]) %>% list() %>%
      sf::st_polygon() %>% sf::st_sfc() %>%
      sf::st_set_crs(sf::st_crs(polys)$proj4string) %>%
      sf::st_area() %>% as.numeric()
    poly_area
  })
  unlist(out)
}

#' Do the actual splitting of large properties into more manageable units
#'
#' @param polys  \code{\link[sf]{sf}} for a single USFWS property
#' @param max_area numeric scalar indicating the approximate maximum area
#'  (in m^2) to allow when splitting \code{ref}. Most will be smaller, often
#'  considerably so, given the irregular boundaries of most properties
dice_polys <- function(polys, max_area){
  stopifnot(all(sf::st_geometry_type(polys) %in% c("POLYGON", "MULTIPOLYGON")))
  polys_bb <- matrix(sf::st_bbox(polys), 2)
  n_sf <- ceiling(poly_bb_area(polys) / max_area)
  # Add arbitrary # rows to balance lat/lon distances in R4
  if (n_sf > 1) {
    llr <- ll_ratio(mean(polys_bb[2, ]))
    nc <- floor(sqrt(n_sf / llr))
    nr <- ceiling(nc * llr)
  } else return(polys)
  overlay <- raster::raster(raster::extent(polys_bb), nrow = nr, ncol = nc) %>%
    raster::rasterToPolygons() %>% sf::st_as_sf()
  sf::st_crs(overlay) <- sf::st_crs(polys)
  out <- suppressWarnings(sf::st_intersection(polys, overlay)) %>%
    select(-.data$layer)
  out
}
