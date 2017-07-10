#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' raster(x)
raster <- function(x, ...) {
  if (inherits(x, "sf")) {
    out <- raster::raster(raster::extent(x), ...)
    if (is.na(projection(out))) projection(out) <- projection(x)
  } else {
    out <- raster::raster(x, ...)
  }
  out
}
#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#'
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' extent(x)
#' xmin(x)
#' xmax(x)
#' ymin(x)
#' ymax(x)
extent.sf <- function(x, ...) {
  raster::extent(unclass(sf::st_bbox(x))[c("xmin", "xmax", "ymin", "ymax")])
}
setMethod(raster::extent, "sf", extent.sf)
xmin.sf <- function(x) {
  unclass(sf::st_bbox(x))[c("xmin")]
}
setMethod(raster::xmin, "sf", xmin.sf)
xmax.sf <- function(x) {
  unclass(sf::st_bbox(x))[c("xmax")]
}
setMethod(raster::xmax, "sf", xmax.sf)
ymin.sf <- function(x) {
  unclass(sf::st_bbox(x))[c("ymin")]
}
setMethod(raster::ymin, "sf", ymin.sf)
ymax.sf <- function(x) {
  unclass(sf::st_bbox(x))[c("ymax")]
}
setMethod(raster::ymax, "sf", ymax.sf)
#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' projection(x)
projection <- function(x, asText = TRUE) {
  if (inherits(x, "sf")) {
    projection(sf::st_crs(x)$proj4string, asText = asText)
  } else {
    raster::projection(x, asText = asText)
  }
}

#' Title
#'
#' @param object
#' @param xy
#' @export
#' @return
#'
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' cellFromXY(raster(x), x)  ## checkout hypertidy/tabularaster
cellFromXY <- function(object, xy) {
  if (inherits(xy, "sf")) {
    xy <- spbabel::sptable(xy) %>% dplyr::select(x_, y_)
    cellFromXY(object, as.matrix(xy))
  } else {
    raster::cellFromXY(object, xy)
  }
}
#' Title
#'
#' @param object
#' @param lns
#' @export
#' @return
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' cellFromLine(raster(x), st_cast(x[1:2, ], "MULTILINESTRING"))
#'
cellFromLine <- function(object, lns) {
  if (inherits(lns, "sf")) {
    raster::cellFromLine(object, as(lns, "Spatial"))
  } else {
    raster::cellFromLine(object, lns)
  }
}
#' Title
#'
#' @param x
#' @param y
#' @param ...
#' @export
#' @return
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' cellFromPolygon(raster(x, res = 0.1), x[1:4, ])
cellFromPolygon <- function (object, p, weights = FALSE) {
  if (inherits(p, "sf")) {
  cellFromPolygon(object, as(p, "Spatial"), weights = weights)
  } else {
  raster::cellFromPolygon(object, p, weights = weights)
  }
    }
#' Title
#'
#' @param x
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' rasterize(x, raster(x))
rasterize.sf <- function(x, y, ...) {
  fasterize::fasterize(sf = x, raster = y, ...)
}
setMethod(raster::rasterize, "sf", rasterize.sf)
