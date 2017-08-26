#' Create a RasterLayer object based on a sf object
#'
#' Methods to create a RasterLayer object. RasterLayer objects can be created from
#' scratch, a file, an Extent object, a matrix, an 'image' object, or from a Raster*,
#' sf, Spatial*, im (spatstat) asc, kasc (adehabitat*), grf (geoR) or kde object.
#'
#' See \code{\link[raster]{raster}} for more details.
#'
#' @param x filename (character), Extent, Raster*, sf object, SpatialPixels*,
#'   SpatialGrid*, 'image', matrix, im, or missing.
#'   Supported file types are the 'native' raster package format and
#'   those that can be read via rgdal (see readGDAL).
#' @inheritParams raster::raster
#'
#' @return RasterLayer
#'
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

#' Extent
#'
#' This function returns an Extent object of an sf object
#'
#' @param x An sf object
#'
#' @return Extent object
#'
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' extent(x)
#' xmin(x)
#' xmax(x)
#' ymin(x)
#' ymax(x)
extent.sf <- function(x) {
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

#' Get a coordinate reference system (projection)
#'
#' Get the coordinate reference system (CRS) of a Raster*, sf, or Spatial object.
#'
#' See \code{\link[raster]{projection}} for more details.
#'
#' @param x Raster*, sf, or Spatial object
#' @param asText logical. If TRUE, the projection is returned as text. Otherwise a CRS object is returned
#'
#' @return character object
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

#' Get cell number from x and y coordinates
#'
#' Get cell number(s) of a Raster* object from x and y coordinates.
#'
#' See \code{\link[raster]{cellFrom}} for more details.
#'
#' @param object Raster* object (or a SpatialPixels* or SpatialGrid* object)
#' @param xy matrix of x and y coordinates, sf, SpatialPoints or SpatialPointsDataFrame object
#' @export
#' @return vector of cell numbers
#'
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' cellFromXY(raster(x), x)
cellFromXY <- function(object, xy) {
  if (inherits(xy, "sf")) {
    xy <- spbabel::sptable(xy) %>% dplyr::select(x_, y_)
    cellFromXY(object, as.matrix(xy))
  } else {
    raster::cellFromXY(object, xy)
  }
}

#' Get cell number from line objects
#'
#' Get cell number(s) of a Raster* object from line objects.
#'
#' See \code{\link[raster]{cellFrom}} for more details.
#'
#' @param object Raster* object (or a SpatialPixels* or SpatialGrid* object)
#' @param lns sf (linestring or mutlilinestring) or SpatialLines object
#' @export
#' @return a list of cell numbers
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

#' Get cell number from polygons
#'
#' Get cell number(s) of a Raster* object from polygon objects.
#'
#' See \code{\link[raster]{cellFrom}} for more details.
#'
#' @param object Raster* object (or a SpatialPixels* or SpatialGrid* object)
#' @param p sf (polygon or mutlipolygon) or SpatialPolygons object
#' @param weights Logical. If TRUE, the fraction of each cell that is covered is also returned
#' @export
#' @return a list of cell numbers
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' pcells <- cellFromPolygon(raster(x, res = 0.1), x[1:4, ])
#' pcells0 <- list(c(27, 28, 29, 30, 116, 117, 118, 119, 120, 206, 207, 208,
#' 296), c(31, 32, 33, 34, 121, 122, 123), c(35, 36, 37, 38, 39,
#' 124, 125, 126, 127, 128, 213, 214, 215, 216, 217, 302), c(81,
#' 82, 83, 84, 85, 171, 172, 262, 352))
cellFromPolygon <- function (object, p, weights = FALSE) {
  which_cells <- function(x) {
    wch <- which(!is.na(raster::values(fasterize::fasterize(x, object))))
    ## this ensures we don't error when no overlap is found
    if (length(wch) < 1L) NULL else wch
  }
  if (inherits(p, "sf")) {
    cells_list <- lapply(split(p, seq_len(nrow(p))), which_cells
           )
    #cellFromPolygon(object, as(p, "Spatial"), weights = weights)
  } else {
    cells_list <- raster::cellFromPolygon(object, p, weights = weights)
  }
  cells_list
}

#' Rasterize an sf object of polygons
#'
#' Rasterize set of sf objects (polygon or mulitpolygon)
#'
#' See \code{\link[fasterize]{fasterize}} for more details.
#'
#' @param x an sf object with a geometry of POLYGON or MULTIPOLYGON
#' @param y RasterLayer object (raster template)
#' @param ... See \code{\link[fasterize]{fasterize}} for more arguments
#' @inheritParams fasterize::fasterize
#'
#' @return A raster of the same size, extent, resolution and projection as the provided raster template
#' @export
#'
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' rasterize(x, raster(x))
rasterize.sf <- function(x, y, ...) {
  fasterize::fasterize(sf = x, raster = y, ...)
}
setMethod(raster::rasterize, "sf", rasterize.sf)

#' Mask values in a Raster object
#'
#' Create a new Raster* object that has the same values as x,
#' except for the cells that are NA (or other maskvalue) in a 'mask'.
#' These cells become NA (or other updatevalue).
#' The mask can be either another Raster* object of the same extent and resolution,
#' a sf object, or a Spatial* object (e.g. SpatialPolygons) in which case all cells
#' that are not covered by the sf/Spatial object are set to updatevalue.
#' You can use inverse=TRUE to set the cells that are not NA (or other maskvalue)
#' in the mask, or not covered by the Spatial* object, to NA (or other updatvalue).
#'
#' See \code{\link[raster]{mask}} for more details.
#'
#' @param x Raster* object
#' @param mask Raster* object, sf object, or a Spatial* object
#' @inheritParams raster::mask
#'
#' @return Raster* object
#'
#' @export
#' @examples
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' new_raster <- raster(x)
#' new_raster[] <- sample(seq_len(ncell(new_raster)))
#' raster_masked <- mask(new_raster, x)
#' plot(raster_masked)
#' plot(x, add = TRUE)
mask <- function(x, mask, ...) {
  if (inherits(mask, "sf")) {
    out <- x
    fmask <- fasterize::fasterize(mask, x)
    out[is.na(fmask)] <- NA
#    out <- raster::mask(x, mask = as(mask, "Spatial"), ...)
  } else {
    out <- raster::mask(x, mask, ...)
  }
  out
}
