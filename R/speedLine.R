#' Interpolate speed along a route shape
#'
#' @param avl speeds data.table
#' @param by_dist distance between gridded points, in units of crs.
#' @param shapes GTFS shapes table, see \code{\link{readGTFS}}.
#' @param crs a projected coordinate reference system, see \code{\link{st_crs}}.
#'
#' @return a data.table of speeds with imputed lonlat 
#' (\code{lon_imp} and \code{lat_imp}) for distances \code{by_dist} apart.
#' @export
speedOnLine <- function(avl, by_dist = 1, shapes, crs) {
  # roll join by start_date, shape_id, trip_id, avl_dist_traveled
  dist_lu <- avl[, .(avl_dist_traveled = seq(by_dist * floor(min(avl_dist_traveled, na.rm = TRUE)/by_dist), by_dist * ceiling(max(avl_dist_traveled, na.rm = TRUE)/by_dist), by = by_dist)), keyby = c('start_date', 'shape_id', 'trip_id')]
  expanded <- avl[dist_lu, on = c('start_date', 'shape_id', 'trip_id', 'avl_dist_traveled'), roll = TRUE][, c('start_date', 'shape_id', 'trip_id', 'avl_dist_traveled', 'match_lat', 'match_lon', 'mps', 'TOD', 'direction_id', 'route_short_name', 'DOW', 'service_name', 'trip_desc')]
  expanded[, c('lon_imp', 'lat_imp') := interpolateLatLon(avl_dist_traveled, shape_id, shapes, crs), by = c('start_date', 'trip_id', 'shape_id')]
  return(expanded)
}

#' Interpolate locations along a shape
#'
#' @param d distance between interpolated points, in units of \code{crs}
#' @param id valid GTFS shape_id
#' @inheritParams speedOnLine
#'
#' @return a list of vectors of longitude and latitude for each interpolated 
#' point.
#' @export
interpolateLatLon <- function(d, id, shapes, crs) {
	# generate spatial line from shape
	shape_line <- spTransform(SpatialLines(list(Lines(Line(shapes[shape_id == id, c('shape_pt_lon', 'shape_pt_lat')]), ID = id)), CRS('+proj=longlat +ellps=WGS84')), CRS(st_crs(crs)$proj4string))
	coords <- coordinates(spTransform(gInterpolate(shape_line, d, normalized = FALSE), CRS('+proj=longlat +ellps=WGS84')))
	return(list(coords[, 1], coords[, 2]))
}
