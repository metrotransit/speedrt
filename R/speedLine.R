# This file is part of speedRT
# Copyright (c) 2019 Metropolitan Council
#
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at https://mozilla.org/MPL/2.0/.

#' @importFrom sp spTransform SpatialLines Lines Line CRS coordinates
#' @importFrom rgeos gInterpolate
NULL

#' Interpolate speed along a route shape
#'
#' Requires packages sp and rgeos.
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
	if (!requireNamespace('sp', quietly = TRUE)) stop('interpolateLatLon requires package \'sp\', install the package: install.packages(\'sp\')', .call = FALSE)
	if (!requireNamespace('rgeos', quietly = TRUE)) stop('interpolateLatLon requires package \'rgeos\', install the package: install.packages(\'rgeos\')', .call = FALSE)
	avl_dist_traveled = shape_id = match_lat = NULL # avoid NSE warnings for data.table
  # roll join by start_date, shape_id, trip_id, avl_dist_traveled
  dist_lu <- avl[, list(avl_dist_traveled = seq(by_dist * floor(min(avl_dist_traveled, na.rm = TRUE)/by_dist), by_dist * ceiling(max(avl_dist_traveled, na.rm = TRUE)/by_dist), by = by_dist)), keyby = c('start_date', 'shape_id', 'trip_id')]
  expanded <- avl[dist_lu, on = c('start_date', 'shape_id', 'trip_id', 'avl_dist_traveled'), roll = TRUE][!is.na(match_lat), c('start_date', 'shape_id', 'trip_id', 'avl_dist_traveled', 'match_lat', 'match_lon', 'mps', 'TOD', 'direction_id', 'route_short_name', 'DOW', 'service_name', 'trip_desc')]
  expanded[, c('lon_imp', 'lat_imp') := interpolateLatLon(avl_dist_traveled, shape_id, shapes, crs), by = c('start_date', 'trip_id', 'shape_id')]
  return(expanded)
}

#' Interpolate locations along a shape
#'
#' Requires packages sp and rgeos.
#'
#' @param d distance between interpolated points, in units of \code{crs}
#' @param id valid GTFS shape_id
#' @inheritParams speedOnLine
#'
#' @return a list of vectors of longitude and latitude for each interpolated 
#' point.
#' @export
interpolateLatLon <- function(d, id, shapes, crs) {
	if (!requireNamespace('sp', quietly = TRUE)) stop('interpolateLatLon requires package \'sp\', install the package: install.packages(\'sp\')', .call = FALSE)
	if (!requireNamespace('rgeos', quietly = TRUE)) stop('interpolateLatLon requires package \'rgeos\', install the package: install.packages(\'rgeos\')', .call = FALSE)
	shape_id = NULL # avoid NSE warnings for data.table
	# generate spatial line from shape
	shape_line <- spTransform(SpatialLines(list(Lines(Line(shapes[shape_id == id, c('shape_pt_lon', 'shape_pt_lat')]), ID = id)), CRS('+proj=longlat +ellps=WGS84')), CRS(st_crs(crs)$proj4string))
	coords <- coordinates(spTransform(gInterpolate(shape_line, d, normalized = FALSE), CRS('+proj=longlat +ellps=WGS84')))
	return(list(coords[, 1], coords[, 2]))
}
