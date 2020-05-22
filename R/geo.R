# This file is part of speedRT
# Copyright (c) 2019 Metropolitan Council
#
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at https://mozilla.org/MPL/2.0/.

#' @importFrom sf st_crs st_coordinates st_sfc st_linestring
NULL

#' Infer UTM Zone from lon-lat or sf POINT
#'
#' @param x either a long-lat pair \code{c(-93, 45)} or an sf POINT object.
#'
#' @return epsg of best fit UTM zone.
#' @export
inferUTM <- function(x) {
  if (inherits(x, 'sfg')) x <- st_coordinates(x)
  z <- ceiling((x[1] + 180)/6)
  h <- ifelse(x[2] >= 0, '', ' +south')
  st_crs(paste0('+proj=utm +zone=', z, h, ' +ellps=WGS84 +datum=WGS84 +units=m +no_defs ', z, h))$epsg
}


#' Create linestrings from GTFS Shapes
#'
#' @param gtfs location of zipped GTFS feed
#'
#' @return data.table with shape_id and geom columns
shapesToLinestring <- function(gtfs) {
	# avoid NSE warnings
	. <- function() NULL
	shape_pt_lon <- shape_pt_lat <- NULL

	shapes <- readGTFS('shapes', gtfs)
	setkeyv(shapes, c('shape_id', 'shape_pt_sequence'))
	shapes_geom <- shapes[, .(geom = st_sfc(st_linestring(cbind(shape_pt_lon, shape_pt_lat)), crs = 4326)), by = c('shape_id')]
	shapes_geom
}


#' Calculate distance offsets for a set of overlapping shapes
#' 
#' 
#' @param gtfs path to zipped GTFS feed
#' @param from_stop GTFS stop_id of stop that defines start of route segment.
#' @param to_stop GTFS stop_id of stop that defines end of route segment
#' @param crs local projection, default to UTM. Use a projection in meters.
#' @param shape_ids character vector of shape_ids,
#' one for each shape of interest in shapes.txt of GTFS. If NULL, will use all
#' shapes within 30 m of \code{from_stop} and \code{to_stop}.
#' 
#' @return data.table of shape_id, from_dist, to_dist and stop_order 
#' To use, add from_dist to avl_dist_traveled for each shape_id in a matched AVL
#' dataset.
#' @export
#' @examples 
#' gtfs <- system.file('extdata', 'gtfs.zip', package = 'speedRT')
#' shape_offsets <- alignShapes(gtfs, from_stop = 13337, to_stop = 13320)
alignShapes <- function(gtfs, from_stop, to_stop, crs, shape_ids = NULL) {
  shape_id <- shape_pt_lon <- shape_pt_lat <- shape_dist_traveled <- stop_id <- .N <- N <- NULL
  # Load shapes
  shapes <- readGTFS('shapes', gtfs)
  
  # Load stops, stop_times & trips
  stops <- readGTFS('stops', gtfs)
  stop_times <- readGTFS('stop_times', gtfs)
  # check that from and to stop are in stop_times
  if (nrow(stop_times[stop_id %in% c(from_stop, to_stop), .N, keyby = 'trip_id'][N == 2]) == 0) stop('Could not find stop_ids:', from_stop, ', ', to_stop, ' in GTFS.')
  trips <- readGTFS('trips', gtfs)
  
  if (is.null(shape_ids)) shape_ids <- shapes[, unique(shape_id)]
  # # unique shape ids for trips with both stops
  # shape_ids <- trips[stop_times[stop_id %in% c(from_stop, to_stop), .N, keyby = 'trip_id'][N == 2], on = 'trip_id', nomatch = NULL][, unique(shape_id)]
  
  # infer CRS if missing
  if (missing(crs)) crs <- shapes[1, inferUTM(c(shape_pt_lon, shape_pt_lat))]
  
  # check shape lengths vs meters
  shape_geo <- shapesToLinestring(gtfs)
  
  # project shared stop locations onto shapes
  from_pt <- stops[stop_id == from_stop]
  to_pt <- stops[stop_id == to_stop]
    
  shape_dist <- rbindlist(lapply(shape_ids, function(id) {
    # check that the stops are close to the shape, if not, return empty table
    close_enough <- pointsWithin(from_pt$stop_lon, from_pt$stop_lat, id, crs, dist = 30, shape_geo)
    if (!close_enough) return(NULL)
    
    from_dist <- distanceAlongShape(from_pt$stop_lon, from_pt$stop_lat, id, crs, shape_geo)
    to_dist <- distanceAlongShape(to_pt$stop_lon, to_pt$stop_lat, id, crs, shape_geo)
    stop_order <- ifelse(from_dist$avl_dist_traveled < to_dist$avl_dist_traveled, 1, 2)
    return(data.table(shape_id = id, from_dist = from_dist$avl_dist_traveled, to_dist = to_dist$avl_dist_traveled, stop_order = stop_order))
  }))
  setkeyv(shape_dist, 'shape_id')
  
  return(shape_dist)
}
