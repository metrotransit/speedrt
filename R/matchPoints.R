# This file is part of speedRT
# Copyright (c) 2019 Metropolitan Council
#
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at https://mozilla.org/MPL/2.0/.

#' @importFrom sp CRS spTransform SpatialPoints
#' @importFrom rgeos gProject
#' @importFrom sf as_Spatial st_transform st_sfc st_point st_buffer st_intersects st_length
NULL

#' Match AVL positions to route shapes
#' 
#' @param avl a data.table. Required fields: trip_id, start_date, latitude, 
#'   longitude, timestamp, vehicle_id. 
#'   Other fields are currently ignored, though bearing and odometer could be 
#'   used eventually.
#' @param gtfs path to a zipped GTFS feed with identifier that match \code{avl}.
#' @param crs local projection, default to UTM. Use a projection in meters.
#' @param tz timezone of observations
#' @param within_dist distance in units of CRS (typically meters) from shape. 
#'   Vehicle positions outside of distance will be dropped from the result.
#' @param time_lu data.table keyed on "Time". See \code{\link{time_lu}}.
#' @param service_lu a keyed data.table.
#'  Use \code{\link{lookupService}} to create the table.
#' 
#' @return data.table with trip_id, route_id, latitude, longitude, timestamp, 
#'   vehicle_id, start_date, match_lat, match_lon, and avl_dist_traveled.
#'   The data.table will also include any additional fields in \code{avl}.
#'
#' @export
#' @examples
#' \dontrun{
#' avl <- readVehiclePosition(system.file('extdata', 'vehiclePositions', package = 'speedRT'))	
#' matched <- matchAVL(avl, gtfs = system.file('extdata', 'gtfs.zip', package = 'speedRT'))
#' }
matchAVL <- function(avl, gtfs, crs = inferUTM(avl[1, c(longitude, latitude)]), 
                     tz = Sys.timezone(), within_dist = 30, time_lu = speedRT::time_lu, service_lu = lookupService(gtfs)) {
	# avoid warnings from data.table NSE
	shape_id <- geom <- timestamp <- trip_id <- longitude <- latitude <- avl_dist_traveled <- i.service_name <- i.route_short_name <- route_short_name <- trip_headsign <- start_date <- NULL
	trips <- readGTFS('trips', gtfs)
	routes = readGTFS('routes', gtfs)
	# convert trip_id to character to match GTFS-realtime spec ("string")
	trips[, `:=` (trip_id = as.character(trip_id))]
	avl_names <- union(c('start_date', 'trip_id', 'vehicle_id', 'timestamp', 'latitude', 'longitude', 'shape_id', 'route_short_name', 'service_id', 'direction_id', 'trip_headsign', 'trip_desc', 'service_name', 'TOD', 'DOW', 'date_range', 'Time', 'match_lat', 'match_lon', 'avl_dist_traveled'), names(avl))
	
	# fill missing start_date
	if (!'start_date' %in% names(avl)) avl[, `:=` (start_date = as.integer(strftime(as.Date(structure(first(timestamp), class = c('POSIXct', 'POSIXt'), tz = tz), tz = tz), format = '%Y%m%d'))), by = trip_id]
	
	avl_trips <- trips[avl, on = 'trip_id', nomatch = NULL]
	
	# filter out outliers
	shape_geo <- shapesToLinestring(gtfs)
	
	# mark points close enough to shape
	avl_trips[, `:=` (inrange = pointsWithin(longitude, latitude, first(shape_id), crs, within_dist, shape_geo)), by = 'shape_id']
	
	# project points onto shape
	avl_trips[(inrange), c('avl_dist_traveled', 'match_lat', 'match_lon') := distanceAlongShape(longitude, latitude, first(shape_id), crs, shape_geo), by = 'shape_id']
	
	# add metadata from GTFS
	avl_trips[routes, on = 'route_id', `:=` (route_short_name = i.route_short_name)]
	avl_trips[, `:=` (trip_desc = paste(route_short_name, trip_headsign, sep = ' - ')), by = 'shape_id']
	avl_trips[, `:=` (Time = as.ITime(structure(timestamp, class = c('POSIXct', 'POSIXt'), tz = tz), tz = tz))]
	avl_trips = time_lu[avl_trips, on = 'Time', roll = TRUE]
	avl_trips[, `:=` (DOW = weekdays(as.Date(as.character(start_date), format = '%Y%m%d')))]
	avl_trips[, `:=` (date_range = paste(range(start_date, na.rm = TRUE), collapse = '\u2013'))]
	avl_trips[service_lu, on = 'service_id', `:=` (service_name = i.service_name)]

	avl_trips[(inrange), avl_names, with = FALSE]
}

#' Are points within a distance of a shape?
#' 
#' @param lon longitude WGS84 longlat
#' @param lat latitude WGS84 longlat
#' @param shape_id. GTFS shape identifier
#' @param srid local projection
#' @param dist distance in units of local projects
#' @param shape_geo data.table with shape_id and sfc_LINESTRING geom columns
#' 
#' @return logical same length as lon, indicating if points defined by lon, lat 
#'    are within dist of shape defined by shape_id.
pointsWithin <- function(lon, lat, shape_id., srid, dist = 30, shape_geo) {
	shape_id <- geom <- NULL
	shape <- shape_geo[shape_id == shape_id., geom]
	points <- st_transform(st_sfc(lapply(seq_len(length(lon)), function(ix) st_point(c(lon[ix], lat[ix]))), crs = 4326), srid)
	shape_buf <- st_buffer(st_transform(shape, srid), dist)
	apply(st_intersects(points, shape_buf, sparse = FALSE), 1, any)
}

#' How far along a shape linestring is a point?
#' 
#' @param lon longitude WGS84 longlat
#' @param lat latitude WGS84 longlat
#' @param shape_id. GTFS shape identifier
#' @param srid local projection
#' @param shape_geo data.table with shape_id and sfc_LINESTRING geom columns
#' 
#' @return list of distance along shape, latitude and longitude of matched points
distanceAlongShape <- function(lon, lat, shape_id., srid, shape_geo) {
	shape_id <- geom <- NULL
	points <- SpatialPoints(cbind(lon, lat), proj4string = CRS(st_crs(4326)$proj4string))
	points_sp <- spTransform(points, CRS(st_crs(srid)$proj4string))
	shape <- shape_geo[shape_id == shape_id., geom]
	segment <- st_transform(shape, srid)
	segment_length <- as.numeric(st_length(segment))
	segment_sp <- as_Spatial(segment)
	shape_dist_traveled <- gProject(segment_sp, points_sp)
	matched_points <- spTransform(rgeos::gInterpolate(segment_sp, shape_dist_traveled), CRS("+proj=longlat +datum=WGS84 +no_defs"))
	coords <- sp::coordinates(matched_points)
	bad_matches <- shape_dist_traveled < 0 | segment_length < shape_dist_traveled
	shape_dist_traveled[bad_matches] <- NA_real_
	coords[bad_matches,] <- NA_real_
	return(list(avl_dist_traveled = shape_dist_traveled, match_lat = coords[, 2], match_lon = coords[, 1]))
}

#' Filter matched locations so points travel forward along shape
#' 
#' Removes vehicle positions that result in negative distance traveled between
#' observations. 
#' When "odometer" values are available, also excludes positions with odometer
#' values that decrease for increasing timestamp values.
#'
#' @param avl_matches vehicle positions matched to route shapes.
#'   See \code{\link{matchAVL}} for matching.
#' @param max_speed maximum expected speed (in units of projection per second,
#' typically m/s). 
#' Observations over max_speed will be dropped and speeds recalculated.
#'
#' @return data.table of filtered values in avl_matches, with dist_to_next and
#'   mps (meters per second) columns.
#' @export
#'
#' @examples
#' \dontrun{
#' avl <- readVehiclePosition(system.file('extdata', 'vehiclePositions', package = 'speedRT'))
#' matched <- matchAVL(avl, gtfs = system.file('extdata', 'gtfs.zip', package = 'speedRT'))
#' filtered <- filterMatches(matched)
#' }
filterMatches <- function(avl_matches, max_speed = Inf) {
  avl_dist_traveled <- timestamp <- dist_to_next <- mps <- odometer <- bad_odo <- NULL
  setkeyv(avl_matches, c('start_date', 'trip_id', 'vehicle_id', 'timestamp'))
  
  # flag out of order odometer
  if ('odometer' %in% names(avl_matches)) {
    avl_matches[, `:=` (bad_odo = c(diff(odometer), 0) < 0), by = c('start_date', 'trip_id', 'vehicle_id')]
  } else {
    avl_matches[, `:=` (bad_odo = FALSE)]
  }
  
	# AVL distance
	avl_matches[!(bad_odo), `:=` (dist_to_next = c(diff(avl_dist_traveled), 0)), by = c('start_date', 'trip_id', 'vehicle_id')]

  # remove rows with negative distance traveled, until only positive distances
	filtered <- avl_matches[!(bad_odo)]
	setkeyv(filtered, c('start_date', 'trip_id', 'vehicle_id', 'timestamp'))
	filtered[!is.na(avl_dist_traveled), `:=` (mps = c(NA_real_, diff(avl_dist_traveled)/as.numeric(diff(timestamp), units = 'secs'))), by = c('start_date', 'trip_id', 'vehicle_id')]
  while (TRUE) {
    n <- nrow(filtered)
    filtered <- filtered[0 <= dist_to_next & (is.na(mps) | mps < max_speed)]
    filtered[, `:=` (dist_to_next = c(diff(avl_dist_traveled), 0)), by = c('start_date', 'trip_id', 'vehicle_id')]
    filtered[!is.na(avl_dist_traveled), `:=` (mps = c(NA_real_, diff(avl_dist_traveled)/as.numeric(diff(timestamp), units = 'secs'))), by = c('start_date', 'trip_id', 'vehicle_id')]
    if (n == nrow(filtered)) break
  }

	# # mark contiguous positive distance runs
	# setkeyv(avl_trips, c('start_date', 'trip_id', 'vehicle_id', 'timestamp'))
	# avl_trips[, `:=` (G = cumsum(dist_to_next < 0 | shift(dist_to_next, 1, 0, 'lag') < 0)), by = c('start_date', 'trip_id', 'vehicle_id')]
	# avl_trips[, .N, by = c('start_date', 'trip_id', 'vehicle_id', 'G')]

	# drop bad_odo variable
  filtered[, 'bad_odo' := NULL]
	# replace NaN (divide by 0) speed with 0
	filtered[is.nan(mps), mps := 0]
	filtered[is.finite(mps)]
}
