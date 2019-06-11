# This file is part of speedRT
# Copyright (c) 2019 Metropolitan Council
# 
# This Source Code Form is subject to the terms of the Mozilla Public License, 
# v. 2.0. If a copy of the MPL was not distributed with this file, 
# You can obtain one at https://mozilla.org/MPL/2.0/.

#' Read VehiclePosition messages from GTFS-realtime ProtoBuf file
#'
#' @param descriptor location of protobuf file or raw vector.
#' @return a data.table with fields: trip_id, start_date, route_id, lat, lon, 
#'   bearing, stop_sequence, status, timestamp, vehicle_id
#' @export
readVehiclePosition <- function(descriptor) {	
	if(!requireNamespace("RProtoBuf", quietly = TRUE)) {
		stop('Install RProtoBuf to use readVehiclePositions')
	}
	
	# Parse GTFS-realtime pbf
	proto <- system.file('extdata', 'gtfs-realtime.proto', package = 'speedRT')
	FeedMessage <- RProtoBuf::P("transit_realtime.FeedMessage", file = proto)
	msg <- RProtoBuf::read(FeedMessage, descriptor)
	
	# Flatten and return data.table
	rbindlist(lapply(msg$entity, function(x) { 
		data.table(trip_id = x$vehicle$trip$trip_id, start_date = x$vehicle$trip$start_date, route_id = x$vehicle$trip$route_id, latitude = x$vehicle$position$latitude, longitude = x$vehicle$position$longitude, bearing = x$vehicle$position$bearing, current_stop_sequence = x$vehicle$current_stop_sequence, current_status = x$vehicle$current_status, timestamp = structure(x$vehicle$timestamp, class = c('POSIXct', 'POSIXt')), vehicle_id = x$vehicle$vehicle$id)
	}))
}

#' Log Protobuf Feeds including GTFS-RT Vehicle Positions
#' @param feed URL of protobuf file.
#' @param refresh minimum time in seconds between queries of the protobuf feed.
#' @param duration total time in seconds to query feed.
#' @param output location to save files, if \code{NULL} will save to a tempfile.
#'
#' @return path to saved protobuf files
#' @export
logProtobufFeed <- function(feed, refresh = 10L, duration = 10L, output = NULL) {
	# check for feed
	if (missing(feed)) stop('Parameter "feed" is missing. Please provide a valid GTFS-Realtime URL')
	
	if(!requireNamespace("curl", quietly = TRUE)) {
		stop('Install curl to use logProtoFeed')
	}
	
	if (is.null(output)) output <- tempdir()
	tic <- Sys.time()
	pb_type = basename(feed)
	while(as.numeric(as.difftime(Sys.time() - tic), units = 'secs') < duration) {
		path <- fetch_feed(feed, output = file.path(output, gsub('\\.', paste0(strftime(Sys.time(), format = '_%Y%m%d%H%M%S.')), pb_type)))
		Sys.sleep(refresh)
	}
	output
}

#' Download protobuf files
#' @param feed URL for protobuf file.
#' @param output path for download (directory).
#' @return response content
fetch_feed <- function(feed, output) {
# Load feed and check status
	resp <- curl::curl_fetch_disk(feed, output)
	if (resp$status_code != 200) {
		stop(gettextf('Attempt to download protobuf feed %s failed with error: HTTP Status %s', feed, resp$status_code))
	}
	
	resp$content
}
