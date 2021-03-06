% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gtfs.R
\name{readGTFS}
\alias{readGTFS}
\title{Read tables directly from a zipped GTFS feed}
\usage{
readGTFS(name = c("agency", "stops", "routes", "trips", "stop_times",
  "calendar", "calendar_dates", "fare_attributes", "fare_rules", "shapes",
  "frequencies", "transfers", "feed_info"), gtfs)
}
\arguments{
\item{name}{name of the table to read, without ".txt" extension.
Current tables include 'agency', 'stops', 'routes', 'trips',
'stop_times', 'calendar', 'calendar_dates', 'fare_attributes', 
'fare_rules', 'shapes', 'frequencies', 'transfers', 'feed_info'.
See the \href{https://gtfs.org/reference/static/}{GTFS Static Reference} for a full list.}

\item{gtfs}{location of zipped GTFS feed}
}
\value{
data.table of GTFS table
}
\description{
Read tables directly from a zipped GTFS feed
}
\examples{
gtfs <- system.file('extdata', 'gtfs.zip', package = 'speedRT')
stop_times <- readGTFS('stop_times', gtfs)
}
