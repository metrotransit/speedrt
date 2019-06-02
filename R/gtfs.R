#' Read tables directly from a zipped GTFS feed
#'
#' @param name name of the table to read, without ".txt" extension.
#' Current tables include 'agency', 'stops', 'routes', 'trips',
#' 'stop_times', 'calendar', 'calendar_dates', 'fare_attributes', 
#' 'fare_rules', 'shapes', 'frequencies', 'transfers', 'feed_info'.
#' See the \href{https://gtfs.org/reference/static/}{GTFS Static Reference} for a full list.
#' @param gtfs location of zipped GTFS feed
#'
#' @return data.table of GTFS table
#' @export
#'
#' @examples
#' gtfs <- system.file('extdata', 'gtfs.zip', package = 'speedRT')
#' stop_times <- readGTFS('stop_times', gtfs)
readGTFS <- function(name = c('agency', 'stops', 'routes', 'trips', 'stop_times', 'calendar', 'calendar_dates', 'fare_attributes', 'fare_rules', 'shapes', 'frequencies', 'transfers', 'feed_info'), gtfs) {
  name <- match.arg(name, choices = c('agency', 'stops', 'routes', 'trips', 'stop_times', 'calendar', 'calendar_dates', 'fare_attributes', 'fare_rules', 'shapes', 'frequencies', 'transfers', 'feed_info'))
  table_name <- paste0(name, '.txt')
  table <- fread(cmd = paste("unzip -cq", shQuote(path.expand(gtfs)), table_name))
  return(table)
}

#' Write tables to a GTFS feed
#'
#' @param x data.table to write
#' @inheritParams readGTFS
#'
#' @return 0 if successful
#' @export
#'
#' @examples
#' gtfs <- system.file('extdata', 'gtfs.zip', package = 'speedRT')
#' routes <- readGTFS('routes', gtfs)
#' routes[1, `:=` (route_short_name = 'Metro 2')]
#' writeGTFS(routes, 'routes', file.path(tempdir(), 'gtfs_test.zip'))
writeGTFS <- function(x, name = c('agency', 'stops', 'routes', 'trips', 'stop_times', 'calendar', 'calendar_dates', 'fare_attributes', 'fare_rules', 'shapes', 'frequencies', 'transfers', 'feed_info'), gtfs) {
  name <- match.arg(name, choices = c('agency', 'stops', 'routes', 'trips', 'stop_times', 'calendar', 'calendar_dates', 'fare_attributes', 'fare_rules', 'shapes', 'frequencies', 'transfers', 'feed_info'))
  table_name <- paste0(name, '.txt')
  fpath <- file.path(tempdir(), table_name)
  on.exit(unlink(fpath), add = TRUE)
  # write to tempdir
  fwrite(x, fpath)

  # add to GTFS zip archive
  ret <- system2('zip', paste('-j9X', shQuote(path.expand(gtfs)), fpath))
  if(ret != 0) stop('There was a problem writing to the GTFS feed, please check
paths')
  ret
}


#' Infer service names from GTFS calendar
#'
#' @param gtfs path to the zipped GTFS feed.
#'
#' @return data.table linking service_id to a descriptive service_name
#' @export
#'
#' @examples
#' gtfs <- system.file('extdata', 'gtfs.zip', package = 'speedRT')
#' service_lu <- lookupService(gtfs)
lookupService <- function(gtfs) {
  service_id <- monday <- tuesday <- wednesday <- thursday <- friday <- saturday <- sunday <- service_name <- NULL
  calendar <- suppressWarnings(readGTFS('calendar', gtfs))
  # if no calendar file, return service_id as service_name
  if (nrow(calendar) == 0) {
    # check class of service_id
    calendar_dates <- readGTFS('calendar_dates', gtfs)
    return(calendar_dates[, list(service_name = as.character(service_id)), keyby = 'service_id'])
  }
  
  # for each service_id, attempt to describe combination of weekdays
  # MTWThF = Weekday, Sat, Sun, Sat + Sun = Weekend, MTWTh, MWF &c.
  # None = 'other'
  service_lu <- calendar[, list(service_name = paste0(c('M', 'T', 'W', 'Th', 'F', 'Sa', 'Su')[as.logical(c(monday, tuesday, wednesday, thursday, friday, saturday, sunday))], collapse = '')), by = service_id]
  # translate
  service_lu[service_name == 'MTWThF', service_name := 'Weekday']
  service_lu[service_name == 'Sa', service_name := 'Saturday']
  service_lu[service_name == 'Su', service_name := 'Sunday']
  service_lu[service_name == 'SaSu', service_name := 'Weekend']
  service_lu[service_name == '', service_name := 'Other']
  setkeyv(service_lu, 'service_id')
  return(service_lu)
}
