## Prepare reduced GTFS feed ####
devtools::load_all()
gtfs <- file.path(tempdir(), 'gtfs.zip')
download.file('ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_transit_schedule_google_fd/csv_trans_transit_schedule_google_fd.zip', gtfs)
sgtfs <- 'inst/extdata/gtfs.zip'

calendar <- readGTFS('calendar', gtfs)
srv <- calendar[grepl('MVS-BUS', service_id, fixed = TRUE) & monday == 1][1]$service_id

# load and subset from master GTFS
routes <- readGTFS('routes', gtfs)
route2 <- routes[route_short_name == 2]
agency <- readGTFS('agency', gtfs)
trips <- readGTFS('trips', gtfs)
trips2 <- trips[route_id == route2$route_id & service_id == srv]
stop_times <- readGTFS('stop_times', gtfs)
stop_times2 <- stop_times[trips2[, .(trip_id)], on = 'trip_id']
stops <- readGTFS('stops', gtfs)
stops2 <- stops[stop_times2[, .(stop_id = unique(stop_id))], on = 'stop_id']
shapes <- readGTFS('shapes', gtfs)

# write short GTFS
writeGTFS(agency[agency_id == route2$agency_id], 'agency', sgtfs)
writeGTFS(calendar[service_id == srv], 'calendar', sgtfs)
writeGTFS(route2, 'routes', sgtfs)
writeGTFS(shapes[trips2[, .(shape_id)], on = 'shape_id'], 'shapes', sgtfs)
writeGTFS(stop_times2, 'stop_times', sgtfs)
writeGTFS(stops2, 'stops', sgtfs)
writeGTFS(trips2, 'trips', sgtfs)
