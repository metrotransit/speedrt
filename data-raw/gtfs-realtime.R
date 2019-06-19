## Prepare minimal vehiclePositions feed ####
devtools::load_all()
feed <- "https://svc.metrotransit.org/mtgtfs/vehicle/VehiclePositions.pb"
path <- file.path(tempdir(), 'vehiclePositions')


output = tempdir()
result <- logProtobufFeed(feed, refresh = 1, duration = 60)
pbs <- list.files(output, pattern = '^VehiclePositions_.+pb$', full.names = TRUE)
names(pbs) = pbs

dt_list = lapply(pbs, readVehiclePosition)
dt = rbindlist(dt_list, idcol = 'filename')
dt[, row := 1:.N, keyby = 'filename']

## filter to Route 2 ####
sgtfs <- system.file('extdata', 'gtfs.zip', package = 'speedRT')
trips <- readGTFS('trips', sgtfs)
ix = unique(dt[trip_id %in% trips$trip_id], by = names(dt_list[[1]]))


proto <- system.file('extdata', 'gtfs-realtime.proto', package = 'speedRT')
FeedMessage <- RProtoBuf::P("transit_realtime.FeedMessage", file = proto)

# write vehicle position messages for each file
lapply(unique(ix$filename), function(fname) {
	msg <- RProtoBuf::read(FeedMessage, fname)

	shortmsg <- RProtoBuf::new(FeedMessage, header = msg$header, entity = msg$entity[ix[filename == fname, row]])
	shortmsg$serialize(file.path('inst/extdata/vehiclePositions/', basename(fname)))
	# readVehiclePosition('inst/extdata/vehiclePositions')
})
