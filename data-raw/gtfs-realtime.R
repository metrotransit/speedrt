## Prepare minimal vehiclePositions feed ####
devtools::load_all()
feed <- "vehiclePositions"
path <- file.path(tempdir(), 'vehiclePositions')
trips <- readGTFS('trips', system.file('extdata', 'gtfs.zip', package = 'speedRT'))

resp <- curl::curl_fetch_disk(feed, path = path)
dt <- readVehiclePosition(path)
ix <- which(dt$trip_id %in% trips$trip_id)
proto <- system.file('extdata', 'gtfs-realtime.proto', package = 'speedRT')
FeedMessage <- RProtoBuf::P("transit_realtime.FeedMessage", file = proto)
msg <- RProtoBuf::read(FeedMessage, resp$content)

shortmsg <- RProtoBuf::new(FeedMessage, header = msg$header, entity = msg$entity[ix])
shortmsg$serialize('inst/extdata/vehiclePositions')
# readVehiclePosition('inst/extdata/vehiclePositions')
