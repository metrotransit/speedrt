context('GTFS read/write')

gtfs <- system.file('extdata', 'gtfs.zip', package = 'speedRT')
tmpgtfs <- file.path(tempdir(), 'gtfs_test.zip')

test_that('Read known files', {
	stop_times <- readGTFS('stop_times', gtfs)
	expect('data.table' %in% class(stop_times), 'readGTFS did not return a data.table')
	expect(all(c('trip_id', 'stop_id', 'stop_sequence') %in% names(stop_times)), 'Require fields missing from stop_times')
})

test_that('Write known files', {
	agency <- readGTFS('agency', gtfs)
	expect_equal(writeGTFS(agency, 'agency', tmpgtfs), 0)
	expect_equal(agency, readGTFS('agency', tmpgtfs))
})
	
test_that('Fail on invalid filename', {
	expect_error(readGTFS('timepoints', gtfs))
	expect_error(writeGTFS(0L, 'timepoints', tmpgtfs))
})

test_that('Bad path errors', {
	expect_error(writeGTFS(data.table(a = 0), 'agency', 'nonsensepath/to/nowhere'))
})

## Test realtime related functions ####
context('GTFS-realtime')
vp <- system.file('extdata', 'vehiclePositions', package = 'speedRT')
feed <- "https://svc.metrotransit.org/mtgtfs/vehicle/VehiclePositions.pb"

test_that('Read sample vehiclePositions', {
	dt <- readVehiclePosition(vp)
	expect_equal(nrow(dt), 11)
	expect_equal(names(dt), c("trip_id", "start_date", "route_id", "latitude", "longitude", "bearing", "current_stop_sequence", "current_status", "timestamp", "vehicle_id"))
})

test_that('Vehicle Positions log', {
output <- tempdir()
expect_equal(result <- logVehiclePositions(feed, refresh = 1, duration = 2.9), output)
pbs <- list.files(output, pattern = '^vehiclePosition_', full.names = TRUE)
expect_gte(length(pbs), 2)
unlink(pbs)
})

test_that('Service lookup', {
  service_lu <- lookupService(gtfs)
  expect_identical(service_lu, data.table(service_id = 'MAR19-MVS-BUS-Weekday-01', service_name = 'Weekday', key = 'service_id'))
})
