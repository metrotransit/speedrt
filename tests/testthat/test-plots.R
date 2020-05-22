context('Plotting functions')

## Build dataset for plotting ####
avl = rbindlist(lapply(list.files(system.file('extdata', 'vehiclePositions', package = 'speedRT'), full.names = TRUE), readVehiclePosition))
shapes = readGTFS('shapes', system.file('extdata', 'gtfs.zip', package = 'speedRT'))
matched = matchAVL(avl, gtfs = system.file('extdata', 'gtfs.zip', package = 'speedRT'))
filtered = filterMatches(matched)
expanded = speedOnLine(filtered, shapes = shapes, crs = 26915)

# 
test_that('SpeedHistogram', {
	p = plotSpeedHistogram(filtered)
	expect(inherits(p, c('gg', 'ggplot')), 'Plot object not a valid ggplot object')
	p = plotSpeedHistogram(filtered, compare = 'TOD')
	expect(inherits(p, c('gg', 'ggplot')), 'Plot object not a valid ggplot object')
})
test_that('SpeedDist', {
	p = plotSpeedDist(expanded)
	expect(inherits(p, c('gg', 'ggplot')), 'Plot object not a valid ggplot object')
	p = plotSpeedDist(expanded, compare = 'TOD')
	expect(inherits(p, c('gg', 'ggplot')), 'Plot object not a valid ggplot object')
	p = plotSpeedDist(expanded, compare = 'TOD', alpha = 0.2)
	expect(inherits(p, c('gg', 'ggplot')), 'Plot object not a valid ggplot object')
})
test_that('SpeedToTT', {
	p = speedToTT(filtered)
	expect(names(p) == 'All', 'speedToTT failed to return a named list of ggplot objects for trivial compare value')
	expect(length(p) == 1, "Result is not a list of length 1")
	expect(inherits(p[[1]], c('gg', 'ggplot')), 'Plot object not a valid ggplot object')
	p = speedToTT(expanded, compare = 'TOD')
	expect(names(p) == 'Midday', 'speedToTT failed to return a list of ggplot objects for non-trivial compare value')
	expect(all(sapply(p, inherits, c('gg', 'ggplot'))), 'All objects in list are not valid ggplot objects')
})
