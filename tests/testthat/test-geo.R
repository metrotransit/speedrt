context('Geometry/Geography tools')
gtfs <- system.file('extdata', 'gtfs.zip', package = 'speedRT')

test_that("infer correct UTM zones", {
  expect_equal(speedRT:::inferUTM(c(-93.2822, 44.9847)), 32615)
  expect_equal(speedRT:::inferUTM(c(-93.2822, -44.9847)), 32715)
  expect_equal(speedRT:::inferUTM(c(15.6292, 41.3157)), 32633)
  expect_equal(speedRT:::inferUTM(c(-93.2822, 44.9847)), speedRT:::inferUTM(sf::st_point(c(-93.2822, 44.9847))))
})

test_that('converts shapes.txt to linestrings', {
	shape_geo <- speedRT:::shapesToLinestring(gtfs)
	expect_equal(c('shape_id', 'geom'), names(shape_geo))
	expect_equal(shape_geo[, uniqueN(shape_id)], nrow(shape_geo))
	expect_true(inherits(shape_geo, 'data.table'))
	expect_true(inherits(shape_geo$geom, 'sfc_LINESTRING'))
})

test_that('align shapes correctly', {
  target <- data.table(shape_id = c(20009, 20010, 20017, 20018, 20019), from_dist = c(486.1249004, 486.1249004, 10886.1812798, 7964.8241467, 11331.6176890), to_dist = c(1923.0282543, 1923.0282543, 9449.2594608, 6527.9023277, 9894.6958700), stop_order = c(1, 1, 2, 2, 2), key = 'shape_id')
  # infer CRS
  expect_equal(target, alignShapes(gtfs, from_stop = 13337, to_stop = 13320))
  # specify CRS
  expect_equal(target, alignShapes(gtfs, from_stop = 13337, to_stop = 13320, crs = 26915))
  # subset shapes
  expect_equal(target[shape_id %in% c(20009, 20010)], alignShapes(gtfs, from_stop = 13337, to_stop = 13320, shape_ids = c(20009, 20010)))
})
