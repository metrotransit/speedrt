libs <- c('speedRT', 'data.table', 'DT', 'd3heatmap', 'ggplot2', 'ggmap', 'leaflet', 'rgeos', 'rgdal', 'shiny', 'shinydashboard', 'sp', 'stringi', 'zoo', 'sf')

suppressMessages(invisible(lapply(libs, library, character.only = TRUE)))

options(shiny.maxRequestSize = 100 * 1024^2)

## Data ####
dow_scale <- c('Sunday' = 'gray30', "Monday" = "#0053A0", "Tuesday" = "#ED1B2E", "Wednesday" = "#FFD200", "Thursday" = "#008144", "Friday" = "#F68A1E", 'Saturday' = 'grey60')
tod_scale <- c("Early" = "#0053A0", "AM Peak" = "#ED1B2E", "Midday" = "#FFD200", "PM Peak" = "#008144", "Evening" = "#F68A1E", "Owl" = "gray")


## Plotting functions ####
plotSpeedHistogram <- function(avl, compare) {
	limits <- avl[, quantile(mps, c(0.01, 0.99), na.rm = TRUE)]
	temp_points <- avl[limits[1] <= mps & mps <= limits[2] & is.finite(mps)]
	if (compare == "shape_id") {
		low <- quantile(temp_points$mps, 0.05, na.rm = TRUE)
		med <- median(temp_points$mps, na.rm = TRUE)
		high <- quantile(temp_points$mps, 0.95, na.rm = TRUE)
		ggplot(data = temp_points) + geom_density(aes(x = mps), fill = "#0053A0", color = "#0053A0", alpha = 0.1) + geom_vline(xintercept = c(med, low, high), linetype = 2) + geom_label(aes(x = med, y = 0, label = c("median"))) + geom_label(aes(x = low, y = 0, label = c("5th"))) + geom_label(aes(x = high, y = 0, label = c("95th"))) + labs(x = "Estimated Speed (m/s)", y = "Count", title = paste0(round(med, 2), " [", round(low, 2), ", ", round(high, 2), "] m/s")) + theme_bw()
	} else if (compare == "TOD") {
		tt <- temp_points[, .(med = median(mps, na.rm = TRUE)), keyby = TOD]
		ggplot(data = temp_points) + geom_density(aes(x = mps, group = TOD, color = TOD, fill = TOD), alpha = 0.1) + geom_vline(aes(xintercept = med, color = TOD), data = tt, linetype = 2) + labs(x = "Estimated Speed (m/s)", y = "Count", title = paste(paste0(tt$TOD, ": ", round(tt$med, 1)), collapse = ", ")) + theme_bw() + scale_fill_manual(values = tod_scale) + scale_color_manual(values = tod_scale)
	} else if (compare == "DOW") {
		tt <- temp_points[, .(med = median(mps, na.rm = TRUE)), keyby = DOW]
		ggplot(data = temp_points) + geom_density(aes(x = mps, group = DOW, color = DOW, fill = DOW), alpha = 0.1) + geom_vline(aes(xintercept = med, color = DOW), linetype = 2, data = tt) + labs(x = "Estimated Speed (m/s)", y = "Count", title = paste(paste0(tt$DOW, ": ", round(tt$med, 1)), collapse = ", ")) + theme_bw() + scale_fill_manual(values = dow_scale) + scale_color_manual(values = dow_scale)
	} else if (compare == "between Date Ranges") {
		tt <- temp_points[, .(med = median(mps, na.rm = TRUE)), keyby = DR]
		ggplot(data = temp_points) + geom_density(aes(x = mps, group = DR, color = DR, fill = DR), alpha = 0.1) + geom_vline(aes(xintercept = med, color = DR), data = tt, linetype = 2) + labs(x = "Estimated Speed (m/s)", y = "Count", title = paste(paste0(tt$DR, ": ", round(tt$med, 1)), collapse = ", ")) + theme_bw() + scale_fill_manual(values = c(First = "#0053A0", Second = "#ED1B2E")) + scale_color_manual(values = c(First = "#0053A0", Second = "#ED1B2E"))
	} else {
		tt <- temp_points[, .(med = median(mps, na.rm = TRUE)), by = route_short_name]
		cols <- c("#0053A0", "#ED1B2E", "#FFD200", "#008144", "#F68A1E",
			"#00cdcd, #551a8b", "#000000", "#ff8197", "#7c2020")[1:nrow(tt)]
		ggplot() + geom_density(data = temp_points, aes(x = mps, group = route_short_name, color = route_short_name, fill = route_short_name), alpha = 0.1) + geom_vline(data = tt, aes(xintercept = med, color = route_short_name), linetype = 2) + labs(x = "Estimated Speed (m/s)", y = "Count", title = paste(paste0(tt$route_short_name, ": ", round(tt$med, 1)), collapse = ", ")) + theme_bw() + scale_fill_manual(values = cols) + scale_color_manual(values = cols)
	}
}

## Plot speed lines ####
interpolateLatLon <- function(d, id, shapes, crs) {
	# generate spatial line from shape
	shape_line <- spTransform(SpatialLines(list(Lines(Line(shapes[shape_id == id, c('shape_pt_lon', 'shape_pt_lat')]), ID = id)), CRS('+proj=longlat +ellps=WGS84')), CRS(st_crs(crs)$proj4string))
	coords <- coordinates(spTransform(gInterpolate(shape_line, d, normalized = FALSE), CRS('+proj=longlat +ellps=WGS84')))
	return(list(coords[, 1], coords[, 2]))
}

speedOnLine <- function(avl, by_dist = 1, shapes, crs) {
  # roll join by start_date, shape_id, trip_id, avl_dist_traveled
  dist_lu <- avl[, .(avl_dist_traveled = seq(by_dist * floor(min(avl_dist_traveled, na.rm = TRUE)/by_dist), by_dist * ceiling(max(avl_dist_traveled, na.rm = TRUE)/by_dist), by = by_dist)), keyby = c('start_date', 'shape_id', 'trip_id')]
  expanded <- avl[dist_lu, on = c('start_date', 'shape_id', 'trip_id', 'avl_dist_traveled'), roll = TRUE][, c('start_date', 'shape_id', 'trip_id', 'avl_dist_traveled', 'match_lat', 'match_lon', 'mps', 'TOD', 'direction_id', 'route_short_name', 'DOW', 'service_name', 'trip_desc')]
  expanded[, c('lon_imp', 'lat_imp') := interpolateLatLon(avl_dist_traveled, shape_id, shapes, crs), by = c('start_date', 'trip_id', 'shape_id')]
  return(expanded)
  }

## Speed by distance ####
plotSpeedDist <- function(speed, ci = 0, compare = 'None') {
	ci <- as.numeric(ci)
	# TODO: add filtering
	sum_tab <- speed[!is.na(mps), as.list(quantile(mps, c(ci, 0.5, 1 - ci))), keyby = c('shape_id', 'avl_dist_traveled')]
	setnames(sum_tab, 3:5, c('low', 'Median', 'high'))
	ptitle <- "Speed by Distance - Median"
	ribbon <- NULL
	pos <- ifelse(compare == 'None', 'none', 'bottom')
	col_scale <- switch(compare,
			'TOD' = tod_scale,
			'DOW' = dow_scale,
			'between Date Ranges' = c("First" = "#0053A0", "Second" = "#ED1B2E"),
			'route_short_name' = c("#0053A0", "#ED1B2E", "#FFD200", "#008144", "#F68A1E", "#00cdcd", "#551a8b", "#000000", "#ff8197", "#7c2020")[seq_len(sum_tab[, uniqueN(compare)])],
			'None' = '#0053A0')

	if (ci > 0) {
		ptitle <- paste0(ptitle, " and ", paste0(100 * (1 - 2 * as.numeric(ci)), '%'), " Confidence Interval")
		ribbon <- geom_ribbon(aes(x = avl_dist_traveled, ymin = low, ymax = high, group = compare), alpha = 0.2, fill = "#0053A0")
	}

	p <- ggplot(data = sum_tab) +
		geom_line(aes(x = avl_dist_traveled, y = Median, color = compare), size = 1) +
		theme_bw() + 
		labs(x = "Distance along segment in meters", y = "Speed (m/s)", title = ptitle) +
		theme(legend.position = pos) + 
		scale_color_manual('', values = col_scale) + 
		ribbon + 
		facet_grid(rows = vars(shape_id))
	p
}

## Cumulative travel time by distance plots ####
speedToTT <- function(speed, compare = 'None') {
	if (compare == 'None') compare <- NULL
	speed[, tt := (timestamp - min(timestamp))/60, keyby = c('start_date', 'trip_id', 'shape_id', 'trip_desc', compare)]
	# speed <- speed[tt < quantile(tt, .99) & tt >= 0]

	if (is.null(compare)) {
		speed_split <- list(All = speed)
	} else {
		speed_split <- split(speed, speed[[compare]])
	}

	slots <- names(speed_split)[!sapply(speed_split, function(x) nrow(x) == 0)]
	result <- lapply(slots, function(x) {
		p <- ggplot(data = speed_split[[x]]) + 
		geom_line(aes(x = avl_dist_traveled, y = tt, group = interaction(start_date, trip_id), color = trip_desc)) +
		theme_bw() +
		scale_color_discrete('Route') + 
		labs(x = "Distance along segment in meters", y = "Time traveled since the start of the segment in minutes", title = paste("Time Traveled by Distance -", x))
		p
	})
	names(result) <- slots
	return(result)
}
