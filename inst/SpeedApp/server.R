shinyServer(function(input, output, session) {
  # if (Sys.getenv('R_ZIPCMD') == '') Sys.setenv(R_ZIPCMD = "/usr/bin/zip")
  rv <- reactiveValues(stops = NULL, trips = NULL, stop_times = NULL, route_stops = NULL, routes = NULL, shapes = NULL, route_names = NULL, sch = NULL, vp = NULL, time_lu = NULL, rd_choices = NULL, crs = NULL)
	matched <- reactiveVal(NULL)
	error_msg <- reactiveVal(NULL)
	speed <- reactiveVal(NULL)
  
  ## Update upload fail message
	observeEvent(error_msg(), {
		showModal(modalDialog(title = "Error:", p(error_msg()), easyClose = TRUE, fade = FALSE), session)
	})

  ## On GTFS Load, update reactive values ####
  observeEvent(input$gtfs_file, {
  	gtfs_file <- input$gtfs_file
  	if (is.null(gtfs_file$datapath)) return()
  	
	rv$gtfspath <- gtfs_file$datapath

  	# read GTFS tables
  	rv$stops <- readGTFS('stops', gtfs_file$datapath)
  	rv$trips <- readGTFS('trips', gtfs_file$datapath)
  	rv$stop_times <- readGTFS('stop_times', gtfs_file$datapath)
  	rv$routes <- readGTFS('routes', gtfs_file$datapath)
	rv$shapes <- readGTFS('shapes', gtfs_file$datapath)
  	rv$route_names <- stringi::stri_sort(unique(rv$routes$route_short_name), opts_collator = stringi::stri_opts_collator(numeric = TRUE))
  	rv$sch <- rv$stops[rv$routes[rv$trips[rv$stop_times, on = 'trip_id'], on = 'route_id'], on = 'stop_id']
  	rv$stops_sf <- st_as_sf(rv$stops, coords = c('stop_lon', 'stop_lat'), crs = 4326)
  	  	
  	## Update UI
  	updateSelectInput(session, 'input_rt', choices = c("Select one" = "", rv$route_names))
  	
  	## initiate map
  	bounds <- rv$stops[, c(range(stop_lon), range(stop_lat))]
  	output$inputmap <- renderLeaflet({
  		leaflet(options = leafletOptions(zoomControl = FALSE)) %>% htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'bottomleft'}).addTo(this)}") %>% addProviderTiles('Stamen.TonerLite') %>% fitBounds(bounds[1], bounds[3], bounds[2], bounds[4])
  	})
  	
  })
  
  ## Select stops from map ####
  # Update route stops from route selection
  observe({
	if (is.null(input$input_rt) || is.null(rv$sch)) return()
	rv$route_stops <- rv$sch[as.character(route_short_name) == input$input_rt, .N, keyby = c('stop_name', 'stop_id')]
	dat <- rv$stops_sf[rv$stops_sf$stop_id %in% rv$route_stops$stop_id,]
	leafletProxy("inputmap", session) %>% clearMarkers() %>% addCircleMarkers(data = dat, fillColor = 'black', fillOpacity = 0.9, stroke = FALSE, layerId = ~stop_id)

	updateSelectizeInput(session, 'input_stops', choices = rv$route_stops, selected = NULL)
  })

  
  # update selected from marker clicks
  observe({
    event <- input$inputmap_marker_click
    req(event)

    isolate({
    	selected <- event$id
	    current <- input$input_stops
    	select_update <- setdiff(union(current, selected), intersect(current, selected))
    	updateSelectizeInput(session, 'input_stops', selected = select_update)
    })
  })
  
  # update markers for removed stops
  observe({
  	selected <- input$input_stops
  	req(selected)
  	
  	isolate({
  		leafletProxy("inputmap", session) %>% addCircleMarkers(data = rv$stops_sf[rv$stops_sf$stop_id %in% rv$route_stops$stop_id,], fillColor = 'black', fillOpacity = 0.9, stroke = FALSE, layerId = ~stop_id) %>% addCircleMarkers(data = rv$stops_sf[rv$stops_sf$stop_id %in% selected,], fillColor = 'red', fillOpacity = 0.9, stroke = FALSE, layerId = ~stop_id)
  	})
  })

	## Select TZ ####
	observeEvent(input$tz, {
		req(input$tz)
		rv$tz <- input$tz
	})

  	## Load vehicle positions ####
	observeEvent(input$vp_file, {
		vp_file <- input$vp_file
		if (is.null(vp_file$datapath)) return()

		# read vehicle positions, may be zipped?
		vp = fread(vp_file$datapath)

		rv$vp <- vp
		if (is.null(rv$crs)) rv$crs <- inferUTM(rv$vp[1, c(longitude, latitude)])
		setkeyv(rv$vp, c('timestamp', 'trip_id'))
	})

	## Check for matching trip_ids
	observe({
		req(rv$vp, rv$trips)
		tryCatch(trip_match <- rv$trips[rv$vp, on = 'trip_id', nomatch = NULL], error = function(e) error_msg('Unable to join vehicle positions to GTFS on trip_id, check that trip_ids in GTFS match trip_ids in vehicle positions file. IDs must be of same type (e.g., character, numeric) and have the same value.'))
		if (nrow(trip_match) == 0) error_msg('No trip_id values in vehicle positions match trip_id values in the GTFS. Check that you have the correct GTFS for the vehicle positions, and that the IDs match and are of the same type (e.g., character, or numeric).')
	})

	## Process data ####
	observeEvent(input$process_action, {
		req(input$input_stops, rv$sch, rv$vp)

		## match to shapes
		mm <- matchAVL(rv$vp, rv$gtfspath, rv$crs, tz = rv$tz)

		## filter
		filtered <- filterMatches(mm, max_speed = 30)

		## add metadata
		filtered[rv$trips, on = 'trip_id', `:=` (route_id = i.route_id, trip_headsign = i.trip_headsign, shape_id = i.shape_id, service_id = i.service_id, direction_id = i.direction_id)]

		# add route_short_name from routes
		filtered[rv$routes, on = 'route_id', `:=` (route_short_name = i.route_short_name)]

		# add time period
		time_lu <- if (!is.null(rv$time_lu)) {
			rv$time_lu
		} else {
			data.table(Time = as.integer(3600 * c(0, 6, 9, 15, 19, 24)), TOD = factor(c('Owl', 'AM Peak', 'Midday', 'PM Peak', 'Evening', 'Owl'), levels = c('AM Peak', 'Midday', 'PM Peak', 'Evening', 'Owl'), ordered = TRUE), key = 'Time')
		}
		filtered[, `:=` (Time = as.ITime(structure(timestamp, class = c('POSIXct', 'POSIXt'), tz = rv$tz), tz = rv$tz))]
		ok <- key(filtered)
		filtered <- time_lu[filtered, on = 'Time', roll = TRUE]
		setkeyv(filtered, ok)

		# add day of week
		filtered[, `:=` (DOW = weekdays(as.Date(as.character(start_date), format = '%Y%m%d')))]

		# add date range
		filtered[, `:=` (date_range = paste(range(start_date, na.rm = TRUE), collapse = '\u2013'))]

		# add service_name
		if (is.null(rv$service_lu)) rv$service_lu <- lookupService(rv$gtfspath)
		filtered[rv$service_lu, on = 'service_id', `:=` (service_name = i.service_name)]
		filtered[, trip_desc := paste(route_short_name, trip_headsign, sep = ' - '), by = 'shape_id']

		matched(filtered)

	})
	
	# signal availability of processed dataset
	output$processed <- reactive({return(!is.null(matched()))})
	outputOptions(output, 'processed', suspendWhenHidden = FALSE)
	
	## Save processed data
	output$saveProcessed <- downloadHandler(
		filename = paste0('vp_', strftime(Sys.Date(), '%Y%m%d'), "_", "_processed.zip"),
		content = function(fname) {
			tmpdir <- tempdir()
			fs <- file.path(tmpdir, "processed.csv")
			fwrite(matched(), fs)
			zip(zipfile = fname, files = fs, flags = '-jr9X')
		}, contentType = "application/zip"
	)

	observeEvent(input$proc_file, {
		proc_file <- input$proc_file
		if (is.null(proc_file$datapath)) return()
		# read vehicle positions
		if (proc_file$type == 'application/zip') {
			fpath <- unzip(proc_file$datapath, exdir = tempdir())
			fpath <- list.files(fpath, pattern = '^processed\\.csv', full.names = TRUE)
		} else {
			fpath <- proc_file$datapath
		}
		tryCatch(vp <- fread(fpath), error = function(e) {error_msg(e); vp <- NULL})
		if (!"data.table" %in% class(vp)) {
			error_msg("Unable to read a data.table from uploaded file. Please upload a zipped csv.")
			return()
		}

		# check required fields
		req_fields <- c('start_date', 'trip_id', 'vehicle_id', 'timestamp', 'latitude', 'longitude')
		missing_ix <- !req_fields %in% names(vp)
		if (any(missing_ix)) {
			error_msg(paste("Required fields", paste(req_fields[missing_ix], collapse = ','), " not found in uploaded vehicle positions file."))
			return()
		}

		setkeyv(vp, c('start_date', 'trip_id', 'vehicle_id', 'timestamp'))
		matched(vp)
		if (is.null(rv$crs)) rv$crs <- inferUTM(matched()[1, c(longitude, latitude)])
		unlink(fpath)
	})

	## Update select inputs from matched data ####
	observeEvent(matched(), {
		req(matched())
		
		# routes and directions
		rv$rd_choices <- matched()[, .N, keyby = c('route_short_name', 'direction_id')][, paste(route_short_name, direction_id, sep = ' - ')]
		updateSelectizeInput(session, 'rt_dir', choices = rv$rd_choices, selected = rv$rd_choices)
		# date range
		dr <- matched()[, as.Date(structure(range(timestamp, na.rm = TRUE), class = c('POSIXct', 'POSIXt'), tz = rv$tz))]
		updateDateRangeInput(session, 'dr', start = dr[1], end = dr[2], min = dr[1], max = dr[2])
		updateDateInput(session, 'date_after', min = dr[1], max = dr[2])
	})
	observeEvent(input$rt_dir_all, {
	    updateSelectizeInput(session, 'rt_dir', selected = rv$rd_choices)
	})

	observeEvent(input$date_after, {
		req(input$date_after, matched())
		# update date_range field of matched
		intdate <- as.integer(strftime(input$date_after, '%Y%m%d'))
		matched(matched()[, `:=` (date_range = paste(range(start_date, na.rm = TRUE), collapse = '\u2013')), by = start_date < intdate])
	})

	## Outputs for summary tab ####
	## Polling rate histogram
	observe({
		req(matched())

		pollhist <- matched()[, .(delta = diff(timestamp)), keyby = c('start_date', 'vehicle_id', 'trip_id', 'route_short_name')][0 < delta, .N, keyby = c('route_short_name', 'delta')]
		output$summary_polling_hist <- renderPlot({
			ggplot(data = pollhist, aes(x = delta, y = N, fill = route_short_name)) + geom_col() + labs(x = "Effective Polling Rate", y = "") + scale_y_continuous(labels = scales::comma) + theme_minimal() + scale_fill_hue('Route')
		})
	})

	## Speed histogram
	output$summary_speed <- renderPlot({
		req(avl <- matched())
		# filter on inputs: date range, day type, time range
		dr <- as.integer(strftime(input$dr, '%Y%m%d'))
		rt_dir <- tstrsplit(input$rt_dir, ' - ', fixed = TRUE, type.convert = TRUE, names = c('route_short_name', 'direction_id'))
		avl <- avl[between(Time, input$time[1], input$time[2]) & between(start_date, dr[1], dr[2])][as.data.table(rt_dir), on = c('route_short_name', 'direction_id')]
		plotSpeedHistogram(avl, input$compare)
	})

	## Map speeds ####
	# initialize map
	output$speedline_map <- renderLeaflet({
		leaflet(options = leafletOptions(zoomControl = FALSE)) %>% htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'bottomleft'}).addTo(this)}") %>% addProviderTiles('Stamen.TonerLite')
	})
	outputOptions(output, 'speedline_map', suspendWhenHidden = FALSE)

	observeEvent(matched(), {
		req(matched(), rv$shapes)
		## initialize map
		bounds <- matched()[, c(range(match_lon), range(match_lat))]
		leafletProxy('speedline_map', session) %>% fitBounds(bounds[1], bounds[3], bounds[2], bounds[4])
		
		# interpolate speed along line
		speed(speedOnLine(matched(), 3, rv$shapes, rv$crs))
  })

	# update speed map
	sl_colorrange <- debounce(reactive(input$sl_colorrange), 1000)
	observe({
	  req(speed())
	  # aggregate
	  # TODO: add filters and grouping
	  grouping <- c('shape_id', 'avl_dist_traveled', 'lon_imp', 'lat_imp')
	  speed <- speed()[!is.na(mps), .(med = median(mps), avg = mean(mps), low = quantile(mps, 0.05), hi = quantile(mps, 0.95)), keyby = grouping]
	  
	  # Create colorscale
	  if (isTRUE(input$sl_autocolorrange)) {
	    speed_domain <- range(unlist(speed[, lapply(.SD, range, na.rm = TRUE), .SDcols = c('med', 'avg', 'low', 'hi')][, lapply(.SD, range)]))
	    updateSliderInput(session, 'sl_colorrange', value = speed_domain)
	  } else {
	    speed_domain <- as.numeric(sl_colorrange())
	  }
	  speedScale <- colorNumeric('magma', domain = speed_domain)

	  # update map
	  leafletProxy('speedline_map', session, data = speed) %>% clearMarkers() %>% clearControls() %>% addCircles(lng = ~lon_imp, lat = ~lat_imp, color = ~speedScale(med), radius = 10, label = ~paste0(round(med, 1), " m/s"), group = "Median") %>% addCircles(lng = ~lon_imp, lat = ~lat_imp, color = ~speedScale(avg), radius = 10, label = ~paste0(round(avg, 1), " m/s"), group = "Average") %>% addCircles(lng = ~lon_imp, lat = ~lat_imp, color = ~speedScale(low), radius = 10, label = ~paste0(round(low, 1), " m/s"), group = "5th Percentile") %>% addCircles(lng = ~lon_imp, lat = ~lat_imp, color = ~speedScale(hi), radius = 10, label = ~paste0(round(hi, 1), " m/s"), group = "95th Percentile") %>% addLegend(position = "bottomright", pal = speedScale, values = seq(speed_domain[1], speed_domain[2], length.out = 5), title = 'Speed (m/s)') %>% addLayersControl(baseGroups = c('Median', 'Average', '5th Percentile', '95th Percentile'), position = 'bottomleft')
	})
	
	## Speed by distance plot ####
	output$speed_dist <- renderPlot({
		req(speed())
		plotSpeedDist(speed(), input$ci, input$compare)
	})

	## Cumulative time by distance plot ####
	observe({
		req(matched())
		dtt <- speedToTT(matched(), input$compare)
		slots <- names(dtt)
		output$dtt <- renderUI({
			lapply(slots, function(x) {
				output[[paste0('dtt_', x)]] <- renderPlot(dtt[[x]])
			})
			plot_output_list <- lapply(slots, function(x) {
				plotOutput(paste0('dtt_', x), height = 400)
			})
			do.call(tagList, plot_output_list)
		})
	})

}) # end server
