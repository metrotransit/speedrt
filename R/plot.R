#' Plot a speed density
#'
#' @param avl 
#' @param compare grouping for speed comparison, one of 'DOW', 'TOD', 
#' 'date_range', 'route_short_name', or 'None'.
#'
#' @return ggplot object
#' @export
plotSpeedHistogram <- function(avl, compare) {
	limits <- avl[, quantile(mps, c(0.01, 0.99), na.rm = TRUE)]
	temp_points <- avl[limits[1] <= mps & mps <= limits[2] & is.finite(mps)]
	if (compare == "None") {
		low <- quantile(temp_points$mps, 0.05, na.rm = TRUE)
		med <- median(temp_points$mps, na.rm = TRUE)
		high <- quantile(temp_points$mps, 0.95, na.rm = TRUE)
		p = ggplot() + geom_density(data = temp_points, aes(x = mps, color = I("#0053A0"), fill = I("#0053A0")), alpha = 0.1) + geom_vline(xintercept = c(med, low, high), linetype = 2) + geom_label(aes(x = med, y = 0, label = c("median"))) + geom_label(aes(x = low, y = 0, label = c("5th"))) + geom_label(aes(x = high, y = 0, label = c("95th"))) + labs(title = paste0(round(med, 1), " [", round(low, 1), ", ", round(high, 1), "] m/s"))
	} else {
		tt <- temp_points[, .(med = median(mps, na.rm = TRUE)), keyby = compare]
		p = ggplot() + geom_density(data = temp_points, aes(x = mps, group = eval(as.name(compare)), color = eval(as.name(compare)), fill = eval(as.name(compare))), alpha = 0.1) + geom_vline(aes(xintercept = med, color = eval(as.name(compare))), data = tt, linetype = 2) + scale_fill_manual('', values = color_scales[[compare]]) + scale_color_manual('', values = color_scales[[compare]]) + labs(title = tt[, paste(paste0(eval(as.name(compare)), ": ", round(med, 1)), collapse = ", ")])
	}
	p + labs(x = "Estimated Speed (m/s)", y = "Count") + theme_bw()
}

#' Plot speed by distance traveled
#'
#' @inheritParams speedToTT
#' @param alpha lower bound of confidence interval, 
#' e.g., 0.05 results in an interval of 0.05--0.95 or 90%.
#'
#' @return a ggplot object
#' @export
plotSpeedDist <- function(speed, compare = 'None', ci = 0) {
	ci <- as.numeric(ci)
	# TODO: add filtering
	if (compare == 'None') {
	  sum_tab <- speed[!is.na(mps), as.list(quantile(mps, c(ci, 0.5, 1 - ci))), keyby = c('shape_id', 'avl_dist_traveled')]
	} else {
	  sum_tab <- speed[!is.na(mps), as.list(quantile(mps, c(ci, 0.5, 1 - ci))), keyby = c('shape_id', 'avl_dist_traveled', compare)]
	}
	setnames(sum_tab, setdiff(names(sum_tab), key(sum_tab)), c('low', 'Median', 'high'))
	ptitle <- "Speed by Distance - Median"
	ribbon <- NULL
	pos <- ifelse(is.null(compare), 'none', 'bottom')
	col_scale <- if (compare == 'route_short_name') {
	  color_scales[[compare]][seq_len(sum_tab[, uniqueN(compare)])]
	} else {
	  color_scales[[compare]]
	}

	if (ci > 0) {
		ptitle <- paste0(ptitle, " and ", paste0(100 * (1 - 2 * as.numeric(ci)), '%'), " Confidence Interval")
		ribbon <- geom_ribbon(aes(x = avl_dist_traveled, ymin = low, ymax = high, group = compare), alpha = 0.2, fill = "#0053A0")
	}

	p <- ggplot(data = sum_tab)
	p <- if (compare == 'None') {
	  p +
	    geom_line(aes(x = avl_dist_traveled, y = Median), size = 1)
	} else {
	  p +
	    geom_line(aes(x = avl_dist_traveled, y = Median, color = eval(as.name(compare))), size = 1)
	}
	
	p + theme_bw() +
	  scale_color_manual('', values = col_scale) +
	  labs(x = "Distance along segment in meters", y = "Speed (m/s)", title = ptitle) +
	  theme(legend.position = pos) +
	  ribbon +
	  facet_grid(rows = vars(shape_id)) +
	  theme_bw() +
	  labs(x = "Distance along segment in meters", y = "Speed (m/s)", title = ptitle) +
	  theme(legend.position = pos)
}

#' Plot cumulative travel time by distance
#'
#' @param speed avl speeds interpolated along shapes,
#'  see \code{\link{speedOnLine}}.
#' @param compare grouping for speed comparison, one of 'DOW', 'TOD', 
#' 'date_range', 'route_short_name', or 'None'.
#'
#' @return ggplot object
#' @export
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
