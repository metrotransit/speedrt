#' @importFrom stats median quantile
NULL

#' Plot a speed density
#'
#' @param avl avl speeds matched to shapes, see \code{\link{filterMatches}}.
#' @param compare grouping for speed comparison, one of 'DOW', 'TOD', 
#' 'date_range', 'route_short_name', or 'None'.
#'
#' @return ggplot object
#' @export
plotSpeedHistogram <- function(avl, compare = 'None') {
	if (!requireNamespace('ggplot2', quietly = TRUE)) stop('Plotting functions require package \'ggplot2\', install the package: install.packages(\'ggplot2\')', .call = FALSE)
	mps = NULL
	limits <- avl[, quantile(mps, c(0.01, 0.99), na.rm = TRUE)]
	temp_points <- avl[limits[1] <= mps & mps <= limits[2] & is.finite(mps)]
	if (compare == "None") {
		low <- quantile(temp_points$mps, 0.05, na.rm = TRUE)
		med <- median(temp_points$mps, na.rm = TRUE)
		high <- quantile(temp_points$mps, 0.95, na.rm = TRUE)
		p = ggplot2::ggplot() + ggplot2::geom_density(data = temp_points, ggplot2::aes(x = eval(as.name('mps'))), color = "#0053A0", fill = "#0053A0", alpha = 0.1) + ggplot2::geom_vline(xintercept = c(med, low, high), linetype = 2) + ggplot2::geom_label(ggplot2::aes(x = med, y = 0, label = c("median"))) + ggplot2::geom_label(ggplot2::aes(x = low, y = 0, label = c("5th"))) + ggplot2::geom_label(ggplot2::aes(x = high, y = 0, label = c("95th"))) + ggplot2::labs(title = paste0(round(med, 1), " [", round(low, 1), ", ", round(high, 1), "] m/s"))
	} else {
		tt <- temp_points[, list(med = median(mps, na.rm = TRUE)), keyby = compare]
		p = ggplot2::ggplot() + ggplot2::geom_density(data = temp_points, ggplot2::aes(x = eval(as.name('mps')), group = eval(as.name(compare)), color = eval(as.name(compare)), fill = eval(as.name(compare))), alpha = 0.1) + ggplot2::geom_vline(ggplot2::aes(xintercept = eval(as.name('med')), color = eval(as.name(compare))), data = tt, linetype = 2) + ggplot2::scale_fill_manual('', values = color_scales[[compare]]) + ggplot2::scale_color_manual('', values = color_scales[[compare]]) + ggplot2::labs(title = tt[, paste(paste0(eval(as.name(compare)), ": ", round(med, 1)), collapse = ", ")])
	}
	p + ggplot2::labs(x = "Estimated Speed (m/s)", y = "Count") + ggplot2::theme_bw()
}

#' Plot speed by distance traveled
#'
#' @param speed avl speeds interpolated along shapes,
#'  see \code{\link{speedOnLine}}.
#' @param compare grouping for speed comparison, one of 'DOW', 'TOD',
#' 'date_range', 'route_short_name', or 'None'.
#' @param alpha lower bound of confidence interval, 
#' e.g., 0.05 results in an interval of 0.05-0.95 or 90\%.
#'
#' @return a ggplot object.
#' @export
plotSpeedDist <- function(speed, compare = 'None', alpha = 0) {
	if (!requireNamespace('ggplot2', quietly = TRUE)) stop('Plotting functions require package \'ggplot2\', install the package: install.packages(\'ggplot2\')', .call = FALSE)
	# avoid NSE warnings for data.table
	avl_dist_traveled = mps = NULL
	ci <- as.numeric(alpha)
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
		ribbon <- ggplot2::geom_ribbon(ggplot2::aes(x = eval(as.name('avl_dist_traveled')), ymin = eval(as.name('low')), ymax = eval(as.name('high')), group = eval(as.name(compare))), alpha = 0.2, fill = "#0053A0")
	}

	p <- ggplot2::ggplot(data = sum_tab)
	p <- if (compare == 'None') {
	  p +
	    ggplot2::geom_line(ggplot2::aes(x = eval(as.name('avl_dist_traveled')), y = eval(as.name('Median'))), size = 1)
	} else {
	  p +
	    ggplot2::geom_line(ggplot2::aes(x = eval(as.name('avl_dist_traveled')), y = eval(as.name('Median')), color = eval(as.name(compare))), size = 1)
	}
	
	p + ggplot2::theme_bw() +
	  ggplot2::scale_color_manual('', values = col_scale) +
	  ggplot2::labs(x = "Distance along segment in meters", y = "Speed (m/s)", title = ptitle) +
	  ggplot2::theme(legend.position = pos) +
	  ribbon +
	  ggplot2::facet_grid(rows = 'shape_id') +
	  ggplot2::theme_bw() +
	  ggplot2::labs(x = "Distance along segment in meters", y = "Speed (m/s)", title = ptitle) +
	  ggplot2::theme(legend.position = pos)
}

#' Plot cumulative travel time by distance
#'
#' @param speed avl speeds interpolated along shapes,
#'  see \code{\link{speedOnLine}}.
#' @param compare grouping for speed comparison, one of 'DOW', 'TOD', 
#' 'date_range', 'route_short_name', or 'None'.
#'
#' @return named list of ggplot objects, one for each member of \code{compare}.
#' @export
speedToTT <- function(speed, compare = 'None') {
	if (!requireNamespace('ggplot2', quietly = TRUE)) stop('Plotting functions require package \'ggplot2\', install the package: install.packages(\'ggplot2\')', .call = FALSE)
	if (compare == 'None') compare <- NULL
	# avoid NSE warnings for data.table
	timestamp = NULL

	speed[, `:=` (tt = as.numeric(timestamp - min(timestamp), units = 'secs')/60), keyby = c('start_date', 'trip_id', 'shape_id', 'trip_desc', compare)]

	if (is.null(compare)) {
		speed_split <- list(All = speed)
	} else {
		speed_split <- split(speed, speed[[compare]])
	}

	slots <- names(speed_split)[!sapply(speed_split, function(x) nrow(x) == 0)]
	result <- lapply(slots, function(x) {
		p <- ggplot2::ggplot(data = speed_split[[x]]) +
		ggplot2::geom_line(ggplot2::aes(x = eval(as.name('avl_dist_traveled')), y = eval(as.name('tt')), group = interaction(eval(as.name('start_date')), eval(as.name('trip_id'))), color = eval(as.name('trip_desc')))) +
		ggplot2::theme_bw() +
		ggplot2::scale_color_discrete('Route') +
		ggplot2::labs(x = "Distance along segment in meters", y = "Time traveled since the start of the segment in minutes", title = paste("Time Traveled by Distance -", x))
		p
	})
	names(result) <- slots
	return(result)
}
