# This file is part of speedRT
# Copyright (c) 2019 Metropolitan Council
#
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at https://mozilla.org/MPL/2.0/.

#' Launch the Shiny Speed app  
#' @param sao speedapp object
#' @param rstudio launch in rstudio viewer instead of web browser? 
#' @param ... passed to shiny::runApp
#' @export
launch_speedapp <- function(sao = NULL, rstudio = Sys.getenv("RSTUDIO") == "1", ...) {
	# check for suggested packages required for app
	pkgs <- c('ggplot2', 'leaflet', 'rgdal', 'shiny', 'shinydashboard', 'stringi')
	pkg_exists <- suppressWarnings(sapply(pkgs, requireNamespace, quietly = TRUE))
	if (!all(pkg_exists)) stop(gettextf(fmt = 'SpeedApp requires one or more packages to be installed. Please install: %s and try again: install.packages(c(%s))', paste(names(pkg_exists)[!pkg_exists], collapse = ', '), paste(sQuote(names(pkg_exists)[!pkg_exists]), collapse = ', ')), call. = FALSE)
  launch.browser <- if(!rstudio) {
		TRUE 
    } else {
		getOption("shiny.launch.browser", interactive())
	}
  
  .sao_env$.SPEEDAPP_OBJECT <- sao  # see zzz.R for .sao_env
  on.exit(.sao_env$.SPEEDAPP_OBJECT <- NULL, add = TRUE)
  shiny::runApp(system.file("SpeedApp", package = "speedRT"), 
                launch.browser = launch.browser, ...)
}
