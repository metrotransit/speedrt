# This file is part of speedRT
# Copyright (c) 2019 Metropolitan Council
#
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at https://mozilla.org/MPL/2.0/.

#' Internal launch function 
#' @param sao speedapp object
#' @param rstudio launch in rstudio viewer instead of web browser? 
#' @param ... passed to shiny::runApp
launch_speedapp <- function(sao = NULL, rstudio = Sys.getenv("RSTUDIO") == "1", ...) {
  launch.browser <- if (!rstudio) 
		TRUE 
    } else {
		getOption("shiny.launch.browser", interactive())
	}
  
  .sao_env$.SPEEDAPP_OBJECT <- sao  # see zzz.R for .sao_env
  on.exit(.sao_env$.SPEEDAPP_OBJECT <- NULL, add = TRUE)
  shiny::runApp(system.file("SpeedApp", package = "speedRT"), 
                launch.browser = launch.browser, ...)
}
