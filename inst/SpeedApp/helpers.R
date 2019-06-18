libs <- c('speedRT', 'data.table', 'DT', 'd3heatmap', 'ggplot2', 'ggmap', 'leaflet', 'rgeos', 'rgdal', 'shiny', 'shinydashboard', 'sp', 'stringi', 'zoo', 'sf')

suppressMessages(invisible(lapply(libs, library, character.only = TRUE)))

options(shiny.maxRequestSize = 100 * 1024^2)
