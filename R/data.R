#' Time period lookup table
#'
#' A data.table containing the start time (seconds since midnight) of 
#' each time period, with the names of each time period as an ordered factor.
#' Used to provide a name to observation times.
#' 
#' @format A data.table with 6 rows and 2 variables:
#' \describe{
#'   \item{Time}{Time, in seconds since midnight}
#'   \item{TOD}{Time period labels, as an ordered factor. 
#'	  AM Peak < Midday < PM Peak < Evening < Owl}
#' }
#' @examples
#' require(data.table)
#' obs = data.table(Time = as.ITime('8:45'), id = 1)
#' time_lu[obs, on = 'Time', roll = TRUE]
"time_lu"
