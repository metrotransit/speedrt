library(data.table)
time_lu = data.table(Time = as.integer(3600 * c(0, 6, 9, 15, 19, 24)), TOD = factor(c('Owl', 'AM Peak', 'Midday', 'PM Peak', 'Evening', 'Owl'), levels = c('AM Peak', 'Midday', 'PM Peak', 'Evening', 'Owl'), ordered = TRUE), key = 'Time')
save(time_lu, file = 'data/time_lu.rda', compress = 'bzip2', version = 2)
