# color scales for shiny app
color_scales = list(DOW = c('Sunday' = 'gray30', "Monday" = "#0053A0", "Tuesday" = "#ED1B2E", "Wednesday" = "#FFD200", "Thursday" = "#008144", "Friday" = "#F68A1E", 'Saturday' = 'grey60'),
                    TOD = c("Early" = "#0053A0", "AM Peak" = "#ED1B2E", "Midday" = "#FFD200", "PM Peak" = "#008144", "Evening" = "#F68A1E", "Owl" = "gray"),
                    date_range = c(Before = "#0053A0", After = "#ED1B2E"),
                    route_short_name = c("#0053A0", "#ED1B2E", "#FFD200", "#008144", "#F68A1E",
                                         "#00cdcd, #551a8b", "#000000", "#ff8197", "#7c2020"),
                    None = "#0053A0")
save(color_scales, file = 'R/sysdata.rda', compress = 'bzip2', version = 2)
