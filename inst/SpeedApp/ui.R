source('helpers.R')

fluidPage(
  tags$head(
    tags$style(HTML(
      ".sidebar {
        height: 97vh; overflow-y: auto;
      }
      .form-group{
        margin-bottom: 0px;
      }
      .selectize-control{
        margin-bottom: 0px;
      }
      div.content-wrapper {
        overflow-y: auto;
      }
      .inner > div.info {display: none;}
      /* display loading message */
      html.shiny-busy header.main-header nav.navbar::after {
        position: fixed; top: 5px; right: 2em; padding:1em;
        background-color: #ee010177;
        clear: unset;
        content: 'Loading... Please wait';
      }
      /* Update buttons */
      button.btn, button.btn:hover, button.btn-default, button.btn-default:hover, button.btn-default:focus, button.btn-default:active {
        font: system-ui;
        font-size: revert;
        background-color: revert;
        color: revert;
        border-color: revert;
        box-shadow: revert;
        border-radius: revert;
        border: revert;
        background-image: revert;
        display: revert;
        padding: revert;
      }
      button.btn:focus, button.btn-default:focus, button.btn-default:active, button.btn:active {
        -webkit-appearance: button;
        -moz-appearance: button;
      }
      html.shiny-busy button, html.shiny-busy button.btn {
        -webkit-appearance: button;
        -moz-appearance: button;
      }
      button.btn-default, button.btn {
        -webkit-appearance: default-button;
        -moz-appearance: default-button;
      }

      /* Nav pills */
      ul.nav-pills li:first-child a {
        border-top-left-radius: 4px;
        border-bottom-left-radius: 4px;
      }
      ul.nav-pills li:last-child a {
        border-top-right-radius: 4px;
        border-bottom-right-radius: 4px;
      }
      .nav-pills>li>a {
        background-color: #f3f3f3;
      }"
    )) # end tags$style
  ), # end tags$head 
  dashboardPage(title = 'Speed Visualization Tool',
    skin = "black", 
    header = dashboardHeader(disable = TRUE), 
    sidebar = dashboardSidebar(width = '250px',
    	h4("Speed Visualization Tool"),
      sidebarMenu(id = 'menu',
                  menuItem(text = "Load Vehicle Positions", icon = icon("cloud-upload"), tabName = 'load'),
                  menuItem(text = "Visualize Speed", icon = icon("map"),
                           menuSubItem(text = "Summary", tabName = 'tab_summary'),
                           menuSubItem(text = "Speed Line", tabName = 'tab_speedline'),
                           menuSubItem(text = "Speed by Distance", tabName = 'tab_speeddist'),
                           menuSubItem(text = "Time by Distance", tabName = 'tab_timedist')
                           ),
                  menuItem(text = "About", icon = icon("list-alt"), tabName = "tab_about")
      ), 
      tags$hr(),
      ## inputs for visualization tabs #####
      conditionalPanel(
        condition = "input.menu != 'tab_about' & input.menu != 'load' & output.processed == true",
        selectizeInput('rt_dir', "Route(s) - Direction(s)", choices = c(), multiple = TRUE, options = list('placeholder' = "multiple allowed")),
        actionButton('rt_dir_all', "Select All"),
        radioButtons(inputId = "compare", inline = FALSE, label = "Compare", choices = c("None" = 'None', "by Time of Day" = 'TOD', "by Day of Week" = 'DOW', "by Date Ranges" = 'date_range', "by Route" = 'route_short_name')),
        conditionalPanel(
          condition = "input.compare == 'date_range'",
          dateInput('date_after', "First day of After Period")
        ),
        conditionalPanel(
          condition = "input.compare != 'date_range'",
          dateRangeInput('dr', "Date Range")
        ),
        conditionalPanel(
          condition = "input.compare != 'DOW'",
          selectInput('daytype', "Day Type", choices = c("Weekday", "Saturday", "Sunday"))
        ),
        conditionalPanel(
          condition = "input.compare != 'TOD'",
          sliderInput('time', "Time Range", min = 0, max = 24, value = c(0, 24))
        )
      )
    ), # end dashboardSidebar 
    body = dashboardBody(
    	tabItems(
        ## about tab ####
        tabItem(
          'tab_about',
          box(
            width = 6, solidHeader = TRUE, status = "primary", title = "Methodology",
            p("Steps taken to calculate bus travel speed:"),
            tags$ol(
              tags$li(p("Vehicle Position records are projected on to their respective route shapes.")),
              tags$li(p("Records that are more than 30 meters (~100 ft) away from the route are discarded.")),
              tags$li(p("Speed is calculated as the distance between consecutive messages divided by the time between consecutive messages (meters per second).")),
              tags$li(p("For each trip, speed is linearly interpolated for every meter along the route."))
            ),
            p("Higher frequency sampling results in more accurate speeds along the route.")
          )
        ), # end methods box
        ## load tab ####
        tabItem('load',
	        box(width = 12, solidHeader = TRUE, title = '1. Load GTFS', status = 'primary',
		        fileInput('gtfs_file', "Choose GTFS feed ZIP file:", accept = c('application/zip', '.zip')),
		        p("The GTFS file should be a zipped GTFS feed, with valid for the dates in the vehicle positions data, and matching trip_id identifiers")
	        ),
	        tabsetPanel(type = "pills", id = "load_method",
		        tabPanel(title = "Load Raw Vehicle Positions",
			        br(),
			        box(width = 6, solidHeader = TRUE, title = '2. Load Vehicle Positions', status = 'primary',
				        fileInput('vp_file', "Choose Vehicle Positions csv:", accept = c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
				        HTML("<p>The Vehicle Positions file is a csv of vehicle positions. It must include trip_id, timestamp, latitude, longitude and vehicle_id. See <code>readVehiclePosition</code> in this package (speedRT) to convert protobuf VehiclePosition files."),
				        br(),
				        selectizeInput('tz', label = "Choose the correct timezone for the vehicle position messages", choices = OlsonNames(), selected = Sys.timezone())
			        ),
					box(width = 6, solidHeader = TRUE, title = "3. Select stops for start and end of route segment.", status = "primary",
						actionButton('process_action', "Process data", class = "primary"),
						conditionalPanel("output.processed == true", {box(width = 6, solidHeader = TRUE, title = "4. Save Processed Data for Future Use", status = "primary", downloadButton('saveProcessed', "Download ZIP File"))})
					)
				),
				tabPanel(title = "Upload Processed Vehicle Positions",
					tags$br(),
					box(width = 12, solidHeader = TRUE, title = "Upload data from computer", status = 'primary',
						p(strong("Zipped csv files processed by the app.")),
						br(),
						fileInput('proc_file', "Choose input ZIP file:", accept = c('application/zip', '.zip'))
					)
				)
			)
        ),
        ## summary tab ####
        tabItem(
          'tab_summary',
          box(
            width = 6, solidHeader = TRUE, status = "primary", title = "Polling Rate",
            plotOutput('summary_polling_hist', height = '200px')
          ),
          box(
            width = 6, solidHeader = TRUE, status = "primary", title = "Speed Histogram",
            plotOutput('summary_speed')
          )
        ),
        ## speed line tab ####
        tabItem(
          'tab_speedline',
          fluidRow(
            column(2, sliderInput('sl_colorrange', "Color Range", min = 0, max = 30, value = c(0, 20))),
            conditionalPanel(
              condition = "input.compare == 'by Date Ranges'",
              column(2, sliderInput('sl_diffcolorrange', "Diff Color Range", min = -30, max = 30, value = c(-5, 5)))
            ),
            column(2, checkboxInput('sl_autocolorrange', "Auto Color Range", value = TRUE))
          ),
          conditionalPanel("output.speed_ready", {
            downloadButton('saveShapefile', "Download ESRI Shapefile")
          }), 
          leafletOutput('speedline_map', height = 800)
        ),
        ## speed by distance tab ####
        tabItem(
          'tab_speeddist',
          fluidRow(
            column(3, selectInput('ci', "Lower/Upper Bounds", 
                                  choices = c("5th - 95th %" = .05, "10th - 90th %" = .1, "20th - 80th %" = .2, "None" = 0)))
          ),
          plotOutput('speed_dist', height = 600)
        ),
        ## cumulative time by distance tab ####
        tabItem(
          'tab_timedist',
          uiOutput('dtt'),
          conditionalPanel(
            condition = "input.compare == 'route_short_name'",
            p("Cumulative Time by Distance plots by Route are not available.")
          )
        )
      )
    )
  )
  
) # close fluidPage
