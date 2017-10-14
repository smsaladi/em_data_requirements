#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Shyam Saladi (saladi@caltech.edu)
# October 2017
#
# to deploy app: rsconnect::deployApp()

library(gdata)
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("EM Microscope Data"),

   # Sidebar with sliders that demonstrate various available options
   fluidRow(
       column(4,
     # dataset size
       sliderInput("box_dim", label = h3("Box side length: 2 ^ N * 2^10 (kpixels)"),
                   min = 1, max = 5, step = 1, value = 2),

       sliderInput("bit_depth", label = h3("Bit depth: 2^N depth"), min = 1,
                   max = 8, step = 1, value = 4),

       # alternatives
       sliderInput("fps", label = h3("Camera frames per second"), min = 10,
                   max = 400, step = 10, value = 40),

       sliderInput("seconds_collected", label = h3("Seconds collected"),
                   min = 0, max = 10, step = .5, value = 3)),

     column(4, offset = 0.5,
       # lagtimes
       sliderInput("time_per_movie", label = h3("Total time per movie: N minutes"),
                   min = 0, max = 5, step = 0.5, value = 1.5),

       sliderInput("movies_per_grid", label = h3("Number of movies per grid"),
                   min = 0, max = 50, step = 5, value = 100),

       # another metric for aggregate data rate
       sliderInput("grids_per_day", label = h3("Number of grids per day"),
                   min = 0, max = 100, step = 5, value = 10),
       sliderInput("calculation_per_dataset", label = h3("Time to calculate (average)"),
                   min = 1, max = 60, step = 1, value = 10)),

     column(4, offset = 0.5,
            # http://www.legitreviews.com/wd-black-512gb-m-2-pcie-nvme-ssd-review_191242/3
            # https://www.pcper.com/reviews/Storage/Triple-M2-Samsung-950-Pro-Z170-PCIe-NVMe-RAID-Tested-Why-So-Snappy/Preliminary-Resul
            selectInput("disk_speed_GB", label = h3("Local disk selection"),
                        choices = list("NVMe SSD (~1.2 GB/s)" = 1,
                                       "NVMe SSD RAID (<= 2.5 GB/s)" = 2,
                                       "SATA SSD (750 MB/s)" = 3,
                                       "SATA SSD RAID (<= 1.5 GB/s)" = 4,
                                       "SATA HDD (100 MB/s)" = 5,
                                       "2x SATA HDD RAID (<= 200 MB/s)" = 6,
                                       "4x SATA HDD RAID (<= 400 MB/s)" = 7,
                                       "Ramdisk (link-limited)" = 8),
                        selected = 5),
            selectInput("network_link_Gb", label = h3("Network link (slowest)"),
                        choices = list("1 Gb/s" = 1,
                                       "10 Gb/s" = 2,
                                       "2 x 10 Gb/s (teamed)" = 3),
                        selected = 1),
            sliderInput("network_duty_cycle", label = h3("Network duty cycle"),
                        min = 0, max = 1, step = .1, value = 1),
            sliderInput("nas_size", label = h3("Storage Array Size (TB)"),
                        min = 0, max = 1000, step = 50, value = 500)
       )),

     hr(),

      # Show a plot of the generated distribution
      fluidRow(
          column(3,
                 h4("Collection Statistics"),
                 tableOutput("collection_table")
          ),
          column(4, offset = 0.5,
                 h4("Link and Usage Statistics"),
                 tableOutput("usage_table")
          ),
          column(4, offset = 0.5,
                 h4("Disk Array Capacity"),
                 tableOutput("capacity_table")
          )
      )
)



# Define server logic required to show calculations
server <- function(input, output, session) {

    # Reactive expression to compose a data frame containing all of the values
    get_tables <- reactive({
        box_dim <- 2 ^ input$box_dim * 2 ^ 10

        # don't convert to bits here
        box_dim_formatted <- 2 ^ input$box_dim %>%
            paste(., "k", sep = "") %>%
            paste(. , "x", . )

        n_pixels <- box_dim ^ 2

        bit_depth <- 2 ^ input$bit_depth

        image_size_bits <- n_pixels * bit_depth

        image_size_formatted <- image_size_bits %>%
            paste("bits = ", humanReadable(. / 8))

        frames_collected <- input$seconds_collected * input$fps

        movie_size <- image_size_bits * frames_collected

        movie_size_formatted <- movie_size %>%
            paste("bits = ", humanReadable(. / 8))

        grid_size_formatted <- (movie_size * input$movies_per_grid) %>%
            paste("bits = ", humanReadable(. / 8))

        # Compose data frame
        collection_table <- data.frame(
            Name = c("Image Dimensions (px x px)",
                    # "Number of pixels",
                     "Bit depth",
                     "Image size",
                     "Frames collected",
                     "Movie size",
                     "Grid size"),
            Value = c(box_dim_formatted,
                     # n_pixels,
                      bit_depth,
                      image_size_formatted,
                      frames_collected,
                      movie_size_formatted,
                      grid_size_formatted) %>% as.character
            )

        disk_speed <- switch(input$disk_speed_GB,
                                "1" = 1.2,    # "NVMe SSD (~1.2 GB/s)" = 1.2,
                                "2" = 2.5,    # "NVMe SSD RAID (<= 2.5 GB/s)" = 2.5,
                                "3" = 0.750,  # "SATA SSD (750 MB/s)" = .750,
                                "4" = 1.5,    # SATA SSD RAID (<= 1.5 GB/s)" = 1.5,
                                "5" = 0.100,  # SATA HDD ,
                                "6" = 0.200,  # 2x SATA HDD RAID,
                                "7" = 0.400,  # 4x SATA HDD RAID,
                                "8" = Inf     # "Ramdisk (link-limited)" = Inf),
        ) * 8 # change to Gb/s

        network_link <- switch(input$network_link_Gb,
                                  "1" = 1,  # "1 Gb/s" = 1,
                                  "2" = 10, # "10 Gb/s" = 10,
                                  "3" = 20  # "2 x 10 Gb/s (teamed)" = 20),
        ) * input$network_duty_cycle

        # in bits/s
        link_speed <- ifelse(network_link < disk_speed,
                             network_link, disk_speed) * (2^10)^3

        limiting_link <- ifelse(network_link < disk_speed,
                                "network", "local disks")

        datarate_per_movie <- (movie_size / (input$time_per_movie * 60)) %>%
            round %>%
            paste(" bits/s = ", humanReadable(. / 8), "/s", sep = "")
        transfer_time_per_movie <- (movie_size / link_speed) / 60

        collection_time_per_grid <-
            input$time_per_movie * input$movies_per_grid
        transfer_time_per_grid <-
            transfer_time_per_movie * input$movies_per_grid

        datarate_per_day <-
            movie_size * input$movies_per_grid * input$grids_per_day
        datarate_per_day_formatted <- paste(
            round(datarate_per_day), " bits/day = ",
            humanReadable(datarate_per_day / 8), "/day", sep = "")


        movie_capacity <- input$nas_size * (10^3)^4 / (movie_size / 8)
        grid_capacity <- movie_capacity / input$movies_per_grid
        day_capacity <- grid_capacity / input$grids_per_day

        usage_table <- data.frame(
            Name = c("Data rate/movie",
                     "Time to transfer movie (min)",
                     "Collection time/grid (min)",
                     "Time to transfer grid (min)",
                     "Collection vs. Transfer Limiting",
                     "Daily aggregate data rate",
                     #"Link speed",
                     "Limiting link"),
            Value = c(datarate_per_movie,
                      transfer_time_per_movie,
                      collection_time_per_grid,
                      transfer_time_per_grid,
                      ifelse(collection_time_per_grid > transfer_time_per_grid,
                             "collection", "transfer"),
                      datarate_per_day_formatted,
                      #link_speed,
                      limiting_link) %>% as.character
        )

        capacity_table <- data.frame(
            Name = c("Dataset capacity (movies)",
                     "Dataset capacity (grids)",
                     "Dataset capacity (days)"),
            Value = c(round(movie_capacity),
                      round(grid_capacity),
                      round(day_capacity)) %>% as.character
        )

        list(collection_table, usage_table, capacity_table)
    })

    output$collection_table <- renderTable({
        get_tables()[[1]]
    })

    output$usage_table <- renderTable({
        get_tables()[[2]]
    })

    output$capacity_table <- renderTable({
        get_tables()[[3]]
    })
}

# Run the application
shinyApp(ui = ui, server = server)

