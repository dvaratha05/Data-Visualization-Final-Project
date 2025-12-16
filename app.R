# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)
library(leaflet) 

# Define UI
ui <- fluidPage(
  titlePanel("South Bend Business Licenses & Points of interest Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Filter Options for Business Licenses Data"),
      selectInput(
        "year",
        "Select Year (Applicable to Business Licenses data only):",
        choices = NULL,
        selected = NULL
      ),
      hr(),
      h4("Business Count by Status Filters"),
      selectInput(
        "zip_code",
        "Select ZIP Code (Business Count by Status Chart Only):",
        choices = NULL,  
        selected = "All ZIP Codes"
      ),
      hr(),
      h4("Business Locations Map Filters"),
      # License type check boxes for map
      checkboxGroupInput(
        "map_license_types",
        "License Types to Show on Map:",
        choices = c(
          "Public Parking Facility License",
          "Hotel and Motel License",
          "Taxi Vehicle License",
          "Taxi Driver License",
          "Taxi Company License"
        ),
        selected = c(
          "Public Parking Facility License",
          "Hotel and Motel License",
          "Taxi Vehicle License",
          "Taxi Driver License",
          "Taxi Company License"
        )
      ),
      # Status filter for map
      checkboxGroupInput(
        "map_status_filter",
        "Status to Show on Map:",
        choices = NULL,  
        selected = NULL
      ),
      hr(),
      h4("Summary"),
      textOutput("total_businesses"),
      br(),
      helpText("Year filter applies to all tabs. ZIP code filter applies to chart only."),
      hr(),
      h4("Filter Options for South Bend: Points Of Interest"),
      # Remove hardcoded choices - will be updated in server
      checkboxGroupInput(inputId = 'source',
                         label="Category",
                         choices = NULL,  # Will be updated in server
                         selected = NULL),
      sliderInput(inputId = 'zoom',
                  label='Zoom Level',
                  min = 10,
                  max = 16,
                  value = 12,
                  step=1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Business Count by Status",
                 fluidRow(
                   column(4,
                          selectInput(
                            "License.Type",
                            "Select License Type (Chart Only):",
                            choices = NULL,
                            selected = "All Types",
                            multiple = FALSE
                          )
                   )
                 ),
                 br(),
                 plotOutput("status_plot", height = "500px"),
                 br(),
        ),
        tabPanel("Business Locations Map",
                 h4("Map of Selected License Types (All South Bend ZIP Codes)"),
                 leafletOutput("business_map", height = "600px"),
                 br(),
                 helpText("Click on markers for business details. Map shows all South Bend ZIP codes.")
        ),
        tabPanel("Points of Interest Map",
                 h4("South Bend: Points of Interest"),
                 leafletOutput('map', height = 650),
                 br(),
                 helpText("Interactive map showing parks, schools, libraries, and other public facilities in South Bend.")
        ),
        tabPanel("About",
                 h3("Dashboard Information"),
                 p("This dashboard visualizes business license data for South Bend, Indiana. 
                   It also shows an interactive map of public services, schools and recreation area."),
                 p("It allows the Mayor's office to:"),
                 tags$ul(
                   tags$li("Track business license counts by status (Active, Expired, etc.)"),
                   tags$li("Analyze trends across different license types"),
                   tags$li("Monitor business activity year over year by ZIP code"),
                   tags$li("Visualize geographic distribution of specific business types"),
                   tags$li("Compare Hotel & Motel business establishments near the recreation area"),
                   tags$li("Compare Public parking facility near the public services and School"),
                   tags$li("This comparison will help the Mayor to approve or deny a license of the business establishments")
                 ),
                 p("South Bend : Point of Interest Map can also be used to :"),
                 tags$ul(
                   tags$li("Highlight features & amenities that may be of interest to residents of
                            South Bend, addressing potential questions or concerns from secondary audiences"),
                   tags$li("Visitors can use this map to identify recreation area, Public services and Map"),
                   tags$li("Both business establishment and point of interest map can be updated and displayed in city website for public
                           use after the review and approval of the Mayor."),
                 ),
                 br(),
                 h4("How to Use"),
                 p("1. Select a year (applies to Business Licenses Data tabs only)"),
                 p("2. Select a ZIP code for Business Count by Status chart analysis only"),
                 p("3. For the business count by start chart tab, select a license type from the dropdown above the chart"),
                 p("4. For the business locations map tab, use the checkboxes to select which license types and statuses to display"),
                 p("5. Points of interest map uses filters like Public services, recreation and schools. Use zoom level to zoom-in or
                   zoom-out of the map"),
                 br(),
                 p(strong("Note:"), "Business count by status chart can be filtered by ZIP code. Business locations Map shows 
                   all South Bend ZIP codes.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Define facility colors
  facility_colors <- c(
    "public park"   = "#1b9e77",
    "golf course"   = "#66a61e",
    "other"         = "#a68454",
    "public school"  = "#7570b3",
    "private school" = "#a510cb",
    "emergency services" = "#d95f02",
    "public library"     = "#e7298a"
  )
  
  # Load points of interest data
  all_facilities_clean <- reactive({
    # Check if file exists
    if(!file.exists("all_facilities_clean.csv")) {
      # Create dummy data if file doesn't exist (for testing)
      return(data.frame(
        source = character(0),
        facility_type = character(0),
        name = character(0),
        lon = numeric(0),
        lat = numeric(0)
      ))
    }
    
    # Load the data
    data <- read.csv("all_facilities_clean.csv", row.names = NULL)
    return(data)
  })
  
  # Update the source choices for points of interest filter
  observe({
    facilities_data <- all_facilities_clean()
    
    if(nrow(facilities_data) > 0 && "source" %in% names(facilities_data)) {
      source_choices <- sort(unique(facilities_data$source))
      
      updateCheckboxGroupInput(
        session, 
        "source",
        choices = source_choices,
        selected = if(length(source_choices) > 0) source_choices else character(0)
      )
    }
  })
  
  #Preparing Business_Licenses Data
  # Load Business_Licenses.csv data and filter data to only South Bend, Indiana
  sample_data <- reactive({
    # Check if file exists
    if(!file.exists("Business_Licenses.csv")) {
      # Create empty data frame if file doesn't exist
      return(data.frame(
        City = character(0),
        State = character(0),
        year = numeric(0),
        License.Type = character(0),
        Status = character(0),
        Zip.Code = character(0),
        Business.Name = character(0),
        Street.Address = character(0)
      ))
    }
    
    # Read the CSV file
    data <- read.csv("Business_Licenses.csv")
    
    city_col <- 'City'
    state_col <- 'State'
    # Apply city and state filters
    if(city_col %in% names(data)) {
      # Clean city names (remove extra spaces, standardize case)
      data[[city_col]] <- trimws(toupper(data[[city_col]]))
      data <- data[data[[city_col]] %in% c("SOUTH BEND", "SOUTHBEND"), ]
    }
    
    if(state_col %in% names(data)) {
      # Clean state names
      data[[state_col]] <- trimws(toupper(data[[state_col]]))
      data <- data[data[[state_col]] %in% c("IN", "INDIANA"), ]
    }
    
    if(!is.numeric(data$year)) {
      data$year <- as.numeric(as.character(data$year))
    }
    
    # Clean ZIP codes if column exists
    if("Zip.Code" %in% names(data)) {
      data$Zip.Code <- trimws(as.character(data$Zip.Code))
    } else if("ZipCode" %in% names(data)) {
      data$ZipCode <- trimws(as.character(data$ZipCode))
      # Rename to Zip.Code for consistency
      names(data)[names(data) == "ZipCode"] <- "Zip.Code"
    } else if("ZIP.Code" %in% names(data)) {
      data$ZIP.Code <- trimws(as.character(data$ZIP.Code))
      names(data)[names(data) == "ZIP.Code"] <- "Zip.Code"
    }
    
    # Remove NA years
    data <- data[!is.na(data$year), ]
    
    return(data)
  })
  
  # Reactive data for the 5 specific license types only to be used in the business locations map
  map_license_data <- reactive({
    data <- sample_data()
    
    # Define the 5 target license types
    target_licenses <- c(
      "Public Parking Facility License",
      "Hotel and Motel License", 
      "Taxi Vehicle License",
      "Taxi Driver License",
      "Taxi Company License"
    )
    
    # Filter to only include these license types
    data %>% dplyr::filter(License.Type %in% target_licenses)
  })
  
  # Update drop down choices based on data
  observe({
    data <- sample_data()
    
    # Update year choices
    updateSelectInput(session, "year",
                      choices = c("All Years", sort(unique(data$year))),
                      selected = "All Years")
    
    # Update ZIP code choices
    if("Zip.Code" %in% names(data)) {
      zip_codes <- sort(unique(data$Zip.Code))
      zip_codes <- zip_codes[!is.na(zip_codes) & zip_codes != ""]
      updateSelectInput(session, "zip_code",
                        choices = c("All ZIP Codes", zip_codes),
                        selected = "All ZIP Codes")
    }
    
    # Update license type choices for chart
    updateSelectInput(session, "License.Type",
                      choices = c("All Types", sort(unique(data$License.Type))),
                      selected = "All Types")
  })
  
  # Observe changes to selected license types and update status filter
  observe({
    # Get the filtered data for the 5 license types
    data <- map_license_data()
    
    # Filter further based on currently selected license types
    if (!is.null(input$map_license_types) && length(input$map_license_types) > 0) {
      filtered_data <- data %>% dplyr::filter(License.Type %in% input$map_license_types)
      
      # Get unique statuses from this filtered data
      if(nrow(filtered_data) > 0 && "Status" %in% names(filtered_data)) {
        available_statuses <- sort(unique(filtered_data$Status))
        
        # Update the status filter choices
        updateCheckboxGroupInput(session, "map_status_filter",
                                 choices = available_statuses,
                                 selected = if("ACTIVE" %in% available_statuses) "ACTIVE" 
                                 else if(length(available_statuses) > 0) available_statuses[1]
                                 else NULL)
      } else {
        # No data for selected license types
        updateCheckboxGroupInput(session, "map_status_filter",
                                 choices = character(0),
                                 selected = character(0))
      }
    } else {
      # No license types selected
      updateCheckboxGroupInput(session, "map_status_filter",
                               choices = character(0),
                               selected = character(0))
    }
  })
  
  # Filter data for YEAR only (applies to both business count by status chart and business locations map)
  year_filtered_data <- reactive({
    data <- sample_data()
    
    # Apply year filter only
    if (!is.null(input$year) && input$year != "All Years") {
      data <- data[data$year == as.numeric(input$year), ]
    }
    
    data
  })
  
  # Filter data for business count by status tab only (applies YEAR, ZIP CODE, and LICENSE TYPE filters)
  chart_data <- reactive({
    data <- year_filtered_data()  # Start with year-filtered data
    
    # Apply ZIP code filter ONLY for the chart
    if (!is.null(input$zip_code) && input$zip_code != "All ZIP Codes" && "Zip.Code" %in% names(data)) {
      data <- data[data$Zip.Code == input$zip_code, ]
    }
    
    # Apply license type filter ONLY for the chart
    if (!is.null(input$License.Type) && input$License.Type != "All Types") {
      data <- data[data$License.Type == input$License.Type, ]
    }
    
    data
  })
  
  # Create summary data for plotting
  summary_data <- reactive({
    chart_data() %>%
      group_by(Status) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count))
  })
  
  # Filter data for business locations map (applies YEAR only, not ZIP code)
  map_data <- reactive({
    # Start with data filtered for the 5 specific license types
    data <- map_license_data()
    
    if (nrow(data) == 0) {
      return(data.frame())
    }
    
    # Apply year filter (only filter applied to map)
    if (!is.null(input$year) && input$year != "All Years") {
      data <- data[data$year == as.numeric(input$year), ]
    }
    
    # Get selected license types from check box input
    if (is.null(input$map_license_types) || length(input$map_license_types) == 0) {
      return(data.frame())  # Return empty if no license types selected
    }
    
    # Get selected statuses from check. box input
    if (is.null(input$map_status_filter) || length(input$map_status_filter) == 0) {
      return(data.frame())  # Return empty if no statuses selected
    }
    
    # Filter for selected license types and statuses
    map_df <- data %>%
      dplyr::filter(License.Type %in% input$map_license_types & 
                      Status %in% input$map_status_filter)
    
    # Below code creates sample coordinates by using the latitude and longitude range of South Bend, IN
    if(!all(c("lat", "lon") %in% colnames(map_df)) && nrow(map_df) > 0) {
      # South Bend downtown coordinates
      south_bend_lat <- 41.6764
      south_bend_lon <- -86.2520
      
      # Create coordinates around South Bend
      map_df$lat <- south_bend_lat + runif(nrow(map_df), -0.015, 0.015)
      map_df$lon <- south_bend_lon + runif(nrow(map_df), -0.015, 0.015)
    }
    
    return(map_df)
  })
  
  # Color mapping for different license types
  license_colors <- reactive({
    list(
      "Public Parking Facility License" = "blue",
      "Hotel and Motel License" = "green",
      "Taxi Vehicle License" = "red",
      "Taxi Driver License" = "orange",
      "Taxi Company License" = "purple"
    )
  })
  
  # Map: Business locations
  output$business_map <- renderLeaflet({
    data <- map_data()
    
    if (nrow(data) == 0) {
      # Return empty map centered on South Bend
      leaflet() %>%
        addTiles() %>%
        setView(lng = -86.2520, lat = 41.6764, zoom = 13) %>%
        addControl(
          html = "<div style='padding: 10px; background: white; border-radius: 5px;'>
                   <strong>No licenses found matching your filters</strong><br>
                   Try selecting different license types or statuses
                 </div>",
          position = "topright"
        )
    } else {
      # Get color mapping
      color_map <- license_colors()
      
      # Create popup content
      popup_content <- paste(
        "<div style='min-width: 200px;'>",
        "<strong>", 
        if("Business.Name" %in% colnames(data)) {
          ifelse(is.na(data$Business.Name) | data$Business.Name == "", 
                 "Business", data$Business.Name)
        } else {
          "Business"
        }, 
        "</strong><br>",
        "<hr style='margin: 5px 0;'>",
        "<b>License Type:</b> ", data$License.Type, "<br>",
        "<b>Status:</b> ", data$Status, "<br>",
        "<b>Year:</b> ", data$year, "<br>",
        ifelse("Street.Address" %in% colnames(data) & 
                 !is.na(data$Street.Address) & data$Street.Address != "", 
               paste("<b>Address:</b> ", data$Street.Address, "<br>"), ""),
        ifelse("City" %in% colnames(data) & 
                 !is.na(data$City) & data$City != "", 
               paste("<b>City:</b> ", data$City, "<br>"), ""),
        ifelse("State" %in% colnames(data) & 
                 !is.na(data$State) & data$State != "", 
               paste("<b>State:</b> ", data$State, "<br>"), ""),
        ifelse("Zip.Code" %in% colnames(data) & 
                 !is.na(data$Zip.Code) & data$Zip.Code != "", 
               paste("<b>ZIP Code:</b> ", data$Zip.Code, "<br>"), ""),
        "</div>"
      )
      
      # Create labels
      labels <- if("Business.Name" %in% colnames(data)) {
        ifelse(!is.na(data$Business.Name) & data$Business.Name != "", 
               paste(data$Business.Name, " (", data$License.Type, ")", sep=""),
               paste("Business (", data$License.Type, ")", sep=""))
      } else {
        paste("Business (", data$License.Type, ")", sep="")
      }
      
      # Initialize map - center on data
      center_lat <- mean(data$lat, na.rm = TRUE)
      center_lon <- mean(data$lon, na.rm = TRUE)
      
      # If we have valid coordinates, use them, otherwise use default
      if(is.na(center_lat) || is.na(center_lon)) {
        center_lat <- 41.6764
        center_lon <- -86.2520
      }
      
      m <- leaflet(data) %>%
        addTiles() %>%
        setView(lng = center_lon, lat = center_lat, zoom = 13)
      
      # Add markers for each license type separately
      license_types_in_data <- unique(data$License.Type)
      
      for(license_type in license_types_in_data) {
        type_data <- data %>% dplyr::filter(License.Type == license_type)
        
        if(nrow(type_data) > 0) {
          color <- ifelse(license_type %in% names(color_map), 
                          color_map[[license_type]], "gray")
          
          m <- m %>%
            addCircleMarkers(
              data = type_data,
              lng = ~lon,
              lat = ~lat,
              radius = 8,
              color = "white",
              fillColor = color,
              fillOpacity = 0.8,
              weight = 2,
              popup = ~popup_content,
              label = ~labels
            )
        }
      }
      
      # Create legend for only the license types present in data
      present_license_types <- license_types_in_data
      legend_colors <- sapply(present_license_types, function(type) {
        ifelse(type %in% names(color_map), color_map[[type]], "gray")
      })
      
      # Add legend
      if(length(present_license_types) > 0) {
        m <- m %>%
          addLegend(
            position = "bottomright",
            colors = legend_colors,
            labels = present_license_types,
            title = "License Type",
            opacity = 0.8
          )
      }
      
      # Add layer control if we have multiple types
      #if(length(present_license_types) > 1) {
      #  m <- m %>%
      #    addLayersControl(
      #      overlayGroups = present_license_types,
      #      options = layersControlOptions(collapsed = FALSE)
      #    )
      #}
      
      # Add info panel
      m <- m %>%
        addControl(
          html = paste0(
            "<div style='padding: 10px; background: white; border-radius: 5px;'>
              <strong>Licenses Map (All South Bend ZIP Codes)</strong><br>
              Showing: ", nrow(data), " businesses<br>
              License Types: ", length(unique(data$License.Type)), "<br>
              Statuses: ", paste(unique(data$Status), collapse=", "), "<br>
              Year: ", ifelse(input$year != "All Years", input$year, "All Years"),
            "</div>"
          ),
          position = "topright"
        )
      
      m
    }
  })
  
  # Added this function to determine color pattern based on license status count
  get_status_colors <- function(statuses) {
    n <- length(statuses)
    
    #larger color palette for many categories
    if(n <= 8) {
      # For 8 or fewer statuses, use Set2
      colors <- RColorBrewer::brewer.pal(max(3, n), "Set2")[1:n]
    } else if(n <= 12) {
      #For 9-12 statuses, use Set3
      colors <- RColorBrewer::brewer.pal(min(12, n), "Set3")[1:n]
    } else if(n <= 20) {
      colors <- scales::hue_pal()(n)
    } else {
      colors <- rainbow(n)
    }
    
    names(colors) <- statuses
    return(colors)
  }
  
  # Plot: Business count by status
  output$status_plot <- renderPlot({
    summary <- summary_data()
    
    if (nrow(summary) == 0) {
      plot(0, 0, type = "n", xlab = "", ylab = "", 
           main = "No data available for selected filters")
      text(0, 0, "Please select different filters")
      return()
    }
    
    # Get colors based on number of statuses
    status_colors <- get_status_colors(summary$Status)
    
    # Create subtitle with ZIP code info
    zip_info <- if(!is.null(input$zip_code) && input$zip_code != "All ZIP Codes") {
      paste("ZIP Code:", input$zip_code)
    } else {
      "All ZIP Codes"
    }
    
    ggplot(summary, aes(x = reorder(Status, Count), y = Count, fill = Status)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), hjust = -0.2, size = 4) +
      coord_flip() +
      labs(title = paste("Business Count by Status"),
           subtitle = ifelse(input$License.Type != "All Types" && input$year != "All Years",
                             paste("License Type:", input$License.Type, "| Year:", input$year, "|", zip_info),
                             ifelse(input$License.Type != "All Types",
                                    paste("License Type:", input$License.Type, "|", zip_info),
                                    ifelse(input$year != "All Years",
                                           paste("Year:", input$year, "|", zip_info),
                                           paste("All Years and Types |", zip_info)))),
           x = "Status",
           y = "Number of Businesses") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12, color = "gray50"),
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 12)) +
      scale_fill_manual(values = status_colors) +
      expand_limits(y = max(summary$Count) * 1.1)
  })
  
  # Output: Total businesses count
  output$total_businesses <- renderText({
    data <- chart_data()  # Use chart_data which includes ZIP filter
    total <- nrow(data)
    zip_info <- if(!is.null(input$zip_code) && input$zip_code != "All ZIP Codes") {
      paste("in ZIP Code", input$zip_code)
    } else {
      "in South Bend, IN"
    }
    paste("Total Businesses", zip_info, ":", total)
  })
  
  # Filter data based on user selection for points of interest
  filtered <- reactive({
    facilities_data <- all_facilities_clean()
    
    if(nrow(facilities_data) == 0) {
      return(data.frame())
    }
    
    # Filter based on source selection
    if (!is.null(input$source) && length(input$source) > 0) {
      filtered_data <- facilities_data %>% 
        filter(source %in% input$source)
    } else {
      filtered_data <- facilities_data
    }
    
    # Convert coordinates to numeric
    filtered_data %>%
      mutate(
        lon = as.numeric(lon),
        lat = as.numeric(lat)
      ) %>%
      filter(!is.na(lon), !is.na(lat))
  })
  
  # Render the points of interest map
  output$map <- renderLeaflet({
    points_of_int <- filtered()
    
    if(nrow(points_of_int) == 0) {
      # Return empty map if no data
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -86.25, lat = 41.67, zoom = input$zoom) %>%
        addControl(
          html = "<div style='padding: 10px; background: white; border-radius: 5px;'>
                   <strong>No points of interest data available</strong><br>
                   Please check if the data file exists and contains valid data
                 </div>",
          position = "topright"
        )
    } else {
      pal <- colorFactor(
        palette = facility_colors,
        domain  = names(facility_colors)
      )
      
      leaflet(points_of_int) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -86.25, lat = 41.67, zoom = input$zoom) %>%
        addCircleMarkers(
          lng = ~lon, 
          lat = ~lat, 
          radius = 5, 
          stroke = FALSE, 
          fillOpacity = 1,
          color = ~pal(facility_type),
          popup = ~paste0("<b>", name, "</b><br>",
                          facility_type, "<br>",
                          "Data Source: ", source)
        ) %>%
        addLegend(
          position = "bottomright", 
          pal = pal, 
          values = ~facility_type, 
          title = "Facility Type", 
          opacity = 1
        )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
