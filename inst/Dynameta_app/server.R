# Dynameta shiny app server script

# ---------------------------------------------------------------------------------------------

# Load packages
library(dplyr) # for data manipulation, part of tidyverse ("%>%" filter arrange relocate distinct)
library(DT) # for interactive tables
library(leaflet) # for mapping (renderLeaflet leaflet addTiles addCircleMarkers)
library(metafor) # for running meta-analytic models (escalc rma.mv forest)
library(shiny) # Required to run any Shiny app
library(shinyjs) # for enabling and disabling download button (enable disable show)
library(shinyWidgets) # for including a 'select all' option for filters (pickerInput)
library(tidyr) # for tidying messy data, part of tidyverse (drop_na)

# ---------------------------------------------------------------------------------------------

# Define the 'then' function, so it only returns the 1 error message at a time to the user 
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

# ---------------------------------------------------------------------------------------------

# Define the server function
server <- function(input, output) {
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### Load data that will be use throughout server.R file
  
  # =================================================================================================================
  # =================================================================================================================
  
  # -------------------------------------------------------------------------------------
  
  # User choice of data to analyse
  shiny::observeEvent(input$data_choice, {
    
    # If the user selects "Your own data", then 'show' the upload_data_to_analyse widget 
    if (input$data_choice == "Your own data") {
      shinyjs::show("upload_data_to_analyse")
    } else {
      shinyjs::hide("upload_data_to_analyse")
    }
    
  })
  
  # Create reactive data object 
  data <- shiny::reactive({
    
    # When testing the package with shinytest, need to load the data in from the data_for_shinytest directory
    # or app can't find data like it does when user has installed the package 
    sample_data <- read.csv("../data_for_shinytest/sample_data_for_shinytest.csv")
    
    # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
    if (input$data_choice == "Your own data" && !is.null(input$upload_data_to_analyse)) {
      
      # Checks on data 1 
      shiny::validate(
        
        # 1. Make sure file is a csv
        shiny::need(tools::file_ext(input$upload_data_to_analyse$name) == "csv", "File must be a .csv.") %then%
          
          # 2. Make sure file has certain columns - these are the columns in Daero's standardised data sheet
          # Colnames have to be exactly the same - tell user which columns in their dataframe are missing, and which shouldn't be there
          shiny::need(base::all(colnames(sample_data) == colnames(read.csv(input$upload_data_to_analyse$datapath))), 
                      paste("Data doesn't contain correct column(s).
                          \nMissing columns that must be present: ",
                            paste(setdiff(colnames(sample_data), colnames(read.csv(input$upload_data_to_analyse$datapath))), collapse = ", "),
                            "\nExtra columns that need to be removed: ", 
                            paste(setdiff(colnames(read.csv(input$upload_data_to_analyse$datapath)), colnames(sample_data)), collapse = ", ")))
      )
      
      data <- read.csv(input$upload_data_to_analyse$datapath)
    
    # Else if no data uploaded, or user has selected the sample data, then the data is the sample data
    } else {
      data <- sample_data
    }

  })
  
  # -------------------------------------------------------------------------------------
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### Intro tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  # ---------------------------------------------------------------------------------------------------------------
  
  ### Table for overview of papers included 
  
  # Add table legend
  output$table_legend_overview <- shiny::renderText({
    base::paste("<b>Table 1.</b>", "Total number of papers and data points available to investigate each threat category.")
  })
  
  # Add sample size table
  output$sample_sizes_overview <- shiny::renderTable({
    
    # Initialise empty data frame
    sample_sizes_table <- base::data.frame(Threat_category = character(), Number_of_papers = numeric(), Total_data_points = numeric())
    
    # Calculate statistics for each paper
    for (i in unique(data()$IUCN_threat_category_1)) { # data is whole spreadsheet
      
      threat_subset <- data() %>%
        dplyr::filter(IUCN_threat_category_1 %in% i)
      
      # make new row which will be added to sample_sizes_table
      new_row <- base::data.frame(
        Threat_category = i,
        Number_of_papers = length(unique(threat_subset$Paper_ID)), # number of unique agricultural systems
        Total_data_points = nrow(threat_subset) # number of instances
      )
      
      sample_sizes_table <- rbind(sample_sizes_table, new_row)
      
    }
    
    # Put table in alphabetical order based on paper_ID
    sample_sizes_table <- sample_sizes_table %>%
      arrange(Threat_category)
    
    # remove _ from colnames
    colnames(sample_sizes_table) <- c("Threat category", "Number of papers", "Total data points")
    
    sample_sizes_table <- sample_sizes_table
    
  },
  
  striped = TRUE
  
  )
  
  # ---------------------------------------------------------------------------------------------------------------
  
  ### Table for details of specific threat chosen by user 
  
  # Make reactive options
  output$reactive_threat <- shiny::renderUI({
    shiny::selectInput(inputId = "threat",
                       label = "Select threat to show more details for:",
                       choices = c("", unique(data()$IUCN_threat_category_1)),
                       selected = NULL)
  })
  
  # Add table legend
  output$table_legend_threat_details <- renderText({
    paste0("<b>Table 2. </b>", "Details on the papers that studied ", input$threat, ". For full paper details, see the References tab.", sep="")
  })
  
  # Add paper details table
  output$threat_details_table <- renderTable({
    
    # Filter for selected threat
    selected_threat <- data() %>%
      dplyr::filter(IUCN_threat_category_1 %in% input$threat) # Shows warnings if you use == rather than %in%
    
    # Make empty dataframe
    threat_details_table <- base::data.frame(Paper = character(), Number_of_data_points = numeric())
    
    # Fill in dataframe
    for (i in unique(selected_threat$Paper_ID)) {
      
      # Filter for each paper
      subset_threat_by_paper <- selected_threat %>%
        dplyr::filter(Paper_ID == i)
      
      # Make new row to add to dataframe
      new_row <- base::data.frame(
        Paper = i,
        Number_of_data_points = nrow(subset_threat_by_paper)
      )
      
      # Add row to dataframe
      threat_details_table <- rbind(threat_details_table, new_row)
    }
    
    # Order 
    threat_details_table <- threat_details_table %>%
      dplyr::arrange(Paper)
    
    # remove _ from colnames
    colnames(threat_details_table) <- c("Paper", "Number of data points")
    
    threat_details_table <- threat_details_table
    
  },
  
  striped = TRUE
  
  )
  
  # ---------------------------------------------------------------------------------------------------------------
  
  # Map of where data comes from
  
  output$map <- leaflet::renderLeaflet({
    
    # Filter the data to include data points which have long and lat available
    coord_data <- data() %>%
      tidyr::drop_na(Longitude, Latitude) %>%
      dplyr::filter(Longitude != "." & Latitude != ".")
    
    # Make leaflet map
    leaflet::leaflet(data = coord_data) %>%
      leaflet::addTiles() %>% # default basemap
      leaflet::addCircleMarkers(lng = ~Longitude, lat = ~Latitude, # identify columns in dataframe containing coords
                                label = ~as.character(Observation_ID), labelOptions = labelOptions(textsize = "15px"), # add labels to points and make text bigger
                                clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)) # cluster large numbers of markers
    
  })
  
  # Total number of data points
  total_data_points <- shiny::reactive({
    
    nrow(data())
    
  })
  
  # Number of data points with co-ordinates available
  data_with_coords <- shiny::reactive({
    
    data_with_coords <- total_data_points() - (sum(is.na(data()$Longitude) | data()$Longitude == "."))
    
  })
  
  # Add map figure legend
  output$map_figure_legend <- shiny::renderText({
    base::paste("<b>Figure 1.</b>", "Map showing location of data points. Currently,", data_with_coords(), "out of", total_data_points(), 
    "data points have latitude and longitude co-ordinates provided to enable them to be plotted.<br>
    Often, clusters of data points have the same co-ordinates. You can zoom in on, and click on, data clusters to explore the map.")
  })
  
  # ---------------------------------------------------------------------------------------------------------------
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### Run models tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  # ----------------------------------------------------------------------------------------------------
  
  # Make reactive IUCN threat category choices
  output$reactive_iucn_threat_category <- shiny::renderUI({
    shinyWidgets::pickerInput(inputId = "iucn_threat_category",
                              label = "IUCN Threat:",
                              choices = unique(data()$IUCN_threat_category_1),
                              selected = NULL,
                              multiple = TRUE) # add actions box for selecting/de-selecting all options
  })
  
  # Make reactive location choices
  output$reactive_location <- shiny::renderUI({
    shinyWidgets::pickerInput(inputId = "location",
                              label = "Location(s):",
                              choices = unique(data()$Country),
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE)) # add actions box for selecting/de-selecting all options
  })
  
  # Make reactive taxa order choices
  output$reactive_taxa_order <- shiny::renderUI({
    shinyWidgets::pickerInput(inputId = "taxa_order",
                              label = "Taxonomic order(s):",
                              choices = unique(data()$Order),
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE))
  })
  
  # Make reactive biodiversity metric choices
  output$reactive_biodiversity_metric_category <- shiny::renderUI({
    shinyWidgets::pickerInput(inputId = "biodiversity_metric_category",
                              label = "Biodiversity metric(s):",
                              choices = unique(data()$Biodiversity_metric),
                              selected = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE))
  })
  
  # ----------------------------------------------------------------------------------------------------
  
  ### Run model
  
  # Filter the data based on user input and run model once the run model button has been pressed
  custom_model <- shiny::eventReactive(input$run_custom_model, {
    
    shiny::validate(
      shiny::need(input$iucn_threat_category != "", "Please select at least one threat category."),
      shiny::need(input$location != "", "Please select at least one location."),
      shiny::need(input$taxa_order != "", "Please select at least one taxonomic order."),
      shiny::need(input$biodiversity_metric_category != "", "Please select at least one biodiveristy metric category.")
    )
    
    # Filter the data based on the studies the user wants to run the model on
    custom_model_data <- data() %>%
      dplyr::filter(IUCN_threat_category_1 %in% input$iucn_threat_category) %>%
      dplyr::filter(Country %in% input$location) %>%
      dplyr::filter(Order %in% input$taxa_order) %>%
      dplyr::filter(Biodiversity_metric %in% input$biodiversity_metric_category)
    
    # Try to run the model on the currently selected subset of data. If doesn't work, tell user to include more data.
    base::tryCatch(
      expr = {
        
        # add small value to control and treatment columns
        custom_model_data$Treatment_Mean <- custom_model_data$Treatment_Mean + 0.1
        custom_model_data$Control_Mean <- custom_model_data$Control_Mean + 0.1
        
        # calculate effect sizes from number, mean, and SD - data needs to be in wide format
        # Adds yi and vi columns to data
        custom_model_data <- metafor::escalc(measure = "ROM", # log transformed ratio of means (i.e. log response ratio)
                                             n1i = custom_model_data$Treatment_N,
                                             n2i = custom_model_data$Control_N,
                                             m1i = custom_model_data$Treatment_Mean,
                                             m2i = custom_model_data$Control_Mean,
                                             sd1i = custom_model_data$Treatment_error,
                                             sd2i = custom_model_data$Control_error,
                                             slab = paste(Paper_ID), # slab adds study labels which will help when we make forest plot
                                             data = custom_model_data)
        
        
        # Run metafor model
        custom_meta_model <- metafor::rma.mv(yi, vi, # effect sizes and corresponding variances
                                             random = ~ 1 | Paper_ID/Observation_ID, # specify random-effects structure of model
                                             data = custom_model_data)
        
        # If model successfully runs, enable the download results buttons
        shinyjs::enable("download_custom_model_output")
        shinyjs::enable("download_custom_model_object")
        #shinyjs::enable("download_custom_model_coeffs")
        
        ### Assign additional attributes to the model object (so if user downloads the rds model object,
        ### they would be able to see exactly what they did last time, and repeat it).
        ### Access attributes with attributes() function
        
        # Date and time model ran
        base::attr(custom_meta_model, "date_and_time") <- base::Sys.time()
        # Data filters
        base::attr(custom_meta_model, "data_filters_IUCN_threat") <- c("IUCN threat category: ", input$iucn_threat_category)
        base::attr(custom_meta_model, "data_filters_locations") <- c("Location(s): ", input$location)
        base::attr(custom_meta_model, "data_filters_taxonomic_orders") <- c("Taxonomic order(s): ", input$taxa_order)
        base::attr(custom_meta_model, "data_filters_biodiversity_metric") <- c("Biodiversity_metric: ", input$biodiversity_metric_category)
        # Session info
        base::attr(custom_meta_model, "session_info") <- utils::sessionInfo()
        
        
        custom_model <- custom_meta_model
        
      }, error = function(e) {
        
        # If model does not successfully run, make sure download results buttons are disabled
        shinyjs::disable("download_custom_model_output")
        shinyjs::disable("download_custom_model_object")
        #shinyjs::disable("download_custom_model_coeffs")
        
        # Then stop the process, and return this error message
        base::stop(shiny::safeError("There is currently insufficient data for this model to run, please select an additional or alternative biodiversity metric category."))
      })
    
  })
  
  # ----------------------------------------------------------------------------------------------------
  
  # Custom model summary
  
  custom_model_summary <- shiny::reactive({
    
    shiny::req(custom_model())
    
    custom_model_summary <- utils::capture.output(base::summary(custom_model())) # capture.output allows it to be put into a txt file that the user can download
    
  })
  
  # ---------------------------------------------------------------------------------------------------
  
  ### Plotting custom model graph
  
  output$custom_model_figure <- shiny::renderPlot({
    
    shiny::req(custom_model())
    
    forest <- metafor::forest(custom_model(),
                              xlim = c(-12, 8), # horizontal limits of the plot region
                              ilab = base::cbind(Treatment), # add in info on treatment used
                              ilab.xpos = -8, # position treatment labels
                              order = Treatment, # Order results by treatment
                              cex = 1.5,
                              col = "#0483A4", # change colour of overall effect size diamond using CEH hero colour
                              mlab = "RE Model for All Studies",
                              header = "Author(s) and Year")
    
  })
  
  # Produce figure legend
  output$custom_model_figure_legend <- shiny::renderText({
    
    shiny::req(custom_model())
    
    percentage_change <- round(100 * (exp(stats::coef(custom_model())) - 1), digits = 2)
    
    paste("<b>Figure 2. </b>", "Forest plot showing the effect sizes for each data point and the overall effect size of ",
          paste(shiny::isolate(input$iucn_threat_category), collapse = ", "), " on biodiversity. The overall effect size is indicated by the diamond -
          the centre of the diamond on the x-axis represents the point estimate,
          with its width representing the 95% confidence interval. The specific ",
          paste(shiny::isolate(input$iucn_threat_category)), " type is listed next to each data point. <br>",
          "The overall effect size of ", paste(shiny::isolate(input$iucn_threat_category), collapse = ", "), " on biodiversity for ",
          paste(shiny::isolate(input$taxa_order), collapse = ", "), " in ", paste(shiny::isolate(input$location), collapse = ", "), " measured with ",
          paste(shiny::isolate(input$biodiversity_metric_category), collapse = ", "), " as the biodiversity metric is ", round(stats::coef(custom_model()), digits = 2),
          ". This equates to a percentage change of ", percentage_change, "%.",
          sep = "")
  })
  
  # ----------------------------------------------------------------------------------------------------------------
  
  ### Downloading the custom R model output and object 
  
  # Disable the download buttons on page load - so can't click it until a model has successfully run
  shinyjs::disable("download_custom_model_output")
  shinyjs::disable("download_custom_model_object")
  
  # Disable the download buttons if the iucn_threat_category choice changes
  observeEvent(input$iucn_threat_category, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
  })
  
  # Disable the download buttons if the location choice changes
  shiny::observeEvent(input$location, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
  })
  
  # Disable the download buttons if the taxa_order choice changes
  shiny::observeEvent(input$taxa_order, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
  })
  
  # Disable the download buttons if the biodiversity_metric_category choice changes
  shiny::observeEvent(input$biodiversity_metric_category, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
  })
  
  # Download custom model output (txt file) button
  output$download_custom_model_output <- shiny::downloadHandler(
    filename = function() {
      paste0("custom_model_output", base::Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      utils::write.table(custom_model_summary(), file)
    }
  )
  
  # Download custom model object (rds file) button
  output$download_custom_model_object <- shiny::downloadHandler(
    filename = function() {
      paste0("custom_model_object", base::Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      base::saveRDS(custom_model(), file)
    }
  )
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### References tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  ### References table
  
  # Add table legend
  output$references_table_legend <- shiny::renderText({
    paste("<b>Table 3.</b>", "References for all papers that contribute data to the analysis.")
  })
  
  # Add references table
  output$references_table <- DT::renderDT({
    
    # Keep all columns - not just the Paper_ID
    references_table <- data() %>%
      dplyr::distinct(Paper_ID, .keep_all = TRUE)
    
    # Now just include the columns we want, and arrange in alphabetical order
    references_table <- references_table[, c("Author", "Year", "Title", "DOI")] %>%
      dplyr::arrange(Author)
    
  },
  
  options = list(
    scrollX = TRUE, # allow scrolling if too wide to fit all columns on one page
    autoWidth = TRUE, # use smart column width handling
    pageLength = 5, # show 5 entries per page
    headerCallback = DT::JS( # use this to alter the font size of the column names
      "function(thead) {",
      "  $(thead).css('font-size', '1em');",
      "}"
    )),
  
  rownames = FALSE # stops it adding column for row numbers
  
  )
  
  # =================================================================================================================
  # =================================================================================================================
  
}


