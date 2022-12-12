# Need to remove bits not relevant to package - e.g. googlesheets not needed
# import packages at start
# other roxygen comments

# Original way I read data in when 1st started developing the app
# robust_coefficients_all <- reactive({
#   read.csv("data_and_models/robust_coefficients.csv", row.names = 1)
# })


server <- function(input, output) {
  
  # =================================================================================================================
  # =================================================================================================================
  
  # Load data that will be use throughout server.R file
  
  # =================================================================================================================
  # =================================================================================================================
  
  # -------------------------------------------------------------------------------------
  
  # Load data from Google Sheets
  
  sheets_names <- reactive({
    # Get a list of the sheets present within the shiny_app_data googlesheet
    sheets_names <- sheet_names(sheet_id)
  })
  
  list_of_data <- reactive({
    # Read each of the sheets and load their data. Puts them into a list of dataframes
    list_of_data <- lapply(sheets_names(), function (x) read_sheet(ss = sheet_id, sheet = x))
  })
  
  number_of_studies <- reactive({
    # Count the number of sheets present
    number_of_studies <- length(list_of_data())
  })
  
  data <- reactive({
    # The initial data is Daero's data
    data <- list_of_data()[[1]]
    
    # Use a for loop to rbind all the sheets together into 1 dataframe (if more than 1 present)
    if (number_of_studies() > 1) {
      for (i in 2:number_of_studies()) {
        data <- rbind(data, list_of_data()[[i]])
      }
    }
    
    data <- data
  })
  
  # -------------------------------------------------------------------------------------
  
  #### Change later?
  # Get table of sample sizes - how many instances do we have of each agricultural system? Used later on to add Conventional back into tables.
  sample_sizes <- reactive({
    sample_sizes <- as.data.frame(table(data()$Treatment))
    colnames(sample_sizes) <- c("Treatment", "Frequency")
    
    sample_sizes <- sample_sizes
  })
  
  # -------------------------------------------------------------------------------------
  
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### Intro tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  # ---------------------------------------------------------------------------------------------------------------
  
  ### Table for overview of papers included (in intro tab)
  
  # Add table legend
  output$table_legend_overview <- renderText({
    paste("<b>Table 1.</b>", "Total number of papers and data points available to investigate each threat category.")
  })
  
  # Add sample size table
  output$sample_sizes_overview <- renderTable({
    
    # Initialise empty data frame
    sample_sizes_table <- data.frame(Threat_category = character(), Number_of_papers = numeric(), Total_data_points = numeric())
    
    # Calculate statistics for each paper
    for (i in unique(data()$IUCN_threat_category_1)) { # data is whole spreadsheet
      
      threat_subset <- data() %>%
        filter(IUCN_threat_category_1 %in% i)
      
      # make new row which will be added to sample_sizes_table
      new_row <- data.frame(
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
  
  ### Table for details of specific threat chosen by user (in intro tab)
  
  # Make reactive options
  output$reactive_threat <- renderUI({
    selectInput(inputId = "threat",
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
    
    selected_threat <- data() %>%
      filter(IUCN_threat_category_1 %in% input$threat) # Shows warnings if you use == rather than %in%
    
    # threat_details_table <- selected_threat
    
    threat_details_table <- data.frame(Paper = character(), Number_of_data_points = numeric())
    
    for (i in unique(selected_threat$Paper_ID)) {
      
      subset_threat_by_paper <- selected_threat %>%
        filter(Paper_ID == i)
      
      new_row <- data.frame(
        Paper = i,
        Number_of_data_points = nrow(subset_threat_by_paper)
      )
      
      threat_details_table <- rbind(threat_details_table, new_row)
    }
    
    threat_details_table <- threat_details_table %>%
      arrange(Paper)
    
    # remove _ from colnames
    colnames(threat_details_table) <- c("Paper", "Number of data points")
    
    threat_details_table <- threat_details_table
    
  },
  
  striped = TRUE
  
  )
  
  # ---------------------------------------------------------------------------------------------------------------
  
  # Map of where data comes from
  
  output$map <- renderLeaflet({
    
    # Filter the data to include data points which have long and lat available
    coord_data <- data() %>%
      drop_na(Longitude, Latitude) %>%
      filter(Longitude != "." & Latitude != ".")
    
    leaflet(data = coord_data) %>%
      addTiles() %>% # default basemap
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, # identify columns in dataframe containing coords
                       label = ~as.character(Observation_ID), labelOptions = labelOptions(textsize = "15px"), # add labels to points and make text bigger
                       clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)) # cluster large numbers of markers
    
  })
  
  # Total number of data points
  total_data_points <- reactive({
    
    nrow(data())
    
  })
  
  # Number of data points with co-ordinates available
  data_with_coords <- reactive({
    
    data_with_coords <- total_data_points() - (sum(is.na(data()$Longitude) | data()$Longitude == "."))
    
  })
  
  # Add map figure legend
  output$map_figure_legend <- renderText({
    paste("<b>Figure 1.</b>", "Map providing geographic representativeness of data included in this app based on latitude and longitude co-ordinates.
          Currently,", data_with_coords(), "out of", total_data_points(), "data points have co-ordinates provided to enable them to be plotted.<br>
          Often, data points are recorded with the same co-ordinates. Zoom in on (or click on) a cluster to explore the map.
          When you reach a zoom where a cluster represents data points all collected at the same location,
          you can hover your mouse over the points to see which study they were collected during.")
  })
  
  # ---------------------------------------------------------------------------------------------------------------
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### Run models tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  # ----------------------------------------------------------------------------------------------------
  
  # Make reactive IUCN threat category choices
  output$reactive_iucn_threat_category <- renderUI({
    pickerInput(inputId = "iucn_threat_category",
                label = "IUCN Threat:",
                choices = unique(data()$IUCN_threat_category_1),
                selected = NULL,
                multiple = TRUE) # add actions box for selecting/de-selecting all options
  })
  
  # Make reactive location choices
  output$reactive_location <- renderUI({
    pickerInput(inputId = "location",
                label = "Location(s):",
                choices = unique(data()$Country),
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE)) # add actions box for selecting/de-selecting all options
  })
  
  # Make reactive taxa order choices
  output$reactive_taxa_order <- renderUI({
    pickerInput(inputId = "taxa_order",
                label = "Taxonomic order(s):",
                choices = unique(data()$Order),
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE))
  })
  
  # Make reactive biodiversity metric choices
  output$reactive_biodiversity_metric_category <- renderUI({
    pickerInput(inputId = "biodiversity_metric_category",
                label = "Biodiversity metric(s):",
                choices = unique(data()$Biodiversity_metric),
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE))
  })
  
  # ----------------------------------------------------------------------------------------------------
  
  ### Run model
  
  # Filter the data based on user input and run model once the run model button has been pressed
  custom_model <- eventReactive(input$run_custom_model, {
    
    validate(
      need(input$iucn_threat_category != "", "Please select at least one threat category."),
      need(input$location != "", "Please select at least one location."),
      need(input$taxa_order != "", "Please select at least one taxonomic order."),
      need(input$biodiversity_metric_category != "", "Please select at least one biodiveristy metric category.")
    )
    
    # Filter the data based on the studies the user wants to run the model on
    custom_model_data <- data() %>%
      dplyr::filter(IUCN_threat_category_1 %in% input$iucn_threat_category) %>%
      dplyr::filter(Country %in% input$location) %>%
      dplyr::filter(Order %in% input$taxa_order) %>%
      dplyr::filter(Biodiversity_metric %in% input$biodiversity_metric_category)
    
    # Try to run the model on the currently selected subset of data. If doesn't work, tell user to include more data.
    tryCatch(
      expr = {
        
        # custom_model_data_1 <- custom_model_data()
        
        # add small value to control and treatment columns
        custom_model_data$Treatment_Mean <- custom_model_data$Treatment_Mean + 0.1
        custom_model_data$Control_Mean <- custom_model_data$Control_Mean + 0.1
        
        # calculate effect sizes from number, mean, and SD - data needs to be in wide format
        # Adds yi and vi columns to data
        custom_model_data <- escalc(measure = "ROM", # log transformed ratio of means (i.e. log response ratio)
                                    n1i = custom_model_data$Treatment_N,
                                    n2i = custom_model_data$Control_N,
                                    m1i = custom_model_data$Treatment_Mean,
                                    m2i = custom_model_data$Control_Mean,
                                    sd1i = custom_model_data$Treatment_error,
                                    sd2i = custom_model_data$Control_error,
                                    slab = paste(Paper_ID), # slab adds study labels which will help when we make forest plot
                                    data = custom_model_data)
        
        
        # Run metafor model
        custom_meta_model <- rma.mv(yi, vi, # effect sizes and corresponding variances
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
        attr(custom_meta_model, "date_and_time") <- Sys.time()
        # Data filters
        attr(custom_meta_model, "data_filters_IUCN_threat") <- c("IUCN threat category: ", input$iucn_threat_category)
        attr(custom_meta_model, "data_filters_locations") <- c("Location(s): ", input$location)
        attr(custom_meta_model, "data_filters_taxonomic_orders") <- c("Taxonomic order(s): ", input$taxa_order)
        attr(custom_meta_model, "data_filters_biodiversity_metric") <- c("Biodiversity_metric: ", input$biodiversity_metric_category)
        # Session info
        attr(custom_meta_model, "session_info") <- sessionInfo()
        
        
        custom_model <- custom_meta_model
        
      }, error = function(e) {
        
        # If model does not successfully run, make sure download results buttons are disabled
        shinyjs::disable("download_custom_model_output")
        shinyjs::disable("download_custom_model_object")
        #shinyjs::disable("download_custom_model_coeffs")
        
        # Then stop the process, and return this error message
        stop(safeError("There is currently insufficient data for this model to run, please select an additional or alternative biodiversity metric category."))
      })
    
  })
  
  # ----------------------------------------------------------------------------------------------------
  
  # Custom model summary
  
  custom_model_summary <- reactive({
    
    req(custom_model())
    
    custom_model_summary <- capture.output(summary(custom_model())) # capture.output allows it to be put into a txt file that the user can download
    
  })
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # ### Get min and max adjusted_LRR and percentage change values for fixing the x axis scale - change?
  #
  # # Adjusted_LRR
  # custom_max_LRR <- reactive({
  #   max(custom_model_analysis()$UCI)
  # })
  #
  # custom_min_LRR <- reactive({
  #   min(custom_model_analysis()$LCI)
  # })
  #
  # # Percentage change
  # custom_max_percent <- reactive({
  #   max(custom_model_analysis()$UCI_percent)
  # })
  #
  # custom_min_percent <- reactive({
  #   min(custom_model_analysis()$LCI_percent)
  # })
  
  # ---------------------------------------------------------------------------------------------------
  
  ### Plotting custom model graph
  
  output$custom_model_figure <- renderPlot({
    
    req(custom_model())
    
    forest <- forest(custom_model(),
                     xlim = c(-12, 8), # horizontal limits of the plot region
                     ilab = cbind(Treatment), # add in info on treatment used
                     ilab.xpos = -8, # position treatment labels
                     order = Treatment, # Order results by treatment
                     cex = 1.5,
                     col = "#0483A4", # change colour of overall effect size diamond using CEH hero colour
                     mlab = "RE Model for All Studies",
                     header = "Author(s) and Year")
    
  })
  
  # Produce figure legend
  output$custom_model_figure_legend <- renderText({
    
    req(custom_model())
    
    percentage_change <- round(100 * (exp(coef(custom_model())) - 1), digits = 2)
    
    paste("<b>Figure 3. </b>", "Forest plot showing the effect sizes for each data point and the overall effect size of ",
          paste(isolate(input$iucn_threat_category), collapse = ", "), " on biodiversity. The overall effect size is indicated by the diamond -
          the placement of the centre of the diamond on the x-axis represents the point estimate,
          and the width of the diamond represents the 95% confidence interval. The ",
          paste(isolate(input$iucn_threat_category)), " type is listed next to each data point. ",
          "The overall effect size of ", paste(isolate(input$iucn_threat_category), collapse = ", "), " on biodiversity for ",
          paste(isolate(input$taxa_order), collapse = ", "), " in ", paste(isolate(input$location), collapse = ", "), " measured with ",
          paste(isolate(input$biodiversity_metric_category), collapse = ", "), " as the biodiversity metric is ", round(coef(custom_model()), digits = 2),
          ". This equates to a percentage change of ", percentage_change, "%.",
          sep = "")
  })
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # ### Make the table and table legend for custom model
  #
  # # Produce table legend
  # output$table_legend_custom_model_output <- renderText({
  #
  #   paste("<b>Table 4. </b>", "Model coefficients extracted or calculated from the model summary. Agricultural systems with absolute t-values greater than 1.96
  #         have significantly different levels of biodiversity than the conventional agricultural system (", custom_model_analysis()[1, 13], " data points).", sep = "")
  # })
  #
  # # Produce table
  # output$custom_model_output_table <- renderTable({
  #
  #   custom_model_output_table <- custom_ordered_robust_coefficients()[c("Treatment","t", "Adjusted_LRR", "percentage_change", "Frequency")] %>%
  #     arrange(Adjusted_LRR, desc(Treatment))
  #
  #   # Remove as not going to include conventional in table anymore as doesn't make sense - just put number of data points in the legend.
  #   #custom_model_output_table <- rbind(custom_model_analysis()[1, c(1,4,5,9,13)], custom_model_output_table) # Add in row for conventional at the top of table
  #
  #   custom_model_output_table <- custom_model_output_table %>%
  #     relocate(t, .before = Frequency)
  #
  #   # Remove _ from colnames
  #   colnames(custom_model_output_table) <- c("Agricultural system", "Adjusted log response ratio", "Percentage change", "t-value", "Data points")
  #
  #   custom_model_output_table <- custom_model_output_table
  #
  # },
  #
  # striped = TRUE
  #
  # )
  
  # ----------------------------------------------------------------------------------------------------------------
  
  ### Downloading the custom R model output and object (and previously coefficients table)
  
  # Disable the download buttons on page load - so can't click it until a model has successfully run
  shinyjs::disable("download_custom_model_output")
  shinyjs::disable("download_custom_model_object")
  #shinyjs::disable("download_custom_model_coeffs")
  
  # Disable the download buttons if the iucn_threat_category choice changes
  observeEvent(input$iucn_threat_category, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    #shinyjs::disable("download_custom_model_coeffs")
  })
  
  # Disable the download buttons if the location choice changes
  observeEvent(input$location, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    #shinyjs::disable("download_custom_model_coeffs")
  })
  
  # Disable the download buttons if the taxa_order choice changes
  observeEvent(input$taxa_order, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    #shinyjs::disable("download_custom_model_coeffs")
  })
  
  # Disable the download buttons if the biodiversity_metric_category choice changes
  observeEvent(input$biodiversity_metric_category, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    #shinyjs::disable("download_custom_model_coeffs")
  })
  
  # Download custom model output (txt file) button
  output$download_custom_model_output <- downloadHandler(
    filename = function() {
      paste0("custom_model_output", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      write.table(custom_model_summary(), file)
    }
  )
  
  # Download custom model object (rds file) button
  output$download_custom_model_object <- downloadHandler(
    filename = function() {
      paste0("custom_model_object", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(custom_model(), file)
    }
  )
  
  # # Download coefficients table button
  # output$download_custom_model_coeffs <- downloadHandler(
  #   filename = function() {
  #     paste0("custom_model_coefficients", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(custom_model_analysis(), file)
  #   }
  # )
  
  
  # ----------------------------------------------------------------------------------------------------------------
  
  ### Details of agricultural systems table which shows once button is clicked, and disappears once hide button is clicked
  
  # defs_data2 <- reactiveValues() # reactiveValues function returns an object for storing reactive values
  #
  # observeEvent(input$show2, { # When "show" button is selected, the object stores the agri_systems_def data
  #   defs_data2$data <- agri_systems_def
  # })
  #
  # observeEvent(input$hide2, { # When "hide" button is selected, the object stores nothing
  #   defs_data2$data <- NULL
  # })
  #
  # output$agri_sys_defs_table2 <- renderTable({
  #   if (is.null(defs_data2$data)) { # If object isn't storing anything, return nothing (i.e. don't show table)
  #     return()
  #   } else { # Else, show the table
  #     agri_systems_def <- agri_systems_def %>%
  #       arrange(Agricultural_system)
  #   }
  #
  # },
  #
  # striped = TRUE
  #
  # )
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### Upload data tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  ### Overview:
  # The user has to input their first and second name (containing only letters, spaces, and hyphens to avoid things such as code injection)
  # Then they can upload their file (as long as it meets checks e.g. is a csv, contains correct columns, is not a duplicate) and preview it
  # Once previewed, can press submit to googlesheets to upload it to here. Get a completion message if successful.
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # Disable the browse/upload button on page load - so can't click it until user has inputted acceptable first and second name
  shinyjs::disable("preview_upload")
  
  # Also disable the upload to googlesheets button
  shinyjs::disable("upload_to_googlesheets")
  
  # Enable the browse/upload button if acceptable names have been inputted, otherwise disable it (and disable upload to googlesheets button too)
  observe({
    if (input$first_name != "" &&
        input$second_name != "" &&
        !str_detect(input$first_name, "[^A-Za-z-\\s]") && # name can only contain letters, hyphens, or spaces
        !str_detect(input$second_name, "[^A-Za-z-\\s]")) {
      
      # enable the browse/upload button
      shinyjs::enable("preview_upload")
    } else {
      shinyjs::disable("preview_upload")
      shinyjs::disable("upload_to_googlesheets")
    }
  })
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # The following 3 observeEvents hide the output$upload_complete message
  # (which indicates that a data sheet has been successfully uploaded to googlesheets) if the inputted names or file changes
  # Also disables the upload_to_googlesheets button
  
  observeEvent(input$first_name, {
    shinyjs::hide("upload_complete")
    shinyjs::disable("upload_to_googlesheets")
  })
  
  observeEvent(input$second_name, {
    shinyjs::hide("upload_complete")
    shinyjs::disable("upload_to_googlesheets")
  })
  
  observeEvent(input$preview_upload, {
    shinyjs::hide("upload_complete")
    shinyjs::disable("upload_to_googlesheets")
  })
  
  # ----------------------------------------------------------------------------------------------------------------
  
  ### Validation checks on the names inputted which gives feedback to the user
  # Same as checks which enable the browse button, BUT needed in additional to provide user feedback
  
  output$name_inputted <- renderText({
    
    validate(
      
      # First name must not be empty
      need(input$first_name != "", "First name must not be empty."),
      
      # Second name must not be empty
      need(input$second_name != "", "Second name must not be empty."),
      
      # First_name must only contain letters
      need(!str_detect(input$first_name, "[^A-Za-z-\\s]"), "First name can only contain letters."), # if contains anything but letters, hyphens, or spaces, it returns FALSE
      
      # Second_name must only contain letters
      need(!str_detect(input$second_name, "[^A-Za-z-\\s]"), "Second name can only contain letters.") # if contains anything but letters, hyphens, or spaces, it returns FALSE
      
    )
    
    paste("") # return blank message if name meets criteria
    
  })
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # Preview the data to be uploaded to googlesheets and enable upload_to_googlesheets button if certain checks are met
  output$preview_upload_data <- renderTable({
    
    req(input$preview_upload)
    
    # Validation checks 1
    validate(
      # Make sure file is a csv
      need(tools::file_ext(input$preview_upload$name) == "csv", "File must be a .csv.")
    )
    
    # If it is a .csv, change blanks to NAs so matches the googlesheet when it is read in
    new_data <- read.csv(input$preview_upload$datapath)
    new_data[new_data == ""] <- NA
    
    # Validation checks 2
    validate(
      # Make sure file has certain columns - these are the columns in Daero's standardised data sheet
      
      # Version 1: New data must contain all colnames in standardised sheet, but can also contain extra ones
      # need(all(colnames(data()) %in% colnames(new_data)), "Data doesn't contain correct column(s).")
      
      # Version 2: Colnames have to be exactly the same - tell user which columns in their dataframe are missing, and which shouldn't be there
      need(all(colnames(data()) == colnames(new_data)), paste("Data doesn't contain correct column(s).\nMissing columns that must be present: ",
                                                              paste(setdiff(colnames(data()), colnames(new_data)), collapse = ", "),
                                                              "\nExtra columns that need to be removed: ", paste(setdiff(colnames(new_data), colnames(data())), collapse = ", ")))
    )
    
    # If passes the checks above, also want to check it is not a duplicate of data already in googlesheets
    # Read in the the sheets present within the googlesheet
    sheets_names <- sheet_names(sheet_id)
    
    # Read each of the sheets and load their data. Puts them into a list of dataframes
    list_of_data <- lapply(sheets_names, function (x) read_sheet(ss = sheet_id, sheet = x))
    
    # Count the number of sheets present
    number_of_studies <- length(list_of_data)
    
    # Check if any of these are identical to the one the user is trying to upload - output needs to only contain FALSE
    output <- c()
    if (number_of_studies > 1) {
      for (i in 1:number_of_studies) {
        output <- c(output, (isTRUE(all.equal(new_data, as.data.frame(list_of_data[[i]])))))
      }
    } else {
      output <- c(output, isTRUE(all.equal(new_data, as.data.frame(list_of_data[[1]]))))
    }
    
    # Validation checks 3
    validate(
      # Make sure file is not a duplicate of one already in the googlesheets
      need(all(output == FALSE), "This is a duplicate of a dataframe already in existence") # Check if they are all FALSE i.e. no identical dataframes
    )
    
    # Checks needed to enable to upload to googlesheets button - need all checks to be met (inputted name and file)
    if (input$first_name != "" &&
        input$second_name != "" &&
        !str_detect(input$first_name, "[^A-Za-z-\\s]") &&
        !str_detect(input$second_name, "[^A-Za-z-\\s]") &&
        tools::file_ext(input$preview_upload$name) == "csv" &&
        # all(colnames(data()) %in% colnames(read.csv(input$preview_upload$datapath))) &&
        all(colnames(data()) == colnames(read.csv(input$preview_upload$datapath))) &&
        all(output == FALSE)) {
      # enable the browse/upload button
      shinyjs::enable("upload_to_googlesheets")
    }
    
    # Produce the table
    preview_upload_data <- head(new_data)
    
  },
  
  striped = TRUE
  
  )
  
  # Add table legend
  output$preview_upload_data_legend <- renderText({
    
    req(input$preview_upload)
    
    paste("<b>Table 5.</b>", "Preview of your data, which you can upload to the database.")
  })
  
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # Upon acceptable name being entered, and selecting an acceptable file, save it to googlesheets, and return a message if successful
  
  observeEvent(input$upload_to_googlesheets, {
    
    # Access actual data via datapath column of the dataframe returned by fileInput()
    data <- read.csv(input$preview_upload$datapath)
    data[data == ""] <- NA
    
    # Create a unique file name
    file_name <- paste0(input$first_name, "_", input$second_name, "_", as.integer(Sys.time()), sep = "")
    
    # Create a filepath using this file_name
    file_path <- file.path(tempdir(), file_name)
    
    # Write the data to a temporary file locally
    write.csv(data, file_path, row.names = FALSE)
    
    # Add new tab to gogglesheet
    sheet_add(ss = sheet_id, sheet = file_name)
    
    # Add new data to this sheet
    write_sheet(data, ss = sheet_id, sheet = file_name)
    
    shinyjs::show("upload_complete")
    
    output$upload_complete <- renderText({
      
      paste0("File with name: ", input$first_name, " ", input$second_name, " ", input$preview_upload$name, " successfully uploaded.")
      
    })
    
    shinyjs::disable("upload_to_googlesheets")
    
  })
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### References tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  ### References table
  
  # Add table legend
  output$references_table_legend <- renderText({
    paste("<b>Table 6.</b>", "References for all papers included in the app.")
  })
  
  # Add references table
  output$references_table <- renderTable({
    
    # Keep all columns - not just the Paper_ID
    references_table <- data() %>%
      distinct(Paper_ID, .keep_all = TRUE)
    
    # Now just include the columns we want, and arrange in alphabetical order
    references_table <- references_table[, c("Author", "Year", "Title", "DOI")] %>%
      arrange(Author)
    
    
  },
  
  digits = 0,
  striped = TRUE
  
  )
  
  # =================================================================================================================
  # =================================================================================================================
  
}