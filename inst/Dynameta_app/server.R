# # Dynameta shiny app server script 
# 
# # ---------------------------------------------------------------------------------------------
# 
# # Load packages
# library(dplyr) # for data manipulation, part of tidyverse ("%>%" filter arrange relocate distinct)
# library(DT) # for interactive tables
# library(leaflet) # for mapping (renderLeaflet leaflet addTiles addCircleMarkers)
# library(metafor) # for running meta-analytic models (escalc rma.mv forest)
# library(shiny) # Required to run any Shiny app
# library(shinyjs) # for enabling and disabling download button (enable disable show)
# library(shinyWidgets) # for including a 'select all' option for filters (pickerInput)
# library(tidyr) # for tidying messy data, part of tidyverse (drop_na)
# library(readr) # for reading in csv files uploaded to the app
# library(mapview) # for downloading the leaflet map
# 
# # ---------------------------------------------------------------------------------------------
# 
# # Define the 'then' function, so it only returns the 1 error message at a time to the user 
# `%then%` <- function(a, b) {
#   if (is.null(a)) b else a
# }
# 
# # ---------------------------------------------------------------------------------------------
# 
# # Define the server function
# server <- function(input, output) {
#   
#   # =================================================================================================================
#   # =================================================================================================================
#   
#   ##### Load data that will be use throughout server.R file
#   
#   # =================================================================================================================
#   # =================================================================================================================
#   
#   # -------------------------------------------------------------------------------------
#   
#   # User choice of data to analyse
#   shiny::observeEvent(input$data_choice, {
#     
#     # If the user selects "Your own data", then 'show' the upload_data_to_analyse widget and info button  
#     if (input$data_choice == "Your own data") {
#       shinyjs::show("upload_data_to_analyse")
#       shinyjs::show("necessary_columns_info_btn")
#     } else {
#       shinyjs::hide("upload_data_to_analyse")
#       shinyjs::hide("necessary_columns_info_btn")
#     }
#     
#   })
#   
#   # Create reactive data object 
#   data <- shiny::reactive({
#     
#     # When testing the package with shinytest, need to load the data in from the data_for_shinytest directory
#     # or app can't find data like it does when user has installed the package 
#     
#     # For csv version
#     #sample_data <- read.csv("../data_for_shinytest/sample_data_for_shinytest.csv")
#     
#     # Sample data in rds file
#     sample_data <- readRDS("../data_for_shinytest/sample_data_rds")
#     
#     # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
#     if (input$data_choice == "Your own data" && !is.null(input$upload_data_to_analyse)) {
#       
#       # Get the file encoding - returns encodings 'guesses' and associated confidence values 
#       encoding_and_confidence <- readr::guess_encoding(input$upload_data_to_analyse$datapath)
#       
#       # List the columns that must be included in the dataset
#       necessary_columns <- c("Paper_ID", "Observation_ID", "Author", "Year", "Title", "DOI", "URL", "Latitude", 
#                              "Longitude", "Country", "Order", "Biodiversity_metric", "IUCN_threat_category_1", 
#                              "Treatment", "Treatment_N", "Treatment_Mean", "Treatment_error", "Control_N", 
#                              "Control_Mean", "Control_error")
#       
#       # Checks on data 1 
#       shiny::validate(
#         
#         # 1. Make sure file is a csv
#         shiny::need(tools::file_ext(input$upload_data_to_analyse$name) == "csv", "File must be a .csv.") %then%
#           
#           # 2. Make sure file encoding is either UTF-8 or ASCII and it is at least 90% confident
#           shiny::need(encoding_and_confidence[1, 1] == "UTF-8" | encoding_and_confidence[1, 1] == "ASCII" & encoding_and_confidence[1, 2] >= 0.9, 
#                       paste0("The encoding of file you have uploaded (", encoding_and_confidence[1, 1], ") is not compatible with Dynameta. Ensure the file has UTF-8 or ASCII encoding.")) %then%
#           
#           # 3. Make sure file has certain columns - these are the columns in the sample data that are needed for Dynameta (other columns can vary)
#           # Tell user which columns in their dataframe are missing (if any)
#           shiny::need(base::all(necessary_columns %in% colnames(readr::read_csv(input$upload_data_to_analyse$datapath))), 
#                       paste("Data doesn't contain correct column(s).
#                             \nMissing columns that must be present: ",
#                             paste(setdiff(necessary_columns, colnames(readr::read_csv(input$upload_data_to_analyse$datapath))), collapse = ", ")))
#         
#         
#       )
#       
#       data <- readr::read_csv(input$upload_data_to_analyse$datapath)
#       
#       # Else if no data uploaded, or user has selected the sample data, then the data is the sample data
#     } else {
#       data <- sample_data
#     }
#     
#   })
#   
#   ### Clickable info button for details on what columns must be present in the dataframe 
#   shiny::observeEvent(input$necessary_columns_info_btn, {
#     
#     # necessary_columns <- c("Paper_ID", "Observation_ID", "Author", "Year", "Title", "DOI", "URL", "Latitude", 
#     #                        "Longitude", "Country", "Order", "Biodiversity_metric", "IUCN_threat_category_1", 
#     #                        "Treatment", "Treatment_N", "Treatment_Mean", "Treatment_error", "Control_N", 
#     #                        "Control_Mean", "Control_error", sep="<br>")
#     
#     
#     shiny::showModal(
#       shiny::modalDialog(
#         title = "Columns that must be included in your dataframe",
#         size = "l",
#         HTML("<b>Paper_ID</b> - The paper from which the comparison was drawn. (character)<br>
#             <b>Observation_ID</b> - The comparison of that row, which should be unique to each row. (character)<br>
#             <b>Author</b> - The full list of authors of the paper. (character)<br>
#             <b>Year</b> - The year of publication of the paper. (integer)<br>
#             <b>Title</b> - The paper title. (character)<br>
#             <b>DOI</b> - The DOI of the paper. (character)<br>
#             <b>URL</b> - The URL of the paper. (character)<br>
#             <b>Latitude</b> - The latitude coordinate of the observation in that row. (double)<br>
#             <b>Longitude</b> - The longitude coordinate of the observation in that row. (double)<br>
#             <b>Country</b> - The country in which the observation in that row is located. (character)<br>
#             <b>Order</b> - The taxonomic order of the biodiversity measured in that row, for both the treatment and control. (character)<br>
#             <b>Biodiversity_metric</b> - The metric of biodiversity measured. Biodiversity_metric should typically be one of “Abundance”, “Richness”, or “Biomass”. (character)<br>
#             <b>IUCN_threat_category_1</b> - Broadest level of IUCN threat. (character)<br>
#             <b>Treatment</b> - Name of the threat measured in that row (e.g. insecticide). (character)<br>
#             <b>Treatment_N</b> - The number of treatment sites from which the mean and error values were drawn. (integer)<br>
#             <b>Treatment_Mean</b> - The mean biodiversity value across all treatment sites for that comparison. (double)<br>
#             <b>Treatment_error</b> - The raw treatment error value reported by the authors for that observation. (double)<br>
#             <b>Control_N</b> - The number of control sites from which the mean and error values were drawn. (integer)<br>
#             <b>Control_Mean</b> - The mean biodiversity value across all control sites for that comparison. (double)<br>
#             <b>Control_error</b> - The raw control error value reported by the authors for that observation. (double)<br><br>
#             Your data can include columns for additional variables that you specified you would collect in your protocol. "),
#         easyClose = TRUE
#       )
#     )
#   })
#   
#   # -------------------------------------------------------------------------------------
#   
#   # =================================================================================================================
#   # =================================================================================================================
#   
#   ##### Intro tab
#   
#   # =================================================================================================================
#   # =================================================================================================================
#   
#   # ---------------------------------------------------------------------------------------------------------------
#   
#   ### Table for overview of papers included 
#   
#   # Add table legend
#   output$table_legend_overview <- shiny::renderText({
#     base::paste("<b>Table 1.</b>", "Total number of papers and data points available to investigate each threat category.")
#   })
#   
#   # Add sample size table
#   output$sample_sizes_overview <- shiny::renderTable({
#     
#     # Initialise empty data frame
#     sample_sizes_table <- base::data.frame(Threat_category = character(), Number_of_papers = numeric(), Total_data_points = numeric())
#     
#     # Calculate statistics for each paper
#     for (i in unique(data()$IUCN_threat_category_1)) { # data is whole spreadsheet
#       
#       threat_subset <- data() %>%
#         dplyr::filter(IUCN_threat_category_1 %in% i)
#       
#       # make new row which will be added to sample_sizes_table
#       new_row <- base::data.frame(
#         Threat_category = i,
#         Number_of_papers = length(unique(threat_subset$Paper_ID)), # number of unique agricultural systems
#         Total_data_points = nrow(threat_subset) # number of instances
#       )
#       
#       sample_sizes_table <- rbind(sample_sizes_table, new_row)
#       
#     }
#     
#     # Put table in alphabetical order based on paper_ID
#     sample_sizes_table <- sample_sizes_table %>%
#       arrange(Threat_category)
#     
#     # remove _ from colnames
#     colnames(sample_sizes_table) <- c("Threat category", "Number of papers", "Total data points")
#     
#     sample_sizes_table <- sample_sizes_table
#     
#   },
#   
#   striped = TRUE
#   
#   )
#   
#   # ---------------------------------------------------------------------------------------------------------------
#   
#   ### Text for data summary
#   
#   # Add data summary text
#   output$data_summary_text <- renderText({
#     
#     # Calculate number of rows and columns
#     nrow <- nrow(data())
#     ncol <- ncol(data())
#     
#     # Calculate percentage of treatment means and control means that are close to 0 (below 0.5)
#     treatment_close_to_zero <- round((sum(data()$Treatment_Mean < 0.5)/nrow(data())) * 100, digits = 1)
#     control_close_to_zero <- round((sum(data()$Control_Mean < 0.5)/nrow(data())) * 100, digits = 1)
#     
#     # Get list of column names
#     colnames <- paste(colnames(data()), collapse = "<br>")
#     
#     paste0("<b>Summary of the data you will analyse using Dynameta.</b><br>", 
#            "Your data has ", nrow, " rows and ", ncol, " columns.<br><br>
#            It is worth noting that if your treatment and control mean values are close to zero, it can affect the accuracy of the variance estimation for a model (see ",
#            tags$a(href="https://esajournals.onlinelibrary.wiley.com/doi/10.1890/14-2402.1", "Lajeunesse, 2015"), " for more information). ",
#            treatment_close_to_zero, "% and ", control_close_to_zero, "% of your treatment and control means, respectively, are below 0.5. <br><br>",
#            "The column names that your data contains are as follows:<br>", colnames, sep="")
#   })
# 
#   
#   # ---------------------------------------------------------------------------------------------------------------
#   
#   # Map of where data comes from
#   # Make reactive object:
#   leaflet_map <- reactive({
#     
#     # Filter the data to include data points which have long and lat available
#     coord_data <- data() %>%
#       tidyr::drop_na(Longitude, Latitude) %>%
#       dplyr::filter(Longitude != "." & Latitude != ".")
#     
#     # # Make leaflet map
#     # leaflet::leaflet(data = coord_data) %>%
#     #   leaflet::addTiles() %>% # default basemap
#     #   leaflet::addCircleMarkers(lng = ~Longitude, lat = ~Latitude, # identify columns in dataframe containing coords
#     #                             label = ~as.character(Observation_ID), labelOptions = labelOptions(textsize = "15px"), # add labels to points and make text bigger
#     #                             clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)) # cluster large numbers of markers
#     
#     # Make leaflet map
#     leaflet_map <- leaflet::leaflet(data = coord_data) %>%
#       leaflet::addTiles() %>% # default basemap
#       leaflet::addCircleMarkers(lng = ~Longitude, lat = ~Latitude, # identify columns in dataframe containing coords
#                                 label = ~as.character(Observation_ID), 
#                                 labelOptions = labelOptions(textsize = "15px"), # add labels to points and make text bigger
#                                 #popup = '<a href=~URL>Link to paper</a>', # Trying to add clickable link to paper but not working
#                                 popup = ~as.character(URL), # You can copy and paste the address with this one
#                                 clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)) # cluster large numbers of markers
#     
#   })
#   
#   
#   output$map <- leaflet::renderLeaflet({
#     
#     req(leaflet_map())
#     
#     map <- leaflet_map()
#     
#   })
#   
#   # output$map <- leaflet::renderLeaflet({
#   #   
#   #   # Filter the data to include data points which have long and lat available
#   #   coord_data <- data() %>%
#   #     tidyr::drop_na(Longitude, Latitude) %>%
#   #     dplyr::filter(Longitude != "." & Latitude != ".")
#   #   
#   #   # # Make leaflet map
#   #   # leaflet::leaflet(data = coord_data) %>%
#   #   #   leaflet::addTiles() %>% # default basemap
#   #   #   leaflet::addCircleMarkers(lng = ~Longitude, lat = ~Latitude, # identify columns in dataframe containing coords
#   #   #                             label = ~as.character(Observation_ID), labelOptions = labelOptions(textsize = "15px"), # add labels to points and make text bigger
#   #   #                             clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)) # cluster large numbers of markers
#   #   
#   #   # Make leaflet map
#   #   leaflet::leaflet(data = coord_data) %>%
#   #     leaflet::addTiles() %>% # default basemap
#   #     leaflet::addCircleMarkers(lng = ~Longitude, lat = ~Latitude, # identify columns in dataframe containing coords
#   #                               label = ~as.character(Observation_ID), 
#   #                               labelOptions = labelOptions(textsize = "15px"), # add labels to points and make text bigger
#   #                               #popup = '<a href=~URL>Link to paper</a>', # Trying to add clickable link to paper but not working
#   #                               popup = ~as.character(URL), # You can copy and paste the address with this one
#   #                               clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)) # cluster large numbers of markers
#   #   
#   #   
#   #   
#   # })
#   
#   
#   # Calculate total number of data points
#   total_data_points <- shiny::reactive({
#     
#     nrow(data())
#     
#   })
#   
#   # Calculate number of data points with co-ordinates available
#   data_with_coords <- shiny::reactive({
#     
#     data_with_coords <- total_data_points() - (sum(is.na(data()$Longitude) | data()$Longitude == "."))
#     
#   })
#   
#   # Add map figure legend
#   output$map_figure_legend <- shiny::renderText({
#     base::paste("<b>Figure 1.</b>", "Map showing location of data points. Currently,", data_with_coords(), "out of", total_data_points(), 
#                 "data points have latitude and longitude co-ordinates provided to enable them to be plotted.<br>
#     Often, clusters of data points have the same co-ordinates. You can zoom in on, and click on, data clusters to explore the map.")
#   })
#   
#   # Download map button 
#   output$download_map <- downloadHandler(
#     
#     filename = function() {
#       paste0("map_from_Dynameta_", Sys.Date(), ".png", sep="")
#     },
#     
#     content = function(file) {
#       mapview::mapshot(x = leaflet_map(),
#                        file = file, 
#                        cliprect = "viewport", # the clipping rectangle matches the height & width from the viewing port
#                        selfcontained = FALSE) # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
#     }
#   ) 
#   
#   
#   # ---------------------------------------------------------------------------------------------------------------
#   
#   # =================================================================================================================
#   # =================================================================================================================
#   
#   ##### Run models tab
#   
#   # =================================================================================================================
#   # =================================================================================================================
#   
#   # ----------------------------------------------------------------------------------------------------
#   
#   # Make reactive IUCN threat category choices
#   output$reactive_iucn_threat_category <- shiny::renderUI({
#     shinyWidgets::pickerInput(inputId = "iucn_threat_category",
#                               label = "IUCN Threat:",
#                               choices = unique(data()$IUCN_threat_category_1),
#                               selected = NULL,
#                               multiple = TRUE) # add actions box for selecting/de-selecting all options
#   })
#   
#   # Make reactive location choices
#   output$reactive_location <- shiny::renderUI({
#     shinyWidgets::pickerInput(inputId = "location",
#                               label = "Location(s):",
#                               choices = unique(data()$Country),
#                               selected = NULL,
#                               multiple = TRUE,
#                               options = list(`actions-box` = TRUE)) # add actions box for selecting/de-selecting all options
#   })
#   
#   # Make reactive taxa order choices
#   output$reactive_taxa_order <- shiny::renderUI({
#     shinyWidgets::pickerInput(inputId = "taxa_order",
#                               label = "Taxonomic order(s):",
#                               choices = unique(data()$Order),
#                               selected = NULL,
#                               multiple = TRUE,
#                               options = list(`actions-box` = TRUE))
#   })
#   
#   # Make reactive biodiversity metric choices
#   output$reactive_biodiversity_metric_category <- shiny::renderUI({
#     shinyWidgets::pickerInput(inputId = "biodiversity_metric_category",
#                               label = "Biodiversity metric(s):",
#                               choices = unique(data()$Biodiversity_metric),
#                               selected = NULL,
#                               multiple = TRUE,
#                               options = list(`actions-box` = TRUE))
#   })
#   
#   # ----------------------------------------------------------------------------------------------------
#   
#   ### Run model
#   
#   # Filter the data based on user input and run model once the run model button has been pressed
#   custom_model <- shiny::eventReactive(input$run_custom_model, {
#     
#     shiny::validate(
#       shiny::need(input$iucn_threat_category != "", "Please select at least one threat category."),
#       shiny::need(input$location != "", "Please select at least one location."),
#       shiny::need(input$taxa_order != "", "Please select at least one taxonomic order."),
#       shiny::need(input$biodiversity_metric_category != "", "Please select at least one biodiveristy metric category.")
#     )
#     
#     # Filter the data based on the studies the user wants to run the model on
#     custom_model_data <- data() %>%
#       dplyr::filter(IUCN_threat_category_1 %in% input$iucn_threat_category) %>%
#       dplyr::filter(Country %in% input$location) %>%
#       dplyr::filter(Order %in% input$taxa_order) %>%
#       dplyr::filter(Biodiversity_metric %in% input$biodiversity_metric_category)
#     
#     # Try to run the model on the currently selected subset of data. If doesn't work, tell user to include more data or view error message.
#     base::tryCatch(
#       expr = {
#         
#         # add small value to control and treatment columns
#         custom_model_data$Treatment_Mean <- custom_model_data$Treatment_Mean + 0.1
#         custom_model_data$Control_Mean <- custom_model_data$Control_Mean + 0.1
#         
#         # calculate effect sizes from number, mean, and SD - data needs to be in wide format
#         # Adds yi and vi columns to data
#         custom_model_data <- metafor::escalc(measure = "ROM", # log transformed ratio of means (i.e. log response ratio)
#                                              n1i = custom_model_data$Treatment_N,
#                                              n2i = custom_model_data$Control_N,
#                                              m1i = custom_model_data$Treatment_Mean,
#                                              m2i = custom_model_data$Control_Mean,
#                                              sd1i = custom_model_data$Treatment_error,
#                                              sd2i = custom_model_data$Control_error,
#                                              slab = paste(Paper_ID), # slab adds study labels which will help when we make forest plot
#                                              data = custom_model_data)
#         
#         
#         # Run metafor model
#         custom_meta_model <- metafor::rma.mv(yi, vi, # effect sizes and corresponding variances
#                                              random = ~ 1 | Paper_ID/Observation_ID, # specify random-effects structure of model
#                                              data = custom_model_data)
#         
#         # If model successfully runs, enable the download results buttons
#         shinyjs::enable("download_custom_model_output")
#         shinyjs::enable("download_custom_model_object")
#         shinyjs::enable("download_forest_plot")
#         
#         ### Assign additional attributes to the model object (so if user downloads the rds model object,
#         ### they would be able to see exactly what they did last time, and repeat it).
#         ### Access attributes with attributes() function
#         
#         # Date and time model ran
#         base::attr(custom_meta_model, "date_and_time") <- base::Sys.time()
#         # Data filters
#         base::attr(custom_meta_model, "data_filters_IUCN_threat") <- c("IUCN threat category: ", input$iucn_threat_category)
#         base::attr(custom_meta_model, "data_filters_locations") <- c("Location(s): ", input$location)
#         base::attr(custom_meta_model, "data_filters_taxonomic_orders") <- c("Taxonomic order(s): ", input$taxa_order)
#         base::attr(custom_meta_model, "data_filters_biodiversity_metric") <- c("Biodiversity_metric: ", input$biodiversity_metric_category)
#         # Session info
#         base::attr(custom_meta_model, "session_info") <- utils::sessionInfo()
#         
#         
#         custom_model <- custom_meta_model
#         
#       }, error = function(e) {
#         
#         # If model does not successfully run, make sure download results buttons are disabled
#         shinyjs::disable("download_custom_model_output")
#         shinyjs::disable("download_custom_model_object")
#         shinyjs::disable("download_forest_plot")
#         
#         # Then stop the process, and return this error message
#         base::stop(shiny::safeError(paste0("This model failed to run. This may be due to insufficient data for this model to run, but please see the R error message: ", e)))
#       })
#     
#   })
#   
#   # ----------------------------------------------------------------------------------------------------
#   
#   # Custom model summary
#   
#   custom_model_summary <- shiny::reactive({
#     
#     shiny::req(custom_model())
#     
#     custom_model_summary <- utils::capture.output(base::summary(custom_model())) # capture.output allows it to be put into a txt file that the user can download
#     
#   })
#   
#   # ---------------------------------------------------------------------------------------------------
#   
#   ### Plotting custom model graph
#   
#   # Make plot a reactive object 
#   figure <- reactive({
#     
#     shiny::req(custom_model())
#     
#     figure <- metafor::forest(custom_model(),
#                               xlim = c(-12, 8), # horizontal limits of the plot region
#                               ilab = base::cbind(Treatment), # add in info on treatment used
#                               ilab.xpos = -8, # position treatment labels
#                               order = Treatment, # Order results by treatment
#                               cex = 1.5,
#                               col = "#0483A4", # change colour of overall effect size diamond using CEH hero colour
#                               mlab = "RE Model for All Studies",
#                               header = "Author(s) and Year")
#     
#   })
#   
#   # Render the plot in Dynameta
#   output$custom_model_figure <- shiny::renderPlot({
#     
#     shiny::req(figure())
#     
#     custom_model_figure <- figure()
#     
#   })
#   
#   # Produce figure legend
#   output$custom_model_figure_legend <- shiny::renderText({
#     
#     shiny::req(custom_model())
#     
#     # Convert LRR overall effect size to percentage
#     percentage_change <- round(100 * (exp(stats::coef(custom_model())) - 1), digits = 2)
#     
#     # Calculate confidence interval lower bound in percentage
#     ci_lb <- round(100 * (exp(custom_model()$ci.lb) - 1), digits = 2)
#     
#     # Calculate confidence interval upper bound in percentage
#     ci_ub <- round(100 * (exp(custom_model()$ci.ub) - 1), digits = 2)
#     
#     # Calculate I2 statistic. Code adapted from http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate - Multilevel Models section
#     W <- diag(1/custom_model()$vi)
#     X <- metafor::model.matrix.rma(custom_model())
#     P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
#     i2 <- round(100 * sum(custom_model()$sigma2) / (sum(custom_model()$sigma2) + (custom_model()$k-custom_model()$p)/sum(diag(P))), digits = 2) 
# 
#     # Add these stats to the paste() below.
#     
#     paste("<b>Figure 2. </b>", "Forest plot showing the effect sizes for each data point and the overall effect size of ",
#           paste(shiny::isolate(input$iucn_threat_category), collapse = ", "), " on biodiversity. The overall effect size is indicated by the diamond -
#           the centre of the diamond on the x-axis represents the point estimate,
#           with its width representing the 95% confidence interval. The specific ",
#           paste(shiny::isolate(input$iucn_threat_category)), " type is listed next to each data point. <br><br>",
#           "The overall effect size of ", paste(shiny::isolate(input$iucn_threat_category), collapse = ", "), " on biodiversity for ",
#           paste(shiny::isolate(input$taxa_order), collapse = ", "), " in ", paste(shiny::isolate(input$location), collapse = ", "), " measured with ",
#           paste(shiny::isolate(input$biodiversity_metric_category), collapse = ", "), " as the biodiversity metric is ", round(stats::coef(custom_model()), digits = 2),
#           ". This equates to a percentage change of ", percentage_change, "%", " [", ci_lb, "%, ", ci_ub, "%]. <br><br>",
#           "The I² statistic for the meta-analysis is ", i2, "%. This describes the percentage of total variance that is due to heterogeneity (variability among studies), and not due to chance. <br><br>",
#           sep = "")
#   })
#   
#   # ----------------------------------------------------------------------------------------------------------------
#   
#   ### Downloading the custom R model output, object, and forest plot 
#   
#   # Disable the download buttons on page load - so can't click it until a model has successfully run
#   shinyjs::disable("download_custom_model_output")
#   shinyjs::disable("download_custom_model_object")
#   shinyjs::disable("download_forest_plot")
#   
#   # Disable the download buttons if the iucn_threat_category choice changes
#   observeEvent(input$iucn_threat_category, {
#     shinyjs::disable("download_custom_model_output")
#     shinyjs::disable("download_custom_model_object")
#     shinyjs::disable("download_forest_plot")
#   })
#   
#   # Disable the download buttons if the location choice changes
#   shiny::observeEvent(input$location, {
#     shinyjs::disable("download_custom_model_output")
#     shinyjs::disable("download_custom_model_object")
#     shinyjs::disable("download_forest_plot")
#   })
#   
#   # Disable the download buttons if the taxa_order choice changes
#   shiny::observeEvent(input$taxa_order, {
#     shinyjs::disable("download_custom_model_output")
#     shinyjs::disable("download_custom_model_object")
#     shinyjs::disable("download_forest_plot")
#   })
#   
#   # Disable the download buttons if the biodiversity_metric_category choice changes
#   shiny::observeEvent(input$biodiversity_metric_category, {
#     shinyjs::disable("download_custom_model_output")
#     shinyjs::disable("download_custom_model_object")
#     shinyjs::disable("download_forest_plot")
#   })
#   
#   # Download custom model output (txt file) button
#   output$download_custom_model_output <- shiny::downloadHandler(
#     filename = function() {
#       paste0("custom_model_output", base::Sys.Date(), ".txt", sep="")
#     },
#     content = function(file) {
#       utils::write.table(custom_model_summary(), file)
#     }
#   )
#   
#   # Download custom model object (rds file) button
#   output$download_custom_model_object <- shiny::downloadHandler(
#     filename = function() {
#       paste0("custom_model_object", base::Sys.Date(), ".rds", sep="")
#     },
#     content = function(file) {
#       base::saveRDS(custom_model(), file)
#     }
#   )
#   
#   # Download forest plot button
#   output$download_forest_plot <- shiny::downloadHandler(
#     
#     filename = function() {
#       paste0("forest_plot", base::Sys.Date(), ".png", sep="")
#     },
#     content = function(file) {
#       grDevices::png(file, width = 1500, height = 1000)
#       metafor::forest(custom_model(),
#                       xlim = c(-12, 8), # horizontal limits of the plot region
#                       ilab = base::cbind(Treatment), # add in info on treatment used
#                       ilab.xpos = -8, # position treatment labels
#                       order = Treatment, # Order results by treatment
#                       cex = 1.5,
#                       col = "#0483A4", # change colour of overall effect size diamond using CEH hero colour
#                       mlab = "RE Model for All Studies",
#                       header = "Author(s) and Year")
#       grDevices::dev.off()
#     }
# 
#   )
#   
#   
#   # ----------------------------------------------------------------------------------------------------------------
#   
#   # =================================================================================================================
#   # =================================================================================================================
#   
#   ##### References tab
#   
#   # =================================================================================================================
#   # =================================================================================================================
#   
#   ### References table
#   
#   # Add table legend
#   output$references_table_legend <- shiny::renderText({
#     paste("<b>Table 2.</b>", "References for all papers that contribute data to the analysis.")
#   })
# 
#   # Make reactive references table
#   references_table_object <- shiny::reactive({
#     
#     # Initialise empty data frame to count data points per Paper_ID and which threat(s) the paper investigated
#     all_details <- base::data.frame(Paper_ID = character(), Total_data_points = numeric(), Threat = character())
#     
#     # Fill in dateframe
#     for (i in unique(data()$Paper_ID)) {
#       
#       # Filter for each paper
#       paper_subset <- data() %>%
#         dplyr::filter(Paper_ID == i)
#       
#       # Count number of rows for each paper
#       paper_details = base::data.frame(
#         Paper_ID = i,
#         Total_data_points = nrow(paper_subset),
#         Threat = unique(paper_subset$IUCN_threat_category_1)
#       )
#       
#       # Add row to dataframe
#       all_details <- rbind(all_details, paper_details)
#     }
#     
#     
#     # Make reference table including unique Paper_IDs (keeping all columns)
#     references_table_object <- data() %>%
#       dplyr::distinct(Paper_ID, .keep_all = TRUE)
#     
#     # Add data points and threats columns
#     references_table_object <- base::merge(references_table_object, all_details, by = "Paper_ID")
#     
#     # Now just include the columns we want, and arrange in alphabetical order
#     references_table_object <- references_table_object[, c("Author", "Year", "Title", "DOI", "Threat", "Total_data_points")] %>%
#       dplyr::rename("Data points" = "Total_data_points") %>%
#       dplyr::rename("Threat investigated" = "Threat") %>%
#       dplyr::arrange(Author)
#     
#   })
#     
#   # Render the references table in the shiny app  
#   output$references_table <- DT::renderDT({
#     
#     shiny::req(references_table_object())
#     
#     references_table <- references_table_object()
#     
#   },
#   
#   options = list(
#     scrollX = TRUE, # allow scrolling if too wide to fit all columns on one page
#     autoWidth = TRUE, # use smart column width handling
#     pageLength = 5, # show 5 entries per page
#     headerCallback = DT::JS( # use this to alter the font size of the column names
#       "function(thead) {",
#       "  $(thead).css('font-size', '1em');",
#       "}"
#     )),
#   
#   rownames = FALSE # stops it adding column for row numbers
#   
#   )
#   
#   # Download reference table (.csv) button
#   output$download_references_table <- shiny::downloadHandler(
#     filename = function() {
#       paste0("references_table_", base::Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#       utils::write.csv(references_table_object(), file, row.names = FALSE)
#     }
#   )
#   
#   # =================================================================================================================
#   # =================================================================================================================
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 

### Version of server.R script adapted to show how the option for subgroup analysis could be added into Dynameta

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
library(readr) # for reading in csv files uploaded to the app
library(mapview) # for downloading the leaflet map

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
    
    # If the user selects "Your own data", then 'show' the upload_data_to_analyse widget and info button  
    if (input$data_choice == "Your own data") {
      shinyjs::show("upload_data_to_analyse")
      shinyjs::show("necessary_columns_info_btn")
    } else {
      shinyjs::hide("upload_data_to_analyse")
      shinyjs::hide("necessary_columns_info_btn")
    }
    
  })
  
  # Create reactive data object 
  data <- shiny::reactive({
    
    # When testing the package with shinytest, need to load the data in from the data_for_shinytest directory
    # or app can't find data like it does when user has installed the package 
    
    # For csv version
    #sample_data <- read.csv("../data_for_shinytest/sample_data_for_shinytest.csv")
    
    # Sample data in rds file
    sample_data <- readRDS("../data_for_shinytest/sample_data_rds")
    
    # If user has selected to use their own data, and uploaded a dataset, the data is their uploaded data
    if (input$data_choice == "Your own data" && !is.null(input$upload_data_to_analyse)) {
      
      # Get the file encoding - returns encodings 'guesses' and associated confidence values 
      encoding_and_confidence <- readr::guess_encoding(input$upload_data_to_analyse$datapath)
      
      # List the columns that must be included in the dataset
      necessary_columns <- c("Paper_ID", "Observation_ID", "Author", "Year", "Title", "DOI", "URL", "Latitude", 
                             "Longitude", "Country", "Order", "Biodiversity_metric", "IUCN_threat_category_1", 
                             "Treatment", "Treatment_N", "Treatment_Mean", "Treatment_error", "Control_N", 
                             "Control_Mean", "Control_error")
      
      # Checks on data 1 
      shiny::validate(
        
        # 1. Make sure file is a csv
        shiny::need(tools::file_ext(input$upload_data_to_analyse$name) == "csv", "File must be a .csv.") %then%
          
          # 2. Make sure file encoding is either UTF-8 or ASCII and it is at least 90% confident
          shiny::need(encoding_and_confidence[1, 1] == "UTF-8" | encoding_and_confidence[1, 1] == "ASCII" & encoding_and_confidence[1, 2] >= 0.9, 
                      paste0("The encoding of file you have uploaded (", encoding_and_confidence[1, 1], ") is not compatible with Dynameta. Ensure the file has UTF-8 or ASCII encoding.")) %then%
          
          # 3. Make sure file has certain columns - these are the columns in the sample data that are needed for Dynameta (other columns can vary)
          # Tell user which columns in their dataframe are missing (if any)
          shiny::need(base::all(necessary_columns %in% colnames(readr::read_csv(input$upload_data_to_analyse$datapath))), 
                      paste("Data doesn't contain correct column(s).
                            \nMissing columns that must be present: ",
                            paste(setdiff(necessary_columns, colnames(readr::read_csv(input$upload_data_to_analyse$datapath))), collapse = ", ")))
        
        
      )
      
      data <- readr::read_csv(input$upload_data_to_analyse$datapath)
      
      # Else if no data uploaded, or user has selected the sample data, then the data is the sample data
    } else {
      data <- sample_data
    }
    
  })
  
  ### Clickable info button for details on what columns must be present in the dataframe 
  shiny::observeEvent(input$necessary_columns_info_btn, {
    
    shiny::showModal(
      shiny::modalDialog(
        title = "Columns that must be included in your dataframe",
        size = "l",
        HTML("<b>Paper_ID</b> - The paper from which the comparison was drawn. (character)<br>
            <b>Observation_ID</b> - The comparison of that row, which should be unique to each row. (character)<br>
            <b>Author</b> - The full list of authors of the paper. (character)<br>
            <b>Year</b> - The year of publication of the paper. (integer)<br>
            <b>Title</b> - The paper title. (character)<br>
            <b>DOI</b> - The DOI of the paper. (character)<br>
            <b>URL</b> - The URL of the paper. (character)<br>
            <b>Latitude</b> - The latitude coordinate of the observation in that row. (double)<br>
            <b>Longitude</b> - The longitude coordinate of the observation in that row. (double)<br>
            <b>Country</b> - The country in which the observation in that row is located. (character)<br>
            <b>Order</b> - The taxonomic order of the biodiversity measured in that row, for both the treatment and control. (character)<br>
            <b>Biodiversity_metric</b> - The metric of biodiversity measured. Biodiversity_metric should typically be one of “Abundance”, “Richness”, or “Biomass”. (character)<br>
            <b>IUCN_threat_category_1</b> - Broadest level of IUCN threat. (character)<br>
            <b>Treatment</b> - Name of the threat measured in that row (e.g. insecticide). (character)<br>
            <b>Treatment_N</b> - The number of treatment sites from which the mean and error values were drawn. (integer)<br>
            <b>Treatment_Mean</b> - The mean biodiversity value across all treatment sites for that comparison. (double)<br>
            <b>Treatment_error</b> - The raw treatment error value reported by the authors for that observation. (double)<br>
            <b>Control_N</b> - The number of control sites from which the mean and error values were drawn. (integer)<br>
            <b>Control_Mean</b> - The mean biodiversity value across all control sites for that comparison. (double)<br>
            <b>Control_error</b> - The raw control error value reported by the authors for that observation. (double)<br><br>
            Your data can include columns for additional variables that you specified you would collect in your protocol. "),
        easyClose = TRUE
      )
    )
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
  
  ### Text for data summary
  
  # Add data summary text
  output$data_summary_text <- renderText({
    
    # Calculate number of rows and columns
    nrow <- nrow(data())
    ncol <- ncol(data())
    
    # Calculate percentage of treatment means and control means that are close to 0 (below 0.5)
    treatment_close_to_zero <- round((sum(data()$Treatment_Mean < 0.5)/nrow(data())) * 100, digits = 1)
    control_close_to_zero <- round((sum(data()$Control_Mean < 0.5)/nrow(data())) * 100, digits = 1)
    
    # Get list of column names
    colnames <- paste(colnames(data()), collapse = "<br>")
    
    paste0("<b>Summary of the data you will analyse using Dynameta.</b><br>", 
           "Your data has ", nrow, " rows and ", ncol, " columns.<br><br>
           It is worth noting that if your treatment and control mean values are close to zero, it can affect the accuracy of the variance estimation for a model (see ",
           tags$a(href="https://esajournals.onlinelibrary.wiley.com/doi/10.1890/14-2402.1", "Lajeunesse, 2015"), " for more information). ",
           treatment_close_to_zero, "% and ", control_close_to_zero, "% of your treatment and control means, respectively, are below 0.5. <br><br>",
           "The column names that your data contains are as follows:<br>", colnames, sep="")
  })
  
  
  # ---------------------------------------------------------------------------------------------------------------
  
  # Map of where data comes from
  # Make reactive object:
  leaflet_map <- reactive({
    
    # Filter the data to include data points which have long and lat available
    coord_data <- data() %>%
      tidyr::drop_na(Longitude, Latitude) %>%
      dplyr::filter(Longitude != "." & Latitude != ".")
    
    # Make leaflet map
    leaflet_map <- leaflet::leaflet(data = coord_data) %>%
      leaflet::addTiles() %>% # default basemap
      leaflet::addCircleMarkers(lng = ~Longitude, lat = ~Latitude, # identify columns in dataframe containing coords
                                popup = ~base::paste('<a href="', URL, '">Link to paper</a>', sep = ''), # Click on point to get clickable link to paper if available
                                clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5)) # cluster large numbers of markers
    
  })
  
  
  output$map <- leaflet::renderLeaflet({
    
    req(leaflet_map())
    
    map <- leaflet_map()
    
  })
  
  
  # Calculate total number of data points
  total_data_points <- shiny::reactive({
    
    nrow(data())
    
  })
  
  # Calculate number of data points with co-ordinates available
  data_with_coords <- shiny::reactive({
    
    data_with_coords <- total_data_points() - (sum(is.na(data()$Longitude) | data()$Longitude == "."))
    
  })
  
  # Add map figure legend
  output$map_figure_legend <- shiny::renderText({
    base::paste("<b>Figure 1.</b>", "Map showing location of data points. Currently,", data_with_coords(), "out of", total_data_points(), 
                "data points have latitude and longitude co-ordinates provided to enable them to be plotted.<br>
    Often, clusters of data points have the same co-ordinates. You can zoom in on, and click on, data clusters to explore the map.")
  })
  
  # Download map button 
  output$download_map <- downloadHandler(
    
    filename = function() {
      paste0("map_from_Dynameta_", Sys.Date(), ".png", sep="")
    },
    
    content = function(file) {
      mapview::mapshot(x = leaflet_map(),
                       file = file, 
                       cliprect = "viewport", # the clipping rectangle matches the height & width from the viewing port
                       selfcontained = FALSE) # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
    }
  ) 
  
  
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
    
    # Try to run the model on the currently selected subset of data. If doesn't work, tell user to include more data or view error message.
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
        shinyjs::enable("download_forest_plot")
        
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
        shinyjs::disable("download_forest_plot")
        
        # Then stop the process, and return this error message
        base::stop(shiny::safeError(paste0("This model failed to run. This may be due to insufficient data for this model to run, but please see the R error message: ", e)))
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
  
  # Make plot a reactive object 
  figure <- reactive({
    
    shiny::req(custom_model())
    
    figure <- metafor::forest(custom_model(),
                              xlim = c(-12, 8), # horizontal limits of the plot region
                              ilab = base::cbind(Treatment), # add in info on treatment used
                              ilab.xpos = -8, # position treatment labels
                              order = Treatment, # Order results by treatment
                              cex = 1.5,
                              col = "#0483A4", # change colour of overall effect size diamond using CEH hero colour
                              mlab = "RE Model for All Studies",
                              header = "Author(s) and Year")
    
  })
  
  # Render the plot in Dynameta
  output$custom_model_figure <- shiny::renderPlot({
    
    shiny::req(figure())
    
    custom_model_figure <- figure()
    
  })
  
  # Produce figure legend
  output$custom_model_figure_legend <- shiny::renderText({
    
    shiny::req(custom_model())
    
    # Convert LRR overall effect size to percentage
    percentage_change <- round(100 * (exp(stats::coef(custom_model())) - 1), digits = 2)
    
    # Calculate confidence interval lower bound in percentage
    ci_lb <- round(100 * (exp(custom_model()$ci.lb) - 1), digits = 2)
    
    # Calculate confidence interval upper bound in percentage
    ci_ub <- round(100 * (exp(custom_model()$ci.ub) - 1), digits = 2)
    
    # Calculate I2 statistic. Code adapted from http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate - Multilevel Models section
    W <- diag(1/custom_model()$vi)
    X <- metafor::model.matrix.rma(custom_model())
    P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
    i2 <- round(100 * sum(custom_model()$sigma2) / (sum(custom_model()$sigma2) + (custom_model()$k-custom_model()$p)/sum(diag(P))), digits = 2) 
    
    # Add these stats to the paste() below.
    
    paste("<b>Figure 2. </b>", "Forest plot showing the effect sizes for each data point and the overall effect size of ",
          paste(shiny::isolate(input$iucn_threat_category), collapse = ", "), " on biodiversity. The overall effect size is indicated by the diamond -
          the centre of the diamond on the x-axis represents the point estimate,
          with its width representing the 95% confidence interval. The specific ",
          paste(shiny::isolate(input$iucn_threat_category)), " type is listed next to each data point. <br><br>",
          "The overall effect size of ", paste(shiny::isolate(input$iucn_threat_category), collapse = ", "), " on biodiversity for ",
          paste(shiny::isolate(input$taxa_order), collapse = ", "), " in ", paste(shiny::isolate(input$location), collapse = ", "), " measured with ",
          paste(shiny::isolate(input$biodiversity_metric_category), collapse = ", "), " as the biodiversity metric is ", round(stats::coef(custom_model()), digits = 2),
          ". This equates to a percentage change of ", percentage_change, "%", " [", ci_lb, "%, ", ci_ub, "%]. <br><br>",
          "The I² statistic for the meta-analysis is ", i2, "%. This describes the percentage of total variance that is due to heterogeneity (variability among studies), and not due to chance. <br><br>",
          sep = "")
  })
  
  # ----------------------------------------------------------------------------------------------------------------
  
  ### Downloading the custom R model output, object, and forest plot 
  
  # Disable the download buttons on page load - so can't click it until a model has successfully run
  shinyjs::disable("download_custom_model_output")
  shinyjs::disable("download_custom_model_object")
  shinyjs::disable("download_forest_plot")
  
  # Disable the download buttons if the iucn_threat_category choice changes
  observeEvent(input$iucn_threat_category, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    shinyjs::disable("download_forest_plot")
  })
  
  # Disable the download buttons if the location choice changes
  shiny::observeEvent(input$location, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    shinyjs::disable("download_forest_plot")
  })
  
  # Disable the download buttons if the taxa_order choice changes
  shiny::observeEvent(input$taxa_order, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    shinyjs::disable("download_forest_plot")
  })
  
  # Disable the download buttons if the biodiversity_metric_category choice changes
  shiny::observeEvent(input$biodiversity_metric_category, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_object")
    shinyjs::disable("download_forest_plot")
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
  
  # Download forest plot button
  output$download_forest_plot <- shiny::downloadHandler(
    
    filename = function() {
      paste0("forest_plot", base::Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      grDevices::png(file, width = 1500, height = 1000)
      metafor::forest(custom_model(),
                      xlim = c(-12, 8), # horizontal limits of the plot region
                      ilab = base::cbind(Treatment), # add in info on treatment used
                      ilab.xpos = -8, # position treatment labels
                      order = Treatment, # Order results by treatment
                      cex = 1.5,
                      col = "#0483A4", # change colour of overall effect size diamond using CEH hero colour
                      mlab = "RE Model for All Studies",
                      header = "Author(s) and Year")
      grDevices::dev.off()
    }
    
  )
  
  
  
# ******************* Add for subgroup analysis ************************************************************************************
  
  ## Make reactive moderator (for sub-group analysis) choices
  output$reactive_moderator_choice <- shiny::renderUI({
    shinyWidgets::pickerInput(inputId = "moderator_choice",
                              label = "Moderator:",
                              choices = base::names(data()),
                              selected = "Country",
                              multiple = FALSE) # add actions box for selecting/de-selecting all options
  })
  
  ### Run model
  
  # Run sub-group analysis model once the run model button has been pressed
  sub_group_analysis <- shiny::eventReactive(input$run_sub_group_analysis, {
    
    subgroup_analysis_data <- data() 
    
    # Try to run the model. If doesn't work, tell user to include more data or view error message.
    base::tryCatch(
      expr = {
  
        # add small value to control and treatment columns
        subgroup_analysis_data$Treatment_Mean <- subgroup_analysis_data$Treatment_Mean + 0.1
        subgroup_analysis_data$Control_Mean <- subgroup_analysis_data$Control_Mean + 0.1
         
        # calculate effect sizes from number, mean, and SD - data needs to be in wide format
        # Adds yi and vi columns to data
        subgroup_analysis_data <- metafor::escalc(measure = "ROM", # log transformed ratio of means (i.e. log response ratio)
                                                  n1i = subgroup_analysis_data$Treatment_N,
                                                  n2i = subgroup_analysis_data$Control_N,
                                                  m1i = subgroup_analysis_data$Treatment_Mean,
                                                  m2i = subgroup_analysis_data$Control_Mean,
                                                  sd1i = subgroup_analysis_data$Treatment_error,
                                                  sd2i = subgroup_analysis_data$Control_error,
                                                  slab = paste(Paper_ID), # slab adds study labels which will help when we make forest plot
                                                  data = subgroup_analysis_data)
        
        # Make the chosen moderator a factor
        input_mod_factor <- as.factor(subgroup_analysis_data[, input$moderator_choice])
        
        # Run metafor model
        sub_group_analysis_model <- metafor::rma.mv(yi, vi, # effect sizes and corresponding variances
                                                    random = ~ 1 | Paper_ID/Observation_ID, # specify random-effects structure of model
                                                    mods = ~ input_mod_factor, # specify the user's chosen moderator
                                                    data = subgroup_analysis_data)
         
        sub_group_analysis <- sub_group_analysis_model
        
         
      }, error = function(e) {

        # If model does not successfully run, stop the process and return this error message
        base::stop(shiny::safeError(paste0("This model failed to run. This may be due to insufficient data for this model to run, but please see the R error message: ", e)))
      })
    
  })
  
  ### Render the model summary output
  output$subgroup_model_output <- shiny::renderPrint({
    
    shiny::req(sub_group_analysis())

    base::summary(sub_group_analysis())
    
  })
  
  
# **********************************************************************************************************************************                  
  
  
  
  
  # ----------------------------------------------------------------------------------------------------------------
  
  # =================================================================================================================
  # =================================================================================================================
  
  ##### References tab
  
  # =================================================================================================================
  # =================================================================================================================
  
  ### References table
  
  # Add table legend
  output$references_table_legend <- shiny::renderText({
    paste("<b>Table 2.</b>", "References for all papers that contribute data to the analysis.")
  })
  
  # Make reactive references table
  references_table_object <- shiny::reactive({
    
    # Initialise empty data frame to count data points per Paper_ID and which threat(s) the paper investigated
    all_details <- base::data.frame(Paper_ID = character(), Total_data_points = numeric(), Threat = character())
    
    # Fill in dateframe
    for (i in unique(data()$Paper_ID)) {
      
      # Filter for each paper
      paper_subset <- data() %>%
        dplyr::filter(Paper_ID == i)
      
      # Count number of rows for each paper
      paper_details = base::data.frame(
        Paper_ID = i,
        Total_data_points = nrow(paper_subset),
        Threat = unique(paper_subset$IUCN_threat_category_1)
      )
      
      # Add row to dataframe
      all_details <- rbind(all_details, paper_details)
    }
    
    
    # Make reference table including unique Paper_IDs (keeping all columns)
    references_table_object <- data() %>%
      dplyr::distinct(Paper_ID, .keep_all = TRUE)
    
    # Add data points and threats columns
    references_table_object <- base::merge(references_table_object, all_details, by = "Paper_ID")
    
    # Now just include the columns we want, and arrange in alphabetical order
    references_table_object <- references_table_object[, c("Author", "Year", "Title", "DOI", "Threat", "Total_data_points")] %>%
      dplyr::rename("Data points" = "Total_data_points") %>%
      dplyr::rename("Threat investigated" = "Threat") %>%
      dplyr::arrange(Author)
    
  })
  
  # Render the references table in the shiny app  
  output$references_table <- DT::renderDT({
    
    shiny::req(references_table_object())
    
    references_table <- references_table_object()
    
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
  
  # Download reference table (.csv) button
  output$download_references_table <- shiny::downloadHandler(
    filename = function() {
      paste0("references_table_", base::Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      utils::write.csv(references_table_object(), file, row.names = FALSE)
    }
  )
  
  # =================================================================================================================
  # =================================================================================================================
  
}















