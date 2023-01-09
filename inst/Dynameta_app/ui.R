# Dynameta shiny app ui script

# Load packages
library(bslib) # for themes to customise appearance of shiny app (bs_theme bs_add_variables font_link)
library(DT) # for interactive tables
library(leaflet) # for mapping (leafletOutput)
library(shiny)
library(shinycssloaders) # for loading symbols (while models run) (withSpinner)
library(shinydisconnect) # for displaying nice error message if whole shiny app disconnects (disconnectMessage)
library(shinyjs) # for enabling and disabling download button (useShinyjs hidden)

ui <- shiny::navbarPage(
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  # Applying UKCEH theme manually by taking code from https://raw.githubusercontent.com/NERC-CEH/UKCEH_shiny_theming/main/theme_elements.R
  # bs_theme for high level theming
  # bs_add_variables for low level theming (a 'theme' is the first argument for this function)
  theme = bslib::bs_add_variables(bslib::bs_theme(
                                    bg = "#fff",
                                    fg = "#292C2F",
                                    primary = "#0483A4",
                                    secondary = "#EAEFEC",
                                    success = "#37a635",
                                    info = "#34b8c7",
                                    warning = "#F49633",
                                    base_font = bslib::font_link(family = "Montserrat",href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600&display=swap")),
                                  # low level theming
                                  "headings-font-weight" = 600),
  
  
  # Use titlePanel() to use the UKCEH logo the title, and change tab/window title
  shiny::titlePanel(title = img(src= "https://www.ceh.ac.uk/sites/default/files/images/theme/ukceh_logo_long_720x170_rgb.png", style= "height: 70px;vertical-align:middle;"),
             windowTitle = base::paste0("Interactive platform for insect biodiversity meta-analyses"," | UK Centre for Ecology & Hydrology")),

  # Add favicon - show UKCEH logo in tab
  tags$head(tags$link(rel="shortcut icon", href="https://brandroom.ceh.ac.uk/themes/custom/ceh/favicon.ico")),
  
  # Add a custom error message if the whole app disconnects / fails
  # Using header makes it apply to all tabs
  header = shinydisconnect::disconnectMessage(text = "An error has occured, please reload the page and try again.",
                             refresh = "", # Don't include a refresh button
                             width = "full", # Message should take up full width of screen
                             size = 30, # Size 30 writing
                             background = "#0483A4", # Blue background
                             colour = "white", # White writing
                             overlayColour = "grey", # Covers the app and draws attention to message
                             overlayOpacity = 0.8), # Nearly full opaque
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  # Intro tab
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  shiny::tabPanel("Introduction",
           
           # Make all the tab buttons bigger
           tags$head(
             tags$style(
               HTML(".navbar-nav > li > a {
                font-size: 30px;
              }")
             )
           ),
           
           # ----------------------------------------------------------------------------------------------------------------------
           
           # Pre-amble
           p(h1(tags$b("Dynameta - An interactive platform for insect biodiversity meta-analyses"))),
           
           tags$br(),
           
           p(h5("Insects are a highly diverse taxa and provide vital ecological services, though their underrepresentation in research compared to vertebrates
                results in a knowledge gap of insect biodiversity change and its drivers.")),
           
           p(h5("This app is designed to best utilise the insect biodiversity data available by allowing users to interactively run meta-meta-analytic models,
                which involves analysing multiple meta-analytic studies together by combining effect sizes for each of these.")),
           
           p(h5("Data is taken from a database containing a collection of meta-analytic studies, described as a 'living' database
                due to the ability of users to upload new data from their own meta-analyses.")),
           
           p(h5("The app can be used to investigate the effect of threats (based on ",
                tags$a(href="https://www.iucnredlist.org/resources/threat-classification-scheme", "IUCN threats classification scheme)"),
                " on biodiversity in terms of the log response ratio (quantifies proportionate change between treatments).
                You can filter the data by threat, location, taxonomic order, and biodiversity metric.")),
           
           p(h5("The app is split into 3 main sections:")),
           
           # Make bullet point list of the descriptions of the 3 main tabs
           h5(tags$ol(
             tags$li("Use this 'Introduction' to investigate the data sources used within the app."),
             tags$li("Go to 'Run models' to run models to investigate the effect of different threats on biodiversity."),
             tags$li("Go to 'Upload data' to upload data from your own meta-analysis.")
           )),
           
           # link to code
           p(h5(shiny::icon("github", lib = "font-awesome", "fa-2x"), # add-in github icon
                tags$a(href="https://github.com/gls21/Dynameta", "View app source code"))),  ######### Repo not yet public so won't work
           
           tags$br(),
           tags$hr(),
           
           # ----------------------------------------------------------------------------------------------------------------------
           
           ### User choice of what data to analyse - sample or their own
           
           p(h3("Choose data to analyse.")),
           
           shiny::fluidRow(
             
             shiny::column(
               6,
               
               # Include user input of choice of data
               h5(shiny::radioButtons(inputId = "data_choice",
                                      label = "Do you want to investigate the sample data provided with the package or upload your own data to analyse?",
                                      choices = c("Sample data", "Your own data"),
                                      inline = TRUE, # put options side by side
                                      selected = "Sample data", # Start with no items initially selected
                                      width = "100%")) # So it puts label on one line, not split it across multiple
        
             ),
             
             shiny::column(
               6,
               
               # Input where user can upload data - hidden to start with
               useShinyjs(), # set up shinyjs
               h5(shinyjs::hidden(shiny::fileInput(inputId = "upload_data_to_analyse",
                                                   label = "Choose dataset (.csv) to upload",
                                                   accept = ".csv")))
               
               
             )
             
           ), 
           
           tags$br(),
           tags$hr(),
           
           # ----------------------------------------------------------------------------------------------------------------------
           
           ### Make tables on where data comes from
           
           p(h2(tags$b("Introduction"))),
           
           p(h3("Use this tab to investigate the data sources used within this app.")),
           
           tags$hr(),
           
           shiny::fluidRow(
             
             shiny::column(
               5, # width of this column within the row (each row has to sum to 12 - includes offsets)
               
               # add table legend for overview table
               h5(shiny::htmlOutput("table_legend_overview")),
               
               # add sample size overview table
               h5(shinycssloaders::withSpinner(shiny::tableOutput("sample_sizes_overview"), type = 8)),
               
               tags$br(),
               
               tags$hr(),
               
               tags$br(),
               
               # add user choice of which threat to show details for
               # This will be reactive based on what threats are included in the data - so can change as more threats are added
               # The options are specified in the server file
               h5(shiny::uiOutput("reactive_threat")),
               
               tags$br(),
               
               # add table legend
               h5(shiny::htmlOutput("table_legend_threat_details")),
               
               # add paper details table
               h5(shinycssloaders::withSpinner(shiny::tableOutput("threat_details_table"), type = 8)),
               
               tags$br()
               
             ),
             
             column(
               7,
               
               # Add map of where data comes from
               shinycssloaders::withSpinner(leaflet::leafletOutput("map"), type = 8),
               
               # Add map figure legend
               h5(shiny::htmlOutput("map_figure_legend")),
               
               tags$br(),
               
             )
           )
           
           # ----------------------------------------------------------------------------------------------------------------------
           
  ),
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  # Modelling tab
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  shiny::tabPanel("Run models",
           
           # ----------------------------------------------------------------------------------------------------------------------
           
           # Title to show at top of tab
           p(h2(tags$b("Run models"))),
           
           p(h5("Use this tab to investigate how different threats impact biodiversity.
                The models run are metafor multivariate meta-analysis (rma.mv()) models.
                The effect size used is ROM (log transformed Ratio Of Means / log response ratio)).
                You are able to filter the data based on threat, location, taxonomic order, and biodiversity metric.")),
           
           tags$br(),
           tags$hr(),
           
           # -----------------------------------------------------------------------------------------------------------------------
           
           # ===========================================================================================================
           
           ### Running custom models
           
           # ===========================================================================================================
           
           h5("Use this section to filter the data you are interested in based on the criteria below.
              Once you have made your selections, click 'Run custom model'. The model should take less than a minute to run,
              and then you will be able to view a figure of the results."),
           
           tags$br(),
           
           # --------------------------------------------------------------------------------------------------
           
           # User inputs on what model to run and a button to run the model
           
           shiny::fluidRow(
             
             shiny::column(
               3,
               
               h4(shiny::uiOutput("reactive_iucn_threat_category"))
               
             ),
             
             shiny::column(
               3,
               
               h4(shiny::uiOutput("reactive_location"))
               
             ),
             
             shiny::column(
               3,
               
               h4(shiny::uiOutput("reactive_taxa_order"))
               
             ),
             
             shiny::column(
               3,
               
               h4(shiny::uiOutput("reactive_biodiversity_metric_category"))
               
             )
             
           ),
           
           tags$br(),
           
           shiny::fluidRow(
             
             shiny::column(
               12,
               
               # include action button to run model once inputs have been selected
               shiny::actionButton("run_custom_model", "Run custom model", style='font-size:125%')
             )
             
           ),
           
           tags$br(),
           
           # --------------------------------------------------------------------------------------------------
           
           # Graph and table produced based on the custom model run
           
           shiny::fluidRow(
             
             shiny::column(
               12,
               
               # This will make the stop error messages grey (rather than red) if the model doesn't run
               tags$head(tags$style(".shiny-output-error{color: grey;}")),
               
               # produce custom model graph
               shinycssloaders::withSpinner(shiny::plotOutput("custom_model_figure", width = 1500, height = 1000), type = 8)
               
             )
             
           ),
           
           shiny::fluidRow(
             
             shiny::column(
               12,
               
               # add custom model figure legend
               h5(shiny::htmlOutput("custom_model_figure_legend")),
               
             )
             
           ),
           
           tags$br(),
           tags$hr(),
           
           # --------------------------------------------------------------------------------------------------
           
           # User input on figure - plot all comparisons or subgroups?
           
           p(h5("Use this section to choose which graph to plot.")),
           
           # shiny::fluidRow(
           #
           #   shiny::column(
           #     12,
           #
           #     # user choice of metric
           #     h5(shiny::radioButtons(inputId = "metric2",
           #                     label = "Select metric:",
           #                     choices = c("Adjusted LRR", "Percentage change"),
           #                     selected = "Adjusted LRR")
           #
           #     ))
           #
           # ),
           
           tags$br(),
           tags$hr(),
           
           # --------------------------------------------------------------------------------------------------
           
           # Click to see descriptions of threats???? and put buttons for downloading custom model results
           
           p(h5("Use this section to view definitions of the ???, and download the results.")),
           
           p(h5(tags$ul(
             tags$li("Click 'Download R custom model summary' to download a .txt file containing the output of the summary() function
                applied to the custom model object. This provides a results summary of the model fitting."),
             tags$li("Click 'Download R custom model object' to download a .rds file containing the model object. This has additional
                     attributes attached, which specify the date and time the model was run, the filters that were applied to the data prior to running the model,
                     and the R session information. Once downloaded to your workspace, the object can be loaded with the readRDS() function, and its
                     attributes can be viewed using the attributes() function. By downloading the model object, it allows the same analysis to be repeated
                     at a later date (perhaps after more data has been uploaded to the app). ")
           ))),
           
           shiny::fluidRow(
             
             # shiny::column(
             #   3,
             #   shiny::actionButton("show2", "Click to see definitions of agricultural systems", style='font-size:125%')
             # ),
             #
             # shiny::column(
             #   3,
             #   shiny::actionButton("hide2", "Hide", style='font-size:125%')
             # ),
             
             shiny::column(
               6,
               
               shinyjs::useShinyjs(), # so can enable and disable the download buttons
               
               # download button for downloading model output
               shiny::downloadButton(outputId = "download_custom_model_output",
                              label = "Download R custom model summary",
                              style='font-size:125%')
               
             ),
             
             shiny::column(
               6,
               
               shinyjs::useShinyjs(), # so can enable and disable the download buttons
               
               # download button for downloading model object in rds file
               shiny::downloadButton(outputId = "download_custom_model_object",
                              label = "Download R custom model object",
                              style='font-size:125%')
               
             )
             
             # shiny::column(
             #   3,
             #
             #   # download button for downloading table of coefficients
             #   shiny::downloadButton(outputId = "download_custom_model_coeffs",
             #                  label = "Download custom model table of coefficients",
             #                  style='font-size:125%')
             # ),
             
             
           ),
           
           tags$br(),
           tags$br()
           
           # fluidRow(
           #
           #   h5(tableOutput("agri_sys_defs_table2"))
           #
           # )
           
  ), # close modelling tab
  
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  # References tab
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  shiny::tabPanel("References",
           
           # Text to explain what the tab is for
           p(h2(tags$b("References"))),
           
           # Include table legend for references table
           h5(shiny::htmlOutput("references_table_legend")),

           tags$br(),

           # add paper details table
           h5(shinycssloaders::withSpinner(DT::DTOutput("references_table"), type = 8)),
           
           tags$br()
           
  )
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
)


