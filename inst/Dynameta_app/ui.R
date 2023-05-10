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
    version = 4, # there is a bootstrap 5 but it doesn't let me alter the heading font size
    bg = "#fff",
    fg = "#292C2F",
    primary = "#0483A4",
    secondary = "#EAEFEC",
    success = "#37a635",
    info = "#34b8c7",
    warning = "#F49633",
    base_font = bslib::font_link(family = "Montserrat",href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600&display=swap"))
  ),

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
                  p(h1(tags$b("Dynameta - A dynamic platform for ecological meta-analyses"))),

                  tags$br(),

                  p(h5("Meta-analyses are used to quantitatively summarise evidence across studies in a systematic process. Their larger
                  sample size (and hence power) compared to individual research studies increases the chance of detecting significant effects.")),

                  p(h5("Despite representing a significant improvement upon individual studies, meta-analyses have a number of limitations
                  which Dynameta was developed to overcome:")),

                  h5(tags$ol(
                    tags$li("Meta-analytic results are based on a snapshot of literature at a particluar time. As a living review platform,
                            Dynameta overcomes this by enabling results to be continually updated as new evidence becomes available."),
                    tags$li("Meta-analytic publications are resticted to presenting the results of the chosen questions asked by those researchers.
                            On the other hand, Dynameta allows investigation of a range of questions based on varying interests of researchers
                            through manipulation of the graphical user interface.")
                  )),

                  p(h5("Dynameta is designed for interactive ecological meta-analyses, oriented around testing the effect of anthropogenic threats
                  (based on the ", tags$a(href="https://www.iucnredlist.org/resources/threat-classification-scheme", "IUCN threats classification scheme", .noWS = "outside"),
                       ") on biodiversity. Nevertheless, the code can be easily repurposed to suit a variety of meta-analytic contexts.")),

                  p(h5("The app is split into 3 main sections:")),

                  h5(tags$ol(
                    tags$li("Use this 'Introduction' to choose data to analyse and investigate the data. The sample data is selected by default
                            and consists of data collected to test the effect of pollution (specifically pesticide application) on dragonfly and damselfly (Odonata) abundance"),
                    tags$li("Go to 'Run models' to run custom meta-analytic models to investigate the effect of threats on biodiversity."),
                    tags$li("Go to 'References' to view full details of the papers that contribute data to your analysis.")
                  )),

                  p(h5("Dynameta was developed as part of the ", tags$a(href="https://glitrs.ceh.ac.uk/", "GLiTRS"), "project, a cross-institutional consortium aiming to build
                  global threat-response models to better understand and predict insect biodiversity change.")),

                  tags$br(),

                  # link to code
                  p(h5(shiny::icon("github", lib = "font-awesome", "fa-2x"), # add-in github icon
                       tags$a(href="https://github.com/gls21/Dynameta", "View app source code."),
                       "If you encounter any issues or bugs while using Dynameta, please submit a new issue in the ",
                       tags$a(href="https://github.com/gls21/Dynameta/issues", "issue tracker"),
                       "with a detailed description of the problem, including steps to reproduce it.")),

                  # Citation
                  p(h5("Please cite Dynameta as follows: Skinner, G., Cooke, R., Junghyuk, K., Purvis, A., Raw, C., Woodcock, B.A., Millard, J. (2023).
                       Dynameta: a dynamic platform for ecological meta-analyses in R Shiny. R package version 0.1.0.")),

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
                      3,

                      # Input where user can upload data - hidden to start with
                      useShinyjs(), # set up shinyjs
                      h5(shinyjs::hidden(shiny::fileInput(inputId = "upload_data_to_analyse",
                                                          label = "Choose dataset (.csv) to upload",
                                                          accept = ".csv")))

                    ),

                    # Include action button that user can click to see details of columns that must be present in their dataframe for Dynameta to work
                    shiny::column(
                      3,

                      shinyjs::hidden(actionButton("necessary_columns_info_btn",
                                                   "Click this button for details on the columns that must be present
                                                   in your dataframe to allow analysis with Dynameta"))

                    )


                  ),

                  tags$br(),
                  tags$hr(),

                  # ----------------------------------------------------------------------------------------------------------------------

                  ### Make tables on where data comes from

                  p(h2(tags$b("Introduction"))),

                  p(h3("Use this tab to investigate your data.")),

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

                      h5(shiny::htmlOutput("data_summary_text")),

                      tags$br()

                    ),

                    column(
                      7,

                      # Add map of where data comes from
                      shinycssloaders::withSpinner(leaflet::leafletOutput("map"), type = 8),

                      # Add map figure legend
                      h5(shiny::htmlOutput("map_figure_legend")),

                      tags$br(),

                      # Add download button for leaflet map
                      shiny::downloadButton(outputId = "download_map",
                                            label = "Download map (.png)",
                                            style='font-size:125%')

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

# ******* Add code chunk ui1 here for subgroup-analysis *****************************************************************************

                  # ----------------------------------------------------------------------------------------------------------------------

                  # Title to show at top of tab
                  p(h2(tags$b("Run models"))),

                  p(h5("Use this tab to investigate how different threats impact biodiversity.")),

                  p(h5("The models are multilevel meta-analytic models, run using the ", tags$a(href="https://www.metafor-project.org/doku.php/metafor", "metafor"), " package. The models account for the non-independence
                  of the data by specifying paper and observation identification as nested random effects.")),

                  p(h5("The effect size used to compare biodiversity is the log transformed Ratio Of Means (ROM) (also known as the log response ratio),
                  which quantifies proportionate change between treatments.")),

                  p(h5("Based on your research question, you can filter the data by threat, location, taxonomic order, and biodiversity metric.")),

# ******* Add code chunk ui2 here for subgroup-analysis *****************************************************************************

                  tags$br(),
                  tags$hr(),

                  # -----------------------------------------------------------------------------------------------------------------------

                  # ===========================================================================================================

                  ### Running custom models

                  # ===========================================================================================================

# ******* Add code chunk ui3 here for subgroup-analysis *****************************************************************************

                                                     h5("Use this section to filter the data. Once you have made your selections, click 'Run custom model'.
                                                        The model runs in real-time and the results are presented as a forest plot."),

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

                                                     # Add buttons for downloading custom model results

                                                     p(h5("Use this section to download the results.")),

                                                     p(h5(tags$ul(
                                                       tags$li("Click 'Download R custom model summary' to download a .txt file containing the output of the summary() function
                                                                applied to the custom model object. This provides a results summary of the model fitting."),
                                                       tags$li("Click 'Download R custom model object' to download a .rds file containing the model object.
                                                                This has additional attributes attached, which specify the date and time the model was run,
                                                                the filters that were applied, and the R session information. Once downloaded,
                                                                use the readRDS() and attributes() functions to load the model object and view its attributes.
                                                                By downloading, it allows the same analysis to be repeated at a later date (perhaps after more data has become available)."),
                                                       tags$li("Click 'Download forest plot' to download a .png file of your forest plot")
                                                     ))),

                                                     shiny::fluidRow(

                                                       shiny::column(
                                                         4,

                                                         shinyjs::useShinyjs(), # so can enable and disable the download buttons

                                                         # download button for downloading model output
                                                         shiny::downloadButton(outputId = "download_custom_model_output",
                                                                               label = "Download R custom model summary",
                                                                               style='font-size:125%')

                                                       ),

                                                       shiny::column(
                                                         4,

                                                         shinyjs::useShinyjs(), # so can enable and disable the download buttons

                                                         # download button for downloading model object in rds file
                                                         shiny::downloadButton(outputId = "download_custom_model_object",
                                                                               label = "Download R custom model object",
                                                                               style='font-size:125%')

                                                       ),

                                                       shiny::column(
                                                         4,

                                                         shinyjs::useShinyjs(), # so can enable and disable the download buttons

                                                         # download button for downloading forest plot
                                                         shiny::downloadButton(outputId = "download_forest_plot",
                                                                               label = "Download forest plot",
                                                                               style='font-size:125%')

                                                       )

                                                     ),

                                                     tags$br(),
                                                     tags$br()

# ******* Add code chunk ui4 here for subgroup-analysis *****************************************************************************

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

                  tags$br(),

                  # Add download button

                  shiny::downloadButton(outputId = "download_references_table",
                                        label = "Download references table (.csv)",
                                        style='font-size:125%'),

                  tags$br(),
                  tags$br()

  ), # close references tab


  # ===============================================================================================================================
  # ===============================================================================================================================

  # Resources tab

  # ===============================================================================================================================
  # ===============================================================================================================================

  shiny::tabPanel("Resources",

                  # Text to explain what the tab is for
                  p(h2(tags$b("Resources"))),

                  p(h5("Please refer to and follow the guidance developed by communities of practice
                       when conducting evidence syntheses:",
                       tags$ul(
                         tags$li(tags$a(href="http://www.prisma-statement.org/", "PRISMA"),
                                 " - For reporting systematic reviews and meta-analysis
                                (developed for medical field but see ecology specific version below)."),
                         tags$li(tags$a(href="http://www.prisma-statement.org/Extensions/EcoEvo", "PRISMA Extension for Ecology and Evolution")),
                         tags$li(tags$a(href="https://www.roses-reporting.com/", "ROSES"),
                                 " - For reporting systematic reviews and meta-analysis (developed for environmental research)."),
                         tags$li(tags$a(href="https://environmentalevidence.org/", "Collaboration for Environmental Evidence (CEE)"),
                                 " - For conducting environmental evidence syntheses. See ",
                                 tags$a(href="https://environmentalevidence.org/information-for-authors", "here"),
                                 "for full guidance document."),
                         tags$li(tags$a(href="https://training.cochrane.org/handbook/current", "Cochrane Handbook for Systematic Reviews of Interventions"),
                                 " - For conducting systematic reviews (developed for medical field)."),
                         tags$li(tags$a(href="https://www.campbellcollaboration.org/research-resources/training-courses.html", "Campbell Collaboration"),
                                 " - For conducting evidence syntheses (developed for social sciences field).")
                       ))),

                  p(h5("The following are useful guides for conducting meta-analyses:",
                       tags$ul(
                         tags$li(tags$a(href="https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/", "Doing Meta-Analysis with R: A Hands-On Guide")),
                         tags$li(tags$a(href="http://www.metafor-project.org/doku.php/tips", "The metafor package tips and notes"))
                       ))),

                  tags$br()

  ) # close resources tab

)

# ===============================================================================================================================
# ===============================================================================================================================



