# LOADING LIBRARIES -----------------------------------------------------------

library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shiny)
options(shiny.maxRequestSize = 500*1024^2)

# DEFINING UI -----------------------------------------------------------------

ui <- bootstrapPage(
  multicol_align(),
  
  # SETTING NAVBAR PAGE -------------------------------------------------------
  
  navbarPage(theme = shinytheme("flatly"),
             collapsible = TRUE,
             HTML(title_html()),
             id="nav",
             windowTitle = "Threat Detection",
             
             # INTRO TAB ------------------------------------------------------
             
             tabPanel("Introduction",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          h3("Introduction"),
                          p(style=justify(), 
                            intro_string()),
                          hr(),
                          
                          strong(p("Select Port Range(s) below:")),
                          tags$div(
                            align = 'left',
                            multicol_align(),
                            custom_checkbox(
                              "well_known",
                              label = "Well-known Ports",
                              inline = TRUE,
                              selected = c(1:5),
                              choiceNames = well_known
                            )
                          ), 
                          
                          tags$div(
                            align = 'left',
                            multicol_align(),
                            custom_checkbox(
                              "registered",
                              label = "Registered Ports",
                              inline = TRUE,
                              choiceNames = registered,
                              choiceValuesOffset = 11
                            )
                          ), 
                            
                          tags$div(
                            align = 'left',
                            multicol_align(),
                            custom_checkbox(
                              "private",
                              label = "Private/Dynamic Ports",
                              inline = TRUE,
                              choiceNames = private,
                              choiceValuesOffset = 16
                            )
                          ),
                          
                          hr(),
                          
                          make_slider(
                            "timestamp",
                            start = 1.0,
                            end = 6.0,
                            step = 0.1,
                            label = "Select Start & End Timestamp:"
                          ),
                          
                          hr(),
                          make_dropdown(
                            "time_interval",
                            "Select Interval",
                            c("5 minute", "30 minute"),
                            c("5 minute")),
                          
                          
                          hr(),
                          make_dropdown(
                            "port_type",
                            "Select Port Type",
                            c("Destination", "Source"),
                            c("Destination"))
                          
                          ),
                        
                        mainPanel(
                          h3("Counts of Port Ranges"),
                          br(),
                          p(style=justify(),
                            plot_desc_string()),
                          hr(),
                          withSpinner(plotlyOutput("port_plot", height="60vh"))
                          )
             )),
             
             # SCATTERPLOT TAB ------------------------------------------------
             
             tabPanel("Scatter Plot",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          h3("Scatter Plot"),
                          p(style=justify(), 
                            HTML(scatter_plot_string())),
                          hr(),
                          
                          make_dropdown(
                            "scatter_port_type",
                            "Select Port Type",
                            c("Well-known", "Registered", "Private"),
                            "Registered"),
                          
                          make_dropdown(
                            "x_axis",
                            "Select x-Axis",
                            c("Source", "Destination"),
                            "Source"),
                          
                          make_slider(
                            id = "perc_data",
                            label = "Percentage of Data to Use:",
                            start = 10,
                            end = 100,
                            step = 10,
                            value = 10
                          )
                          
                        ),
                        mainPanel(
                          
                          withSpinner(plotlyOutput("scatter", height="90vh"))
                          
                        )
                      )
             ),
             
             # RAW DATA TAB ---------------------------------------------------
             
             tabPanel("Raw Data Preview",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          h3("Data Preview"),
                          p(style=justify(), 
                            raw_data_string()),
                          hr(),
                          
                          fileInput("raw_data",
                                    "Upload Raw Data",
                                    accept = c("text/csv",
                                               "text/comma-separated-values,
                                               text/plain",
                                               ".csv"),
                                    ),
                          hr(),
                          HTML(column_note()),
                          br(),
                          uiOutput("column_names")
                          
                        ),
                        mainPanel(
                          withSpinner(DTOutput("input_data"))
                          
                        )
                      )
             ),
             
             # PREDICT TAB ----------------------------------------------------
             
             tabPanel("Predict",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          h3("Prediction"),
                          p(style=justify(), 
                            prediction_string()),
                          br(),
                          HTML(vector_bullet_list(prediction_features())),
                          hr(),
                          
                          fileInput("pred_data",
                                    NULL,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,
                                               text/plain",
                                               ".csv"),
                          ),
                          
                          strong(textOutput("feature_match")),
                          
                          hr(),
                          
                          p(style=justify(),
                            strong("Note: "),
                            help_string()),
                          downloadButton("download_blank",
                                         label = "Format"),
                          downloadButton("download_sample",
                                         label = "Sample")
                        ),
                        mainPanel(
                          
                          
                          h3("Predictions"),
                          hr(),
                          fluidRow(
                            
                            valueBoxOutput("cyber_attack_box"),
                            valueBoxOutput("normal_box"),
                            valueBoxOutput("final_box"),
                            
                          ),
                          hr(),
                          p("The first six columns & rows are shown below"),
                          hr(),
                          withSpinner(DTOutput("pred_data_preview"))
                          
                        )
                      )
             )
  )
)
