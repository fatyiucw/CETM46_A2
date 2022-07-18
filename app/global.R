# CLEARING ENVIRONMENT --------------------------------------------------------

rm(list=ls())

# LOADING LIBRARIES -----------------------------------------------------------

library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(scales)
library(shinycssloaders)
library(DT)
library(caret)
library(ranger)
options(scipen=999)
options(shiny.maxRequestSize = 500*1024^2)

# LOADING DATA ----------------------------------------------------------------

for_plot <- read.csv("data/25_for_plot.csv")
rf_model <- readRDS("data/rf.rds")

# SOURCING FILES --------------------------------------------------------------

source('./components.R')

# FUNCTIONS -------------------------------------------------------------------

get_port_breaks <- function(){
  
  break_labels <- c(
    "0-99",
    "100-199",
    "200-299",
    "300-399",
    "400-499",
    "500-599",
    "600-699",
    "700-799",
    "800-899",
    "900-999",
    "1000-1099",
    "1100-9999",
    "10000-19999",
    "20000-29999",
    "30000-39999",
    "40000-49999",
    "50000-65535"
  )
  
  return(break_labels)
  
}

vector_bullet_list <- function(vector) {
  if(length(vector > 1)) {
    paste0("<ul><li>", 
           paste0(
             paste0(vector, collpase = ""), collapse = "</li><li>"),
           "</li></ul>")   
  }
}

# PREPROCESSING ---------------------------------------------------------------

## TIMESTAMP ------------------------------------------------------------------

for_plot$Timestamp <- strsplit(for_plot$Timestamp, " ")
for_plot$Timestamp <- sapply(for_plot$Timestamp, tail, 1)

# Manipulation to get the decimal timestamp for correct time values
# as per a numeric access
# e.g 1:30 ~ 1.5

for_plot$Timestamp.Ratio <- strsplit(for_plot$Timestamp, ":")
for_plot$Timestamp.Ratio_ <- sapply(for_plot$Timestamp.Ratio, head, 1)
for_plot$Timestamp.Ratio <- sapply(for_plot$Timestamp.Ratio, tail, 1)
for_plot$Timestamp.Ratio <- as.numeric(for_plot$Timestamp.Ratio) / 60
for_plot$Timestamp.Ratio <- as.numeric(for_plot$Timestamp.Ratio) + 
  as.numeric(for_plot$Timestamp.Ratio_)
for_plot$Timestamp.Ratio_ <- NULL
for_plot$Timestamp.Ratio <- round(for_plot$Timestamp.Ratio, 1)

for_plot$Timestamp.Ratio.30 <- ceiling(for_plot$Timestamp.Ratio - 0.5)

## LABEL ----------------------------------------------------------------------

breaks <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 10000,
            20000, 30000, 40000, 50000, 65535)

break_labels <- get_port_breaks()

for_plot$Source.Port.Range <- cut(as.numeric(for_plot$Source.Port),
                                  breaks = breaks,
                                  labels = break_labels)

for_plot$Destination.Port.Range <- cut(as.numeric(for_plot$Destination.Port),
                                  breaks = breaks,
                                  labels = break_labels)

# VARIABLES -------------------------------------------------------------------

## PORTS ----------------------------------------------------------------------

well_known <- c("0-99",
                "100-199",
                "200-299",
                "300-399",
                "400-499",
                "500-599",
                "600-699",
                "700-799",
                "800-899",
                "900-999",
                "1000-1099")

registered <- c("1100-9999",
                "10000-19999",
                "20000-29999",
                "30000-39999",
                "40000-49999")

private <- c("50000-65535")

## FEATURES -------------------------------------------------------------------

prediction_features <- function() {
  
  features <- c("Packet.Length.Std",
                "Total.Length.of.Bwd.Packets",
                "Subflow.Bwd.Bytes",
                "Destination.Port",
                "Packet.Length.Variance",
                "Bwd.Packet.Length.Mean",
                "Avg.Bwd.Segment.Size",
                "Bwd.Packet.Length.Max",
                "Init_Win_bytes_backward",
                "Total.Length.of.Fwd.Packets",
                "Subflow.Fwd.Bytes",
                "Init_Win_bytes_forward",
                "Average.Packet.Size",
                "Packet.Length.Mean",
                "Max.Packet.Length")
  
  return(features)
  
}

# STRINGS --------------------------------------------------------------------

intro_string <- function(){
  
  intro <- "This app visualises and predicts malacious network attacks based
  on the information in each request. To get started, select one of the tab.
  If you want to continue on this tab, please use the options below to 
  visualise the data."
  
  return(intro)
  
}

plot_desc_string <- function() {
  
  desc <- "The plot below gives the total counts of the port appearances in the
  Source & Destination data as per the timestamp selected. Please note, the 
  timestamps are in decimals where 1:30 ~ 1.5. Also, you may change the plot
  with the options on the sidebar."
  
  return(desc)
  
}

raw_data_string <- function(){
  
  raw <- "Here, you may upload the raw data. For convenience and readability up
  to the first 5 columns are displayed. You can select the others below after
  uploading the file. The maximum file size allowed is 500MB."
  
  return(raw)
  
}

scatter_plot_string <- function(){
  
  scatter <- "On this screen, we've displayed a scatter plot of the Ports from
  Source & Destination. You can select some of the options from the dropdowns
  below.\n
  
  <strong>Note: </strong>Since the data is huge, we are plotting with random
  sample of the data so as the plot loads faster. In any case, this can be
  adjusted using the slider below. The default is set to 10%."
  
  return(scatter)
  
}

prediction_string <- function() {
  
  pred <- "For this part of the app, we will use a pretrained random forest
  model to distinguish between the three classes: PortScan, Benign & DDoS. The
  model was trained to achieve an accuracy of 99% across all three labels.
  For the modelling process, we only accept data with fifteen features listed
  below."
  
  return(pred)
  
}

help_string <- function() {
  
  help <- "If you feel you do not have the data in the format, please feel free
  to download the format or sample data from the buttons below. The upload button
  button above expects a .csv file with data in the fifteen columns."
  
  return(help)
  
}

# CSS & HTML -----------------------------------------------------------------

multicol_align <- function(){
  
  object <- list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: balance;
                                   -column-fill: balance;
                                 } 
                                 
                                 .checkbox-inline { 
                                   margin-left: 0px;
                                   margin-right: 10px;
                                 }
                                 .checkbox-inline+.checkbox-inline {
                                   margin-left: 0px;
                                   margin-right: 10px;
                                 }
                                 
                                 .irs-grid-pol.small {height: 0px;}")) 
  ))
  
  return(object)
  
} 

title_html <- function(){
  
  string <- '<a style="text-decoration:none;cursor:default;"
  class="active" href="#">Threat Detection</a>'
  
  return(string)
}

column_note <- function(){
  
  note <- "<p><strong>Note: </strong>Large datasets may take some time to load.
  </p>"
  
  return(note)
}

justify <- function(){
  
  return("text-align: justify;")
  
}