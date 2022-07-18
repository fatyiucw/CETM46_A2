# LOADING LIBRARIES -----------------------------------------------------------

library(shinydashboard)
library(shiny)
options(shiny.maxRequestSize = 500*1024^2)

# DEFINING SERVER -------------------------------------------------------------

server <- function(input, output, session) {
  
  # PORT PLOT -----------------------------------------------------------------
  
  port_plot_summary <- reactive({
    
    temp <- for_plot
    
    selected_interval <- input$time_interval
    
    if(selected_interval == "30 minute") {
      temp$Chosen.Timestamp <- temp$Timestamp.Ratio.30
      
      updateSliderInput(session,
                        "timestamp",
                        min = 1,
                        max = 6,
                        step = 1)
      
    } else {
      temp$Chosen.Timestamp <- temp$Timestamp.Ratio
      
      updateSliderInput(session,
                        "timestamp",
                        min = 1.0,
                        max = 6.0,
                        step = 0.1)
      
    }
    
    if(input$port_type == "Destination") {
      temp <- temp %>%
        group_by(Destination.Port.Range, Chosen.Timestamp)
    } else {
      temp <- temp %>%
        group_by(Source.Port.Range, Chosen.Timestamp)
    }
    
    temp <- temp %>%
      summarise(Total = n())
    
    colnames(temp) <- c("Port", "Timestamp", "Total")
    
    
    
    selected_ports <- c(input$well_known,
                        input$registered,
                        input$private)
    
    breaks <- get_port_breaks()
    
    break_labels <- breaks[as.numeric(selected_ports)]
    
    selected_timestamp <- input$timestamp
    
    temp <- temp %>%
      filter(Port %in% break_labels,
             Timestamp >= selected_timestamp[1],
             Timestamp <= selected_timestamp[2])
    
    return(temp)
    
  })
  
  port_plot <- reactive({
    
    plot <- ggplot(port_plot_summary(), aes(x = Timestamp, y = Total,
                                 fill = Port)) +
      ylab("") +
      geom_col(alpha = 0.8) + theme_minimal()
    
  })
  
  # READ RAW DATA -------------------------------------------------------------
  
  raw <- reactive({
    file <- input$raw_data
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    temp <- read.csv(file$datapath)
    return(temp)
  })
  
  raw_cols <- reactive({
    cols <- colnames(raw())
    return(cols)
  })
  
  # SCATTER PLOT --------------------------------------------------------------
  
  scatter_data <- reactive({
    
    temp <- for_plot
    
    temp$Source.Port <- as.integer(temp$Source.Port)
    temp$Destination.Port <- as.integer(temp$Destination.Port)
    
    temp <- temp %>%
      sample_frac(as.numeric(input$perc_data/100)) %>%
      select(Source.Port, Destination.Port, Label)
    
    if(input$x_axis == "Source") {
      
      temp$Port <- temp$Source.Port
      temp$Other.Port <- temp$Destination.Port
      
    } else {
      
      temp$Port <- temp$Destination.Port
      temp$Other.Port <- temp$Source.Port
      
    }
    
    temp$Source.Port <- NULL
    temp$Destination.Port <- NULL
    
    temp$Port.Type <- ifelse(temp$Port < 1100, "Well-known",
                             ifelse((temp$Port < 50000 & temp$Port >= 1100),
                             "Registered", "Private"))
    
    temp <- temp %>%
      filter(Port.Type == input$scatter_port_type)
    
    return(temp)
    
  })
  
  scatter <- reactive({
    
    x_axis <- input$x_axis
    
    xlab <- ifelse(x_axis == "Source", "Source", "Destination")
    ylab <- ifelse(x_axis == "Source", "Destination", "Source")
    
    temp <- scatter_data()
    
    g <- ggplot(temp, aes(x = Port,
                                y = Other.Port,
                                colour = Label)) + xlab(xlab) + ylab(ylab)
    
    g <- g + geom_jitter(alpha=0.2) + theme_minimal()
    
    return(g)
    
  })
  
  # PREDICTION ----------------------------------------------------------------
  
  pred_data <- reactive({
    file <- input$pred_data
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    temp <- read.csv(file$datapath)
    return(temp)
  })
  
  pred_check <- reactive({
    
    pred_cols <- colnames(pred_data())
    required_cols <- prediction_features()
    
    rows <- nrow(pred_data())
    row_text <- ifelse(rows > 0, "Data detected!", "Data not detected!")
    
    if(length(setdiff(pred_cols, required_cols)) == 0) {
      return(paste("All features found!", row_text))
    } else {
      return("Feature mismatch! Please refer the note below.")
    }
    
  })
  
  predictions <- reactive({
    
    if(pred_check() == "All features found! Data detected!"){
      preds <- predict(rf_model, newdata = pred_data())
      return(preds)
      
    }
    
  })
  
  pred_summary <- reactive({
    
    req(predictions())
    
    temp <- data.frame(predictions())
    colnames(temp) <- "Label"
    temp$Label <- ifelse(temp$Label == "DDoS",
                               "Cyber Attack",
                               "Normal")
    temp <- temp %>%
      group_by(Label) %>%
      summarise(Count = n())
    
    return(temp)
    
    
  })
  
  cyber_attacks <- reactive({
    
    req(predictions())
    
    temp <- pred_summary()[pred_summary()$Label == "Cyber Attack", "Count"]
    return(temp)
  
    
  })
  
  normal <- reactive({
    
    req(predictions())
    
    temp <- pred_summary()[pred_summary()$Label == "Normal", "Count"]
    return(temp)
    
  })
  
  # OUTPUTS -------------------------------------------------------------------
  
  output$column_names <- renderUI({
    
    custom_checkbox("column_select",
                    "Select the columns below:",
                    selected = c(1:5),
                    choiceNames = raw_cols(),
                    inline = FALSE)
    
  })
  
  output$feature_match <- renderText({
    
    pred_check()
    
  })
  
  ## VALUE BOXES --------------------------------------------------------------
  
  ### CYBER ATTACKS -----------------------------------------------------------
  
  output$cyber_attack_box <- renderValueBox({
    
    valueBox(
      value = cyber_attacks(),
      subtitle = "Cyber Attacks",
      icon = icon("biohazard", verify_fa = FALSE),
      color = "red"
    )
  })
  
  ### NORMAL ------------------------------------------------------------------
  
  output$normal_box <- renderValueBox({
    
    valueBox(
      value = normal(),
      subtitle = "Normal",
      icon = icon("shield", verify_fa = FALSE),
      color = "green"
    )
  })
  
  ### FINAL -------------------------------------------------------------------
  
  output$final_box <- renderValueBox({
    
    threat <- ifelse(cyber_attacks() > 0, "Cyber Attack!", "Normal")
    
    valueBox(
      value = threat,
      subtitle = "Threat",
      icon = if (threat == "Normal") icon("shield", verify_fa = FALSE) 
      else icon("biohazard", verify_fa = FALSE),
      color = if (threat == "Normal") "green" else "red"
    )
  })

  ## TABLES -------------------------------------------------------------------
  
  ### RAW DATA PREVIEW --------------------------------------------------------
  
  output$input_data <- DT::renderDataTable({
    datatable(raw()[, as.numeric(input$column_select)],
              options = list(lengthMenu = c(25, 50, 100, 200),
                             pageLength = 25),
              rownames=FALSE)
  })
  
  ## PRED DATA PREVIEW --------------------------------------------------------
  
  output$pred_data_preview <- DT::renderDataTable({
    datatable(pred_data()[, c(1:6)],
              options = list(lengthMenu = NULL,
                             pageLength = 6,
                             lengthMenu = c(6)),
              rownames=FALSE)
  })
  
  ## PLOTS --------------------------------------------------------------------
  
  ### SCATTER PLOT ------------------------------------------------------------
  
  output$scatter <- renderPlotly({
    ggplotly(scatter())
  })
  
  ### PORT PLOT ---------------------------------------------------------------
  
  output$port_plot <- renderPlotly({
    ggplotly(port_plot())
  })
  
  ## DOWNLOAD BUTTONS ---------------------------------------------------------
  
  ### FORMAT ------------------------------------------------------------------
  
  output$download_blank <- downloadHandler(
    filename = function() {
      
      "format.csv"
    },
    
    content = function(file) {
      
      temp <- read.csv("data/format.csv")
      write.csv(temp, file, row.names = FALSE)
      
    }
  )
  
  ### SAMPLE ------------------------------------------------------------------
  
  output$download_sample <- downloadHandler(
    filename = function() {
      
      "sample.csv"
    },
    
    content = function(file) {
      
      temp <- read.csv("data/sample.csv")
      write.csv(temp, file, row.names = FALSE)
      
    }
  )
  
}
