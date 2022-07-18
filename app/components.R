# LOADING LIBRARIES -----------------------------------------------------------

library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shiny)

# CHECKBOXES ------------------------------------------------------------------

custom_checkbox <- function(id, label = "Checkbox", choiceNames = c(),
                            selected = NULL, inline = FALSE,
                            choiceValuesOffset = 0) {
  
  # This function creates a custom checkbox group for the UI
  # This is a Shiny Component.
  
  # @id - the namespace id for the component (how other elements talk to it)
  # @label - the text that goes along with the group
  # @choiceNames - the vector of choices
  # @selected - which checkboxes are pre-selected
  # @inline - whether to display checkbox options inline or vertically
  
  ns <- NS(id)
  checkboxGroupInput(
    inputId = id,
    label = label,
    selected = selected,
    inline = inline,
    choiceNames = choiceNames,
    
    # the way we have defined our functions in server.R, the options are numeric
    # hence, this sequence 1:length(choiceNames) gives us the correct
    # corresponding option for the choices.
    
    # e.g. if the choices are c("A", "B", "C"), this vector is 1:3
    # if we select only A and B, the returned choices will be c(1,2)
    
    choiceValues = (1+choiceValuesOffset):
      (length(choiceNames)+choiceValuesOffset)
  )
}

# SLIDER ----------------------------------------------------------------------

make_slider <- function(id, start, end,
                        step = 1, sep = "",
                        label = "Select Timestamp",
                        value = c(2, 5)) {
  ns <- NS(id)
  sliderInput(id, label, min = as.numeric(start),
              max = as.numeric(end), step = step,
              sep = sep, value = value)
  
}

# DROPDOWN --------------------------------------------------------------------

make_dropdown <- function(id, label = "Select", choices,
                          selected) {
  
  ns <- NS(id)
  selectInput(
    id,
    label,
    choices,
    selected = NULL,
    multiple = FALSE
  )
  
}
