# Load required packages
library(shiny)
library(shinydashboard) #GUI
library(shinyjs) #show/hide/toggle, reset, inlineCSS
library(shinyWidgets) #materialSwitch
library(shinyMatrix) #matrixInput
library(rsconnect) #connect to server Shinyapps.io
library(shinytest) #automated snapshot testing
library(roxygen2)

myApp <- function(...) {
  shinyApp(ui = ui, server = server)
}
