# Load required packages
library(shiny)
library(shinydashboard) #GUI
library(shinyjs) #show/hide/toggle, reset, inlineCSS
library(shinyWidgets) #materialSwitch
library(shinyMatrix) #matrixInput
library(rsconnect) #connect to server Shinyapps.io
library(shinytest) #automated snapshot testing

#dashboardPage UI using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Computation of Selection Accuracy Indexes",
                  titleWidth = 400),
  dashboardSidebar(width = 105,
                   sidebarMenu(
                     menuItem(
                       "FirstPage",
                       tabName = "firstpage",
                       icon = icon("dashboard")
                     ),
                     #unused
                     menuItem("Other", tabName = "other", icon = icon("table"))
                   )),
  dashboardBody(
    #include for shinyjs features to work
    useShinyjs(),
    #inlineCSS using shinyjs to style margins and padding as well as size of box
    inlineCSS(
      '
          #outputTableBox {overflow-y: auto; height: 350px}
          .box {margin:5px;}
          .col-sm-4 {padding:6px !important;}
          .col-sm-8 {padding:6px !important;}'
    ),
    #pages
    tabItems(
      #page one
      tabItem(tabName = "firstpage", fluidPage(source(
        'pageOneUI.R', local = TRUE
      ))),
      #second page
      tabItem(tabName = "other", fluidPage(fluidRow()))
    )
  )
)

#SERVER-SIDE
server <- function(input, output) {
  #loads PartInv.R application
  source('PartInv.R', local = TRUE)
  #loads pageOne server file
  source('pageOneServer.R', local = TRUE)
}

shinyApp(ui = ui, server = server)
