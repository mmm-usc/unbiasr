
install.packages("shiny")
install.packages("shinyMatrix")
library(shiny)
library(shinyMatrix)

n <- 10
vnames <- as.character(1:n)

m <- matrix(0, n, n, dimnames = list(vnames, vnames)) 

ui <- fluidPage(titlePanel("shinyMatrix: Simple App"),   
                sidebarPanel(width = n+2,
                             tags$h4("Data"),
                             matrixInput("sample",
                                         value = m,
                                         rows = list(names = TRUE, editableNames = TRUE),
                                         cols = list(names = TRUE, editableNames = TRUE))),
                mainPanel(width = 6,
                          tableOutput("matrix")))

server <- function(input, output, session) {
    output$matrix <- renderTable({input$sample})
}

shinyApp(ui, server) 