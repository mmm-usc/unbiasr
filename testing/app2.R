library("shiny")
library("shinyMatrix")
m <- matrix(runif(12), 6, 2, dimnames = list(NULL, c("x", "y")))
ui <- fluidPage( 
    tags$head(# Note the wrapping of the string in HTML()
        tags$style(
            HTML(
                "
      .matrix-input-table tr:nth-child(odd){
      background-color: blue;
      color: #fff;
}"
            )
        )), 
    titlePanel("shinyMatrix: Simple App"), 
    sidebarPanel( 
        width = 6, 
        tags$h4("Data"), 
        matrixInput( 
            "sample", 
            value = m, 
            rows = list( 
                extend = TRUE 
            ), 
            cols = list( 
                names = TRUE 
            ) 
        ) 
    ), 
    mainPanel( 
        width = 6, 
        plotOutput("scatter") 
    ) 
) 
server <- function(input, output, session) { 
    output$scatter <- renderPlot({ 
        plot(input$sample, col = "red", main = "Scatterplot") 
    }) 
} 
shinyApp(ui, server) 
