
library(shiny)
library(shinydashboard) #UI
library(shinyjs) #toggle hide, reset
library(shinyWidgets) #using for a single button
library(shinyMatrix)#for matrix input

#dashboard page using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Computation of Selection Accuracy Indexes",
                  titleWidth = 400),
  dashboardSidebar(width = 175,
                   sidebarMenu(
                     menuItem("Inputs", tabName = "inputs", icon = icon("dashboard")),
                     menuItem("Outputs", tabName = "outputs", icon = icon("table")),
                     menuItem("Matrix", tabName = "matrix", icon = icon("th"))
                   )),
dashboardBody(useShinyjs(),  #(need for shinyjs toggle to work)
              tabItems(
                tabItem(tabName = "inputs",
                        fluidPage(fluidRow(
                          h1("INPUTS"),
                          box(
                            div(style = "display:inline-block;margin-right: 52%;padding-bottom: 10px;",
                                actionButton("resetButton", "reset inputs")),
                            
                            textInput(
                              'lambda_r',
                              'Input factor loadings for the reference group', "1.00, 1.66, 2.30, 2.29"
                            ),
                            textInput(
                              'lambda_f',
                              'Input factor loadings for the focal group', "1.00, 1.66, 2.30, 2.29"
                            ),
                            switchInput("uselambda_f", "Focal group?", FALSE),
                            textInput(
                              'tau_r',
                              'Input measurement intercepts for the reference group', "1.54, 1.36, 1.16, 1.08"
                            ),
                            textInput(
                              'tau_f',
                              'Input measurement intercepts for the focal group', "0.68, 1.36, 1.16, 1.08"
                            ),
                            switchInput("usetau_f", "Focal group?", FALSE),
                            textInput(
                              'theta_r',
                              'Input the diagonal of the unique factor variance-covariance matrix for the reference group',
                              "1.20, 0.81, 0.32, 0.32"
                            ),
                            textInput(
                              'theta_f',
                              'Input the diagonal of the unique factor variance-covariance matrix for the focal group', 
                              "0.72, 0.81, 0.32, 0.32"
                            ),
                            switchInput("usetheta_f", "Focal group?", FALSE),
                            
                            tags$h4("tau example"),
                            matrixInput(
                              "sampleOut",
                              value = matrix(runif(4), 1, 4, dimnames = list(NULL, c("1","2","3","4"))),
                              rows = list(
                                
                              ),
                              cols = list(
                                names = FALSE,
                                extend = FALSE
                              )
                            ),
                            
                            tags$h4("theta example"),
                            sliderInput("matrixSlider", "",
                                        min = 2, max = 8,
                                        value = 4),
                            
                            uiOutput(
                              "sampleOutAdjustable",
                            ),
                            
                          ),
                          box(
                            #use propsel
                            switchInput("usepropsel", "Select 10% population?", FALSE),
                            #plot contour TF
                            numericInput(
                              "cut_z",
                              "Cutoff score on the observed composite:",
                              value = 0.5,
                              min = 0,
                              max = 1,
                              step = 0.01
                            ),
                            numericInput(
                              "prop",
                              "Selection proportion:",
                              value = 0.5,
                              min = 0,
                              max = 1,
                              step = 0.01
                            ),
                            numericInput(
                              "pmix",
                              "Mixing proportion:",
                              value = 0.5,
                              min = 0,
                              max = 1,
                              step = 0.01
                            ),
                            numericInput(
                              "kappa_r",
                              "Latent factor mean (reference):", 
                              value = 0.5, 
                              min = 0, 
                              max = 1, 
                              step = 0.01
                            ),
                            numericInput(
                              "kappa_f",
                              "Latent factor mean (focal):",
                              value = 0.0,
                              min = 0,
                              max = 1,
                              step = 0.01
                            ),
                            switchInput("usekappa_f", "Focal group?", FALSE),
                            numericInput(
                              "phi_r",
                              "Latent factor variance (reference):",
                              value = 1.,
                              min = 0,
                              max = 1,
                              step = 0.01
                            ),
                            numericInput(
                              "phi_f",
                              "Latent factor variance (focal):",
                              value = 1.,
                              min = 0,
                              max = 1,
                              step = 0.01
                            ),
                            switchInput("usephi_f", "Focal group?", FALSE)
                          )
                        ))),
                tabItem(tabName = "outputs",
                        fluidPage(fluidRow(
                          h2("OUTPUTS"),
                          fluidRow(
                            box(title = "plot title", plotOutput("distPlot")),
                            box(title = "table title", tableOutput("table"))
                          )
                        ))),
                tabItem(tabName = "matrix",
                        fluidPage(fluidRow(
                          h2("Matrix"),
                          fluidRow(box(title = "future content"),)
                          
                        )))
              ))
                        )

# Define server logic required to draw a histogram
server <- function(input, output) {
  source('PartInv.R', local = TRUE)
  
  output$sampleOutAdjustable <- renderUI({
    matrixInput(
      "sampleOut",
      value = matrix("1",input$matrixSlider,input$matrixSlider),
      rows = list(
         ),
      cols = list(
        names = FALSE,
        extend = FALSE
      ),
      paste = TRUE,
    )
  })
  
  observeEvent(input$usepropsel, {
    if (input$usepropsel == TRUE) {
      shinyjs::hide(id = "cut_z")
      shinyjs::show(id = "prop")
    } else{
      shinyjs::show(id = "cut_z")
      shinyjs::hide(id = "prop")
    }
  })
  observeEvent(input$uselambda_f, {
    shinyjs::toggle(id = "lambda_f")
  })
  observeEvent(input$usetau_f, {
    shinyjs::toggle(id = "tau_f")
  })
  observeEvent(input$usetheta_f, {
    shinyjs::toggle(id = "theta_f")
  })
  observeEvent(input$usekappa_f, {
    shinyjs::toggle(id = "kappa_f")
  })
  observeEvent(input$usephi_f, {
    shinyjs::toggle(id = "phi_f")
  })
  
  observeEvent(input$resetButton, {
    reset("prop")
    reset("cut_z")
    reset("pmix")
    reset("lambda_f")
    reset("lambda_r")
    reset("tau_f")
    reset("tau_r")
    reset("theta_f")
    reset("theta_r")
    reset("kappa_f")
    reset("kappa_r")
    reset("phi_f")
    reset("phi_r")
  })
  
  lambda_rNumeric <- reactive({
    as.numeric(unlist(strsplit(input$lambda_r, ",")))
  })
  lambda_fNumeric <- reactive({
    as.numeric(unlist(strsplit(input$lambda_f, ",")))
  })
  tau_rNumeric <- reactive({
    as.numeric(unlist(strsplit(input$tau_r, ",")))
  })
  tau_fNumeric <- reactive({
    as.numeric(unlist(strsplit(input$tau_f, ",")))
  })
  theta_rNumeric <- reactive({
    as.numeric(unlist(strsplit(input$theta_r, ",")))
  })
  theta_fNumeric <- reactive({
    as.numeric(unlist(strsplit(input$theta_f, ",")))
  })
  
  exampleNumeric <- reactive({
    as.numeric(c(input$sampleOut))
  })
  
  sampleOutAdjustableNumeric <- reactive({
    input$sampleOutAdjustable
  })
  
  lambda_f <- reactive({
    if (input$uselambda_f == FALSE) {
      lambda_f = lambda_rNumeric()
    }
    else{
      lambda_f = lambda_fNumeric()
    }
  })
  tau_f <- reactive({
    if (input$usetau_f == FALSE) {
      tau_f = tau_rNumeric()
    }
    else{
      tau_f = tau_fNumeric()
    }
  })
  theta_f <- reactive({
    if (input$usetheta_f == FALSE) {
      theta_f = theta_rNumeric()
    }
    else{
      theta_f = theta_fNumeric()
    }
  })
  kappa_f <- reactive({
    if (input$usekappa_f == FALSE) {
      kappa_f = input$kappa_r
    }
    else{
      kappa_f = input$kappa_f
    }
  })
  phi_f <- reactive({
    if (input$usephi_f == FALSE) {
      phi_f = input$phi_r
    }
    else{
      phi_f = input$phi_f
    }
  })
  
  output$distPlot <- renderPlot({
    if (input$usepropsel == FALSE) {
      PartInv(
        plot_contour = TRUE,
        cut_z = input$cut_z,
        pmix_ref = input$pmix,
        kappa_r = input$kappa_r,
        kappa_f = kappa_f(),
        phi_r = input$phi_r,
        phi_f = phi_f(),
        lambda_r = lambda_rNumeric(),
        lambda_f = lambda_f(),
        tau_f = exampleNumeric(), #tau_f() #EXAMPLE
        Theta_f = theta_f(),
        tau_r = tau_rNumeric(),
        Theta_r = diag(theta_rNumeric())
      )
    }
    else{
      PartInv(
        propsel = input$prop,
        plot_contour = TRUE,
        cut_z = input$cut_z,
        pmix_ref = input$pmix,
        kappa_r = input$kappa_r,
        kappa_f = kappa_f(),
        phi_r = input$phi_r,
        phi_f = phi_f(),
        lambda_r = lambda_rNumeric(),
        lambda_f = lambda_f(),
        tau_f = tau_f(),
        Theta_f = theta_f(),
        tau_r = tau_rNumeric(),
        Theta_r = diag(theta_rNumeric())
      )
    }
  })
  
  output$table <- renderTable(rownames = TRUE, {
    print(sampleOutAdjustableNumeric())
    
    if (input$usepropsel == FALSE) {
      PartInv(
        plot_contour = FALSE,
        cut_z = input$cut_z,
        pmix_ref = input$pmix,
        kappa_r = input$kappa_r,
        kappa_f = kappa_f(),
        phi_r = input$phi_r,
        phi_f = phi_f(),
        lambda_r = lambda_rNumeric(),
        lambda_f = lambda_f(),
        tau_f = tau_f(),
        Theta_f = theta_f(),
        tau_r = tau_rNumeric(),
        Theta_r = diag(theta_rNumeric())
      )[[4]]
    }
    else{
      PartInv(
        propsel = input$prop,
        plot_contour = FALSE,
        cut_z = input$cut_z,
        pmix_ref = input$pmix,
        kappa_r = input$kappa_r,
        kappa_f = kappa_f(),
        phi_r = input$phi_r,
        phi_f = phi_f(),
        lambda_r = lambda_rNumeric(),
        lambda_f = lambda_f(),
        tau_f = tau_f(),
        Theta_f = theta_f(),
        tau_r = tau_rNumeric(),
        Theta_r = diag(theta_rNumeric())
      )[[4]]
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
