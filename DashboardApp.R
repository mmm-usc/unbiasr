
library(shiny)
library(shinydashboard) #UI
library(shinyjs) #toggle hide, reset, inlinecss
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
                     menuItem("Other", tabName = "other", icon = icon("th"))
                   )),

  dashboardBody(
    #need this for shinyjs features to work
    useShinyjs(),
    #inlinecss using shinyjs and css settings for matrix styling
    inlineCSS(
      ".matrix-input-table td
          {
              border:1px solid #D3D3D3;
              padding: 0px 8px;
              color: #333333;
              height: 35px !important;
          }"
    ),
    tabItems(
      tabItem(tabName = "inputs",
              fluidPage(withMathJax(), fluidRow(
                h1("INPUTS"),
                box(
                  div(style = "display:inline-block;margin-right: 52%;padding-bottom: 10px;",
                      actionButton("resetButton", "reset inputs")),
                  
                  textInput(
                    'lambda_r',
                    'Input factor loadings \\( \\lambda \\) for the reference group',
                    placeholder = "1.00, 1.66, 2.30, 2.29"
                  ),
                  textInput(
                    'lambda_f',
                    'Input factor loadings \\( \\lambda \\) for the focal group',
                    placeholder = "1.00, 1.66, 2.30, 2.29"
                  ),
                  switchInput("uselambda_f", "Focal group?", FALSE),
                  textInput(
                    'tau_r',
                    'Input measurement intercepts \\( \\tau \\) for the reference group',
                    placeholder = "1.54, 1.36, 1.16, 1.08"
                  ),
                  textInput(
                    'tau_f',
                    'Input measurement intercepts \\( \\tau \\) for the focal group',
                    placeholder = "0.68, 1.36, 1.16, 1.08"
                  ),
                  switchInput("usetau_f", "Focal group?", FALSE),
                  h4('unique factor variance-covariance'),
                  switchInput("useMatrix", "Matrix input?", FALSE, inline = TRUE),
                  switchInput("usetheta_f", "Focal group?", FALSE, inline = TRUE),
                  textInput(
                    'theta_r',
                    'Input the diagonal of the unique factor variance-covariance matrix \\( \\theta \\) for the reference group',
                    placeholder = "1.20, 0.81, 0.32, 0.32"
  
                  ),
                  textInput(
                    'theta_f',
                    'Input the diagonal of the unique factor variance-covariance matrix \\( \\theta \\) for the focal group',
                    placeholder = "0.72, 0.81, 0.32, 0.32"
                  ),
                  
                  sliderInput(
                    "theta_rSlider",
                    "Input the unique factor variance-covariance matrix for the reference group",
                    min = 2,
                    max = 5,
                    value = 2
                  ),
                  uiOutput("theta_rMatrixUI"),
                  
                  sliderInput(
                    "theta_fSlider",
                    "Input the unique factor variance-covariance matrix for the focal group",
                    min = 2,
                    max = 5,
                    value = 2
                  ),
                  
                  uiOutput("theta_fMatrixUI"),
 
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
                    "Latent factor mean \\( \\kappa \\) for the reference group:",
                    value = 0.5,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  numericInput(
                    "kappa_f",
                    "Latent factor mean \\( \\kappa \\) for the focal group:",
                    value = 0.0,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  switchInput("usekappa_f", "Focal group?", FALSE),
                  numericInput(
                    "phi_r",
                    "Latent factor variance \\( \\phi \\) for the reference group:",
                    value = 1.,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  numericInput(
                    "phi_f",
                    "Latent factor variance \\( \\phi \\) for the focal group:",
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
                  box(title = "plot name", plotOutput("distPlot")),
                  box(title = "table title", tableOutput("table"))
                )
              ))),
      tabItem(tabName = "other",
              fluidPage(fluidRow(
                h2("Other"),
                fluidRow(box(title = "future content"), )
                
              )))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  source('PartInv.R', local = TRUE)
  
  #MATRIX TABLES
  
  output$theta_fMatrixUI <- renderUI({
    matrixInput(
      "theta_fMatrixInput",
      value = matrix("1", input$theta_fSlider, input$theta_fSlider),
      rows = list(),
      cols = list(names = FALSE,
                  
                extend = FALSE),
      class = "numeric",
      paste = TRUE,
    )
  })
  
  #this is how to get the matrix to plug in
  theta_fMatrixOutput <- reactive({
    input$theta_fMatrixInput
  })
  
  output$theta_rMatrixUI <- renderUI({
    matrixInput(
      "theta_rMatrixInput",
      value = matrix("1", input$theta_rSlider, input$theta_rSlider),
      rows = list(),
      cols = list(names = FALSE,
                  extend = FALSE),
      class = "numeric",
      paste = TRUE,
    )
  })
  
  #this is how to get the matrix to plug in
  theta_rMatrixOutput <- reactive({
    input$theta_rMatrixInput
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
  observeEvent(input$usekappa_f, {
    shinyjs::toggle(id = "kappa_f")
  })
  observeEvent(input$usephi_f, {
    shinyjs::toggle(id = "phi_f")
  })
  
  #listens for either a button press from theta or from useMatrix
  listenFromThetaMatrix <- reactive({
    list(input$usetheta_f,input$useMatrix)
  })
  #if either button is pressed
  observeEvent(listenFromThetaMatrix(), {
    #for every combination of button presses, show/hide appropriate inputs
    if (input$useMatrix == FALSE & input$usetheta_f == TRUE){
      shinyjs::show(id = "theta_f")
      shinyjs::show(id = "theta_r")
      
      shinyjs::hide(id = "theta_rSlider")
      shinyjs::hide(id = "theta_rMatrixUI")
      shinyjs::hide(id = "theta_fSlider")
      shinyjs::hide(id = "theta_fMatrixUI")
    }
    else if (input$useMatrix == FALSE & input$usetheta_f == FALSE){
      shinyjs::hide(id = "theta_f")
      shinyjs::show(id = "theta_r")
      
      shinyjs::hide(id = "theta_rSlider")
      shinyjs::hide(id = "theta_rMatrixUI")
      shinyjs::hide(id = "theta_fSlider")
      shinyjs::hide(id = "theta_fMatrixUI")
    }
    else if(input$useMatrix == TRUE & input$usetheta_f == FALSE){
      shinyjs::hide(id = "theta_f")
      shinyjs::hide(id = "theta_r")
      
      shinyjs::show(id = "theta_rSlider")
      shinyjs::show(id = "theta_rMatrixUI")
      shinyjs::hide(id = "theta_fSlider")
      shinyjs::hide(id = "theta_fMatrixUI")
    }
    else{
      shinyjs::hide(id = "theta_f")
      shinyjs::hide(id = "theta_r")
      
      shinyjs::show(id = "theta_rMatrixUI")
      shinyjs::show(id = "theta_fMatrixUI")
      shinyjs::show(id = "theta_rSlider")
      shinyjs::show(id = "theta_fSlider")
    }
    
  })
  
  
  observeEvent(input$resetButton, {
    reset("usepropsel")
    reset("uselambda_f")
    reset("usetau_f")
    reset("usetheta_f")
    reset("usekappa_f")
    reset("usephi_f")
    reset("useMatrix")
    reset("theta_rSlider")
    reset("theta_fSlider")
    
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
    if (input$usetheta_f == FALSE & input$useMatrix == FALSE) {
      theta_f = diag(theta_rNumeric())
    }
    else if(input$usetheta_f == TRUE & input$useMatrix == FALSE){
      theta_f = diag(theta_fNumeric())
    }
    else if(input$usetheta_f == TRUE & input$useMatrix == TRUE){
      theta_f = theta_fMatrixOutput()
    }
    else{
      theta_f = theta_rMatrixOutput()
    }
  })
  
  theta_r <- reactive({
    if (input$useMatrix == TRUE) {
      theta_r = theta_rMatrixOutput()
    }
    else{
      theta_r = diag(theta_rNumeric())
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
        tau_f = tau_f(),
        Theta_f = theta_f(),
        tau_r = tau_rNumeric(),
        Theta_r = theta_r()
      )
      title("Relationship between true latent construct scores
               and observed test scores", cex.main = 1)
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
        Theta_r = theta_r()
      )
    }
  })
  
  output$table <- renderTable(rownames = TRUE, {
    print(theta_fMatrixOutput())
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
        Theta_r = theta_r()
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
        Theta_r = theta_r()
      )[[4]]
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
