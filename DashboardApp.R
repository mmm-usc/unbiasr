
# Load required packages
library(shiny)
library(shinydashboard) #GUI
library(shinyjs) #show/hide/toggle, reset, inlineCSS
library(shinyWidgets) #switchInput
library(shinyMatrix) #matrixInput

#dashboardPage UI using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Computation of Selection Accuracy Indexes",
                  titleWidth = 400),
  dashboardSidebar(width = 105,
                   sidebarMenu(
                     menuItem("Inputs", tabName = "inputs", icon = icon("dashboard")),
                     menuItem("Outputs", tabName = "outputs", icon = icon("table")),
                     menuItem("Other", tabName = "other", icon = icon("th"))
                   )),
  dashboardBody(
    #include for shinyjs features to work
    useShinyjs(),
    #inlinecss using shinyjs and css for matrixInput styling
    inlineCSS(
      ".matrix-input-table td
          {
              border:1px solid #D3D3D3;
              padding: 0px 8px;
              color: #333333;
              height: 35px !important;
          }"
    ),
    #pages
    tabItems(
      #frontpage
      tabItem(tabName = "inputs",
              #withMathJax() for greek char display
              fluidPage(withMathJax(), fluidRow(
                h1("INPUTS"),
                box(
                  #style for alignment of reset button
                  div(style = "display:inline-block;margin-right: 52%;padding-bottom: 10px;",
                      actionButton("resetButton", "reset inputs")),
                  #textInput for multi-value inputs
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
                    'Input the diagonal of the unique factor variance-covariance matrix \\( \\Theta \\) for the reference group',
                    placeholder = "1.20, 0.81, 0.32, 0.32"
                    
                  ),
                  textInput(
                    'theta_f',
                    'Input the diagonal of the unique factor variance-covariance matrix \\( \\Theta \\) for the focal group',
                    placeholder = "0.72, 0.81, 0.32, 0.32"
                  ),
                  #sliderInput for size of matrix
                  #also includes title for theta_rMatrix
                  sliderInput(
                    "matrixSlider",
                    "Input the unique factor variance-covariance matrix \\( \\Theta \\)for the reference group",
                    min = 2,
                    max = 5,
                    value = 2
                  ),
                  #uiOutput used for dynamic inputs
                  #allows slider to change size of matrixInput
                  #logic defined in renderUI
                  uiOutput("theta_rMatrixUI"),
                  sliderInput(
                    "theta_fSlider",
                    "Input the unique factor variance-covariance matrix \\( \\Theta \\) for the focal group",
                    min = 2,
                    max = 5,
                    value = 2
                  ),
                  strong(id ="theta_fMatrixTitle","input the unique factor variance-covariance matrix \\( \\theta \\) for the focal group"),
                  uiOutput("theta_fMatrixUI"),
               ),
                #next column
                box(
                  #use propsel
                  switchInput("usepropsel", "Select a part of population?", FALSE),
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
                    value = 0.25,
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
                  switchInput("usephi_f", "Focal group?", FALSE),
                  textInput('legend_r',
                            'Input reference group label',
                            value = "Reference group"),
                  textInput('legend_f',
                            'Input label for the focal group',
                            value = "Focal group")
                )
              ))),
      #output page
      tabItem(tabName = "outputs",
              fluidPage(fluidRow(
                h2("OUTPUTS"),
                fluidRow(
                  box(title = "Relationship Between True Latent Construct Scores
               and Observed Test Scores", plotOutput("distPlot")),
                  box(title = "Impact of Item Bias on Selection Accuracy Indices", tableOutput("table"))
                )
              ))),
      #unused third page
      tabItem(tabName = "other",
              fluidPage(fluidRow(
                h2("Other"),
                fluidRow(box(title = "future content"),)
                
              )))
    )
  )
)

#SERVER-SIDE
server <- function(input, output) {
  #include local file PartInv.R
  source('PartInv.R', local = TRUE)
  
  #renderUI output for Matrix Inputs
  output$theta_fMatrixUI <- renderUI({
    
    #Create matrixInput for focal
    matrixInput(
      "theta_fMatrixInput",
      #use input from matrixSlider to determine x and y size of matrix
      value = matrix("1", input$matrixSlider, input$matrixSlider),
      rows = list(),
      cols = list(names = FALSE,
                  extend = FALSE),
      class = "numeric",
      paste = TRUE,
    )
  })
  
  #reactive expression triggered when input value changes
  #when triggered, reactive expression updates the output
  theta_fMatrixOutput <- reactive({
    input$theta_fMatrixInput
  })
  
  #renderUI output for Matrix Inputs
  output$theta_rMatrixUI <- renderUI({
    #Create matrixInput for referance 
    matrixInput(
      "theta_rMatrixInput",
      #use input from matrixSlider to determine x and y size of matrix
      value = matrix("1", input$matrixSlider, input$matrixSlider),
      rows = list(),
      cols = list(names = FALSE,
                  extend = FALSE),
      class = "numeric",
      paste = TRUE,
    )
  })
  
  #reactive expression triggered when input value changes
  #when triggered, reactive expression updates the output
  theta_rMatrixOutput <- reactive({
    input$theta_rMatrixInput
  })
  
  #when a button press is observed for usepropsel
  observeEvent(input$usepropsel, {
    #this function evaluates this statement
    if (input$usepropsel == TRUE) {
      #and shows/hides the appropriate inputs
      shinyjs::hide(id = "cut_z")
      shinyjs::show(id = "prop")
    } else{
      shinyjs::show(id = "cut_z")
      shinyjs::hide(id = "prop")
    }
  })
  #when a button press is observed for uselambda_f
  observeEvent(input$uselambda_f, {
    #this funtion toggles between show and hide for the input
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
  
  #listens for either a button press from usetheta_f or from useMatrix
  listenFromThetaMatrix <- reactive({
    list(input$usetheta_f, input$useMatrix)
  })
  #if either button press is observed
  observeEvent(listenFromThetaMatrix(), {
    #for every combination of the two buttons, show/hide the appropriate inputs
    if (input$useMatrix == FALSE & input$usetheta_f == TRUE) {
      shinyjs::show(id = "theta_f")
      shinyjs::show(id = "theta_r")
      
      shinyjs::hide(id = "matrixSlider")
      shinyjs::hide(id = "theta_rMatrixUI")
      shinyjs::hide(id = "theta_fMatrixTitle")
      shinyjs::hide(id = "theta_fMatrixUI")
    }
    else if (input$useMatrix == FALSE & input$usetheta_f == FALSE) {
      shinyjs::hide(id = "theta_f")
      shinyjs::show(id = "theta_r")
      
      shinyjs::hide(id = "theta_rMatrixUI")
      shinyjs::hide(id = "matrixSlider")
      shinyjs::hide(id = "theta_fMatrixTitle")
      shinyjs::hide(id = "theta_fMatrixUI")
    }
    else if (input$useMatrix == TRUE & input$usetheta_f == FALSE) {
      shinyjs::hide(id = "theta_f")
      shinyjs::hide(id = "theta_r")
      
      shinyjs::show(id = "matrixSlider")
      shinyjs::show(id = "theta_rMatrixUI")
      shinyjs::hide(id = "theta_fMatrixTitle")
      shinyjs::hide(id = "theta_fMatrixUI")
    }
    else{
      shinyjs::hide(id = "theta_f")
      shinyjs::hide(id = "theta_r")
      
      shinyjs::show(id = "theta_rMatrixUI")
      shinyjs::show(id = "theta_fMatrixTitle")
      shinyjs::show(id = "theta_fMatrixUI")
      shinyjs::show(id = "matrixSlider")
    }
    
  })
  
  #if resetButton is pressed
  observeEvent(input$resetButton, {
    #reset all
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
    reset("legend_r")
    reset("legend_f")
  })
  
  #turn every textInput into a numeric list
  #reactive allows these values to be defined outside of the output render
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
  
  
  #set lambda_f to lambda_r input or lambda_f input depending on button press
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
  
  #set theta_f output to either the diagonal inputs or matrix inputs
  #for theta_r and theta_f depending on button presses
  theta_f <- reactive({
    if (input$usetheta_f == FALSE & input$useMatrix == FALSE) {
      theta_f = diag(theta_rNumeric())
    }
    else if (input$usetheta_f == TRUE & input$useMatrix == FALSE) {
      theta_f = diag(theta_fNumeric())
    }
    else if (input$usetheta_f == TRUE & input$useMatrix == TRUE) {
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
  
  
  
  #distribution plot output
  output$distPlot <- renderPlot({
    
    #makes sure inputs are filled before program runs
    #prevents error messages from popping up
    #displays message telling user what inputs need to be filled
    validate(
      need(input$lambda_r, "Input for factor loadings of reference group is missing\n"),
      need(input$tau_r, "Input for actor variance-covariance matrix of reference group is missing\n"),
      #only checks for numeric input of theta_r when matrix is not being used as input
      if(input$useMatrix == FALSE){
        need(input$theta_r,"Input for measurement intercepts of reference group is missing\n")
      }
    )
    
    if (input$usepropsel == FALSE) {
      #plug everything into PartInv function
      #calls to reactive funtions have () brackets
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
        Theta_r = theta_r(),
        labels = c(input$legend_r, input$legend_f)
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
        Theta_r = theta_r(),
        labels = c(input$legend_r, input$legend_f)
      )
    }
  })
  
  output$table <- renderTable(rownames = TRUE, {
    
    #makes sure inputs are filled before program runs
    #prevents error messages from popping up
    validate(
      need(input$lambda_r,""),
      need(input$tau_r,""),
      #only checks for numeric input of theta_r when matrix is not being used as input
      if(input$useMatrix == FALSE){
        need(input$theta_r,"")
      }
    )
    
    if (input$usepropsel == FALSE) {
      #plug everything into PartInv function
      #calls to reactive funtions have () brackets
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
        Theta_r = theta_r(),
        labels = c(input$legend_r, input$legend_f)
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
        Theta_r = theta_r(),
        labels = c(input$legend_r, input$legend_f)
      )[[4]]
    }
  })
}

# setting up the server
shinyApp(ui = ui, server = server)
