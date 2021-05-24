
# Load required packages
library(shiny)
library(shinydashboard) #GUI
library(shinyjs) #show/hide/toggle, reset, inlineCSS
library(shinyWidgets) #materialSwitch
library(shinyMatrix) #matrixInput
library(rsconnect) #connect to server Shinyapps.io

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
          .col-sm-4 {padding:12px !important;}
          .col-sm-8 {padding:12px !important;}'
    ),
    #pages
    tabItems(
      #page one
      tabItem(tabName = "firstpage",
              #withMathJax() for greek char display
              fluidPage(
                withMathJax(), fluidRow(
                  box(
                    title = "directions",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    p(
                      "Pellentesque aliquam, nibh in posuere ullamcorper, nisl tortor tempus ipsum, id elementum diam orci eget ex. Morbi id lacus libero. Vestibulum fermentum imperdiet ultricies. In volutpat eleifend tincidunt. Duis luctus ligula eget lorem sollicitudin maximus. Nunc cursus interdum orci, eu auctor lorem pellentesque eu. Ut tincidunt mauris a mi consequat condimentum. Integer semper ultrices eros, a suscipit dolor porttitor sit amet."
                    )
                  ),
                  box(
                    title = "inputs",
                    status = "primary",
                    solidHeader = FALSE,
                    width = 4,
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
                    #materialSwitch button using shinyWidgets
                    materialSwitch("uselambda_f", "Focal group?", status = "primary", FALSE),
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
                    materialSwitch("usetau_f", "Focal group?", status = "primary", FALSE),
                    h4('unique factor variance-covariance'),
                    
                    #div to style buttons inline
                    div(
                      style = "display:inline-block, float:right",
                      materialSwitch(
                        "useMatrix",
                        "Matrix input?",
                        status = "primary",
                        FALSE,
                        inline = TRUE
                      ),
                      materialSwitch(
                        "usetheta_f",
                        "Focal group?",
                        status = "primary",
                        FALSE,
                        inline = TRUE
                      )
                    ),
                    
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
                    
                    #strong is a text output in bold
                    strong(
                      id = "theta_rMatrixTitle",
                      "input the unique factor variance-covariance matrix \\( \\Theta \\) for the referance group"
                    ),
                    #uiOutput used for dynamic inputs
                    #Outputs matrix rows & cols based on number of values in lambda_r
                    #logic defined in renderUI
                    uiOutput("theta_rMatrixUI"),
                    strong(
                      id = "theta_fMatrixTitle",
                      "input the unique factor variance-covariance matrix \\( \\Theta \\) for the focal group"
                    ),
                    uiOutput("theta_fMatrixUI"),
                    
                    materialSwitch("usepropsel", "Select 10% population?", status = "primary", FALSE),
                    #numeric input for single number values
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
                    materialSwitch("usekappa_f", "Focal group?", status = "primary", FALSE),
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
                    materialSwitch("usephi_f", "Focal group?", status = "primary", FALSE),
                    textInput('legend_r',
                              'Input reference group label',
                              value = "Reference group"),
                    textInput('legend_f',
                              'Input label for the focal group',
                              value = "Focal group")
                  ),
                  
                  box(
                    id = "outputBox",
                    title = "Relationship Between True Latent Construct Scores
               and Observed Test Scores",
                    status = "primary",
                    solidHeader = FALSE,
                    width = 8,
                    plotOutput("distPlot")
                  ),
                  box(
                    id = "outputTableBox",
                    title = "Impact of Item Bias on Selection Accuracy Indices",
                    status = "primary",
                    solidHeader = FALSE,
                    width = 8,
                    tableOutput("table")
                  )
                  
                  
                )
              )),
      
      #second page
      tabItem(tabName = "other",
              fluidPage(fluidRow()))
    )
  )
)

#SERVER-SIDE
server <- function(input, output) {
  #include local file PartInv.R
  source('PartInv.R', local = TRUE)
  #renderUI output for Matrix Inputs
  output$theta_fMatrixUI <- renderUI({
    #validate makes sure need statements are true before running
    validate(
      #don't execute until loadings and intercepts have the same number of values
      #display message if need statement not met
      need(
        length(lambda_rNumeric()) == length(tau_rNumeric()),
        "loadings and intercepts need to have the same value"
      ),
      need(
        input$lambda_r,
        "Input for factor loadings of reference group is missing\n"
      ),
      need(
        input$tau_r,
        "Input for actor variance-covariance matrix of reference group is missing\n"
      )
    )
    #Create matrixInput for focal
    matrixInput(
      "theta_fMatrixInput",
      #use length of lamda_rNumeric() vector function to determine x and y size of matrix
      value = matrix("0", length(lambda_rNumeric()), length(lambda_rNumeric())),
      rows = list(names = FALSE),
      cols = list(names = FALSE),
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
    validate(
      need(
        length(lambda_rNumeric()) == length(tau_rNumeric()),
        "loadings and intercepts need to have the same value"
      ),
      need(
        input$lambda_r,
        "Input for factor loadings of reference group is missing\n"
      ),
      need(
        input$tau_r,
        "Input for actor variance-covariance matrix of reference group is missing\n"
      )
    )
    #Create matrixInput for reference
    matrixInput(
      inputId = "theta_rMatrixInput",
      #use length of lamda_rNumeric() for dynamic x and y size of matrix
      value = matrix("0", length(lambda_rNumeric()), length(lambda_rNumeric())),
      rows = list(names = FALSE),
      cols = list(names = FALSE),
      class = "numeric"
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
      shinyjs::hide(id = "theta_rMatrixTitle")
      shinyjs::hide(id = "theta_rMatrixUI")
      shinyjs::hide(id = "theta_fMatrixTitle")
      shinyjs::hide(id = "theta_fMatrixUI")
    }
    else if (input$useMatrix == FALSE & input$usetheta_f == FALSE) {
      shinyjs::hide(id = "theta_f")
      shinyjs::show(id = "theta_r")
      shinyjs::hide(id = "theta_rMatrixTitle")
      shinyjs::hide(id = "theta_rMatrixUI")
      shinyjs::hide(id = "theta_fMatrixTitle")
      shinyjs::hide(id = "theta_fMatrixUI")
    }
    else if (input$useMatrix == TRUE & input$usetheta_f == FALSE) {
      shinyjs::hide(id = "theta_f")
      shinyjs::hide(id = "theta_r")
      shinyjs::show(id = "theta_rMatrixTitle")
      shinyjs::show(id = "theta_rMatrixUI")
      shinyjs::hide(id = "theta_fMatrixTitle")
      shinyjs::hide(id = "theta_fMatrixUI")
    }
    else{
      shinyjs::hide(id = "theta_f")
      shinyjs::hide(id = "theta_r")
      shinyjs::show(id = "theta_rMatrixTitle")
      shinyjs::show(id = "theta_rMatrixUI")
      shinyjs::show(id = "theta_fMatrixTitle")
      shinyjs::show(id = "theta_fMatrixUI")
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
      need(
        input$lambda_r,
        "Input for factor loadings of reference group is missing\n"
      ),
      need(
        input$tau_r,
        "Input for actor variance-covariance matrix of reference group is missing\n"
      ),
      need(
        length(lambda_rNumeric()) == length(tau_rNumeric()),
        "loadings and intercepts need to have the same value"
      ),
      #only checks for numeric input of theta_r when matrix is not being used as input
      if (input$useMatrix == FALSE) {
        need(input$theta_r,
             "Input for measurement intercepts of reference group is missing\n")
      },
      if (input$useMatrix == TRUE) {
        need(length(unique(theta_r())) / length(lambda_rNumeric())[1] != 1,
             "matrix empty")
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
      need(
        input$lambda_r,
        "Input for factor loadings of reference group is missing\n"
      ),
      need(
        input$tau_r,
        "Input for actor variance-covariance matrix of reference group is missing\n"
      ),
      need(
        length(lambda_rNumeric()) == length(tau_rNumeric()),
        "loadings and intercepts need to have the same value"
      ),
      #only checks for numeric input of theta_r when matrix is not being used as input
      if (input$useMatrix == FALSE) {
        need(input$theta_r,
             "Input for measurement intercepts of reference group is missing\n")
      },
      if (input$useMatrix == TRUE) {
        need(length(unique(theta_r())) / length(lambda_rNumeric())[1] != 1,
             "matrix empty")
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
#server component
shinyApp(ui = ui, server = server)
