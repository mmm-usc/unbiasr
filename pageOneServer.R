#server-side for pageOne

#renderUI output for Matrix Inputs
output$theta_fMatrixUI <- renderUI({
  #Create matrixInput for focal
  matrixInput(
    "theta_fMatrixInput",
    #use length of lamda_rNumeric() vector function to determine x and y size of matrix
    value = matrix("0", input$matrixSlider, input$matrixSlider),
    rows = list(names = FALSE),
    cols = list(names = FALSE),
    class = "numeric",
    paste = TRUE,
  )
})
#reactive expression triggered when input value changes
theta_fMatrixOutput <- reactive({
  input$theta_fMatrixInput
})
#renderUI output for Matrix Inputs
output$theta_rMatrixUI <- renderUI({
  #Create matrixInput for reference
  matrixInput(
    inputId = "theta_rMatrixInput",
    #use length of lamda_rNumeric() for dynamic x and y size of matrix
    value = matrix("0", input$matrixSlider, input$matrixSlider),
    rows = list(names = FALSE),
    cols = list(names = FALSE),
    class = "numeric"
  )
})
#reactive expression triggered when input value changes
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
    shinyjs::hide(id = "matrixSlider")
    
  }
  else if (input$useMatrix == FALSE & input$usetheta_f == FALSE) {
    shinyjs::hide(id = "theta_f")
    shinyjs::show(id = "theta_r")
    shinyjs::hide(id = "theta_rMatrixTitle")
    shinyjs::hide(id = "theta_rMatrixUI")
    shinyjs::hide(id = "theta_fMatrixTitle")
    shinyjs::hide(id = "theta_fMatrixUI")
    shinyjs::hide(id = "matrixSlider")
  }
  else if (input$useMatrix == TRUE & input$usetheta_f == FALSE) {
    shinyjs::hide(id = "theta_f")
    shinyjs::hide(id = "theta_r")
    shinyjs::show(id = "theta_rMatrixTitle")
    shinyjs::show(id = "theta_rMatrixUI")
    shinyjs::hide(id = "theta_fMatrixTitle")
    shinyjs::hide(id = "theta_fMatrixUI")
    shinyjs::show(id = "matrixSlider")
  }
  else{
    shinyjs::hide(id = "theta_f")
    shinyjs::hide(id = "theta_r")
    shinyjs::show(id = "theta_rMatrixTitle")
    shinyjs::show(id = "theta_rMatrixUI")
    shinyjs::show(id = "theta_fMatrixTitle")
    shinyjs::show(id = "theta_fMatrixUI")
    shinyjs::show(id = "matrixSlider")
  }
})
#if resetButton is pressed
observeEvent(input$resetButton, {
  #reset all
  reset("matrixSlider")
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
      "Input for factor variance-covariance matrix of reference group is missing\n"
    ),
    need(
      length(lambda_rNumeric()) == length(tau_rNumeric()),
      "Factor loadings and intercepts need to have the same value\n"
    ),
    if(input$uselambda_f == TRUE){
      need(
        length(lambda_rNumeric()) == length(lambda_fNumeric()),
        "factor loadings for referance and focal groups need to have the same number of values\n"
      )
    },
    if(input$usetau_f == TRUE){
      need(
        length(tau_rNumeric()) == length(tau_fNumeric()),
        "intercepts for referance and focal groups need to have the same number of values\n"
      )
    },
    
    #only checks for numeric input of theta_r when matrix is not being used as input
    if (input$useMatrix == FALSE) {
      need(input$theta_r,
           "Input for measurement intercepts of reference group is missing\n")
    },
    if (input$useMatrix == TRUE) {
      need(input$matrixSlider == length(lambda_rNumeric()), "Matrix dimensions must match # loadings and intercepts\n")
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
      "Input for measurement intercepts of reference group is missing\n"
    ),
    need(
      length(lambda_rNumeric()) == length(tau_rNumeric()),
      "loadings and intercepts need to have the same dimension\n"
    ),
    if(input$uselambda_f == TRUE){
      need(
        length(lambda_rNumeric()) == length(lambda_fNumeric()),
        "factor loadings for referance and focal groups need to have the same number of values\n"
      )
    },
    if(input$usetau_f == TRUE){
      need(
        length(tau_rNumeric()) == length(tau_fNumeric()),
        "intercepts for referance and focal groups need to have the same number of values\n"
      )
    },
    #only checks for numeric input of theta_r when matrix is not being used as input
    if (input$useMatrix == FALSE) {
      need(input$theta_r,
           "Input for unique variance-covariance matrix of reference group is missing\n")
    },
    if (input$useMatrix == TRUE) {
      need(input$matrixSlider == length(lambda_rNumeric()), "Matrix dimensions must match # loadings and intercepts\n")
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