#' Launch the 'PartInv' app
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs inlineCSS reset
#' @importFrom shinyWidgets materialSwitch
#' @importFrom shinyMatrix matrixInput
#' @export
myApp <- function(...) {
  ui <- dashboardPage(
    dashboardHeader(title = "Computation of Selection Accuracy Indexes",
                    titleWidth = 400),
    dashboardSidebar(width = 150,
                     sidebarMenu(
                       menuItem(
                         "Instruction",
                         tabName = "Reference",
                         icon = icon("dashboard")
                       ),
                       menuItem(
                         "Single dimension",
                         tabName = "Single",
                         icon = icon("dashboard")
                       ),
                       #unused
                       menuItem("Multidimensions", tabName = "Multiple", 
                                icon = icon("dashboard"))
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
        tabItem(tabName = "Reference",
                fluidPage(fluidRow(
                  box(
                    title = "Summary of the framework",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    p(
                      "The multidimensional classification accuracy analysis framework (MCAA) is an extension of the selection accuracy
                    framework proposed by Millsap & Kwok (2004). Since real-world selection usually involves multiple tests or subtests
                    with different weights assigned to each dimension, the MCAA framework is proposed to quantify
                    the impact of item bias on selection accuracy by examining the changes in 
                    selection accuracy indices (proportion selected, success ratio, 
                    sensitivity, specificity) for each subgroup. The adverse impact (AI) ratio 
                    (i.e., the ratio of the proportions selected between a minority and a majority group with matching latent trait levels) 
                    is also provided in results. Further details are provided in Lai & Zhang (2022)."
                    )),
                  box(
                    title = "Example",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    p(
                      "Here is an example for how to use the app."),
                    p("The first step is to input group labels for the graph."
                    ),
                    
                    img(src="step1.png", align = "center",height='300px',width='270px'),
                    p("Next, input factor loadings and intercepts
                       for each group."
                    ),
                    img(src="step2.png", align = "center",height='400px',width='270px'),
                    p("Third, input unique factor variance-covariance matrix. You can choose to enter the diagonal 
                      of the covariance matrix, or upload the covariance matrix. Note the header should not be included. "
                    ),
                    img(src="step3.png", align = "center",height='400px',width='280px'),
                    p("Then, input latent factor
                      means and variances. Last, set selection parameters such as mixing proportion and selection proportion."
                    ),
                    img(src="step4.png", align = "center",height='400px',width='200px'),
                    p("Lastly, set selection parameters such as mixing proportion and selection proportion."
                    ),
                    img(src="step5.png", align = "center",height='300px',width='270px'),
                    p("Here are the graph and the selection accuracy table"
                    ),
                    img(src="graph.png", align = "left",height='300px',width='300px'),
                    img(src="table.png", align = "right",height='280px',width='330px'),
                  ),
                  
                  box(
                    title = "Reference",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    p("Lai, M. H. C., Kwok, O., Yoon, M., & Hsiao, Y. (2017). 
                        Understanding the impact of partial factorial invariance on selection 
                        accuracy: An R script. Structural Equation Modeling: A Multidisciplinary 
                        Journal, 24, 783-799. https://doi.org/10.1080/10705511.2017.1318703"), 
                    p(
                      "Lai, M. H. C., & Zhang, Y (2022). Classification accuracy of multidimensional tests: 
                      Quantifying the impact of noninvariance. Structural Equation Modeling: 
                      A Multidisciplinary Journal. https://doi.org/10 .1080/10705511.2021.1977936"),
                    p("Millsap, R. E., & Kwok, O.-M. (2004). Evaluating the impact of 
                        partial factorial invariance on selection in two populations. 
                        Psychological Methods, 9, 93--115. https://doi.org/10.1037/1082-989X.9.1.93."
                    )),
                  
                  box(
                    title = "Note",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    p(
                      "If you have any questions, please email us at mmm.lab.usc@zohomail.com ."
                    )
                  )
                ))),
        #second page
        tabItem(tabName = "Single", fluidPage(
          #wrap all UI code in list command so it can be accessed from main app.R file
          list(
            #withMathJax adds HTML for greek characters
            withMathJax(),
            #first row which contains the directions
            fluidRow(
              #no column needed for this row because there is only one box in the row
              box(
                title = "Directions",
                #status controls the color
                status = "primary",
                #using a 12-column responsive grid, so this box (width 12) takes up the full container its in, in this case the full row.
                width = 12,
                solidHeader = FALSE,
                p(
                  "This application is designed for the use of the Multidimensional Classification Accuracy Analysis (MCAA) Framework
                      for evaluating measurement invariance in the personnel selection context. By entering the parameter estimates from the factor model,
                      such as the factor loadings and intercepts, you will be able to visualize the impact of item bias on selection accuracy indices and
                      get a table that summarize the change of selection indices."
                )
              )
            ),
            #second row which contains input and output columns
            fluidRow(
              #input column (width 4/12 of the page)
              column(width = 4,
                     box(
                       title = "Inputs",
                       status = "primary",
                       solidHeader = FALSE,
                       #all boxes in this column take up the full width of the column (12/12)
                       width = 12,
                       #HTML styling to increase bottom padding of reset button
                       div(style = "padding-bottom: 10px;",
                           #reset button
                           actionButton("resetButton", "reset inputs")),
                       #text inputs for graph labels
                       textInput('legend_r',
                                 'Input reference group label',
                                 #default value
                                 value = "Reference group"),
                       textInput('legend_f',
                                 'Input focal group label',
                                 value = "Focal group")
                     ),
                     box(
                       title = "Intercepts and Loadings",
                       status = "primary",
                       solidHeader = FALSE,
                       width = 12,
                       #text inputs for multi-value inputs
                       textInput(
                         'lambda_r',
                         'Reference group factor loadings \\( \\lambda \\)',
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
                         'Reference group measurement intercepts \\( \\tau \\)',
                         placeholder = "1.54, 1.36, 1.16, 1.08"
                       ),
                       textInput(
                         'tau_f',
                         'Input measurement intercepts \\( \\tau \\) for the focal group',
                         placeholder = "0.68, 1.36, 1.16, 1.08"
                       ),
                       materialSwitch("usetau_f", "Focal group?", status = "primary", FALSE),
                     ),
                     box(
                       title = "Unique Factor Variance-Covariance",
                       status = "primary",
                       solidHeader = FALSE,
                       width = 12,
                       #HTML style buttons side by side
                       div(style = "display:inline-block, float:right",
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
                           ),
                           sliderInput(
                             "matrixSlider",
                             "Matrix Size",
                             min = 2,
                             max = 10,
                             value = 4
                           )
                       ),
                       textInput(
                         'theta_r',
                         'Reference group uniqueness \\( \\theta \\)',
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
                       #uiOutput used for dynamic inputs, in this case matrices with variable size
                       #logic defined in renderUI in pageOneServer.R
                       uiOutput("theta_rMatrixUI"),
                       strong(
                         id = "theta_fMatrixTitle",
                         "input the unique factor variance-covariance matrix \\( \\Theta \\) for the focal group"
                       ),
                       uiOutput("theta_fMatrixUI"),
                       
                     ),
                     box(
                       title = "Mean and Variances",
                       status = "primary",
                       solidHeader = FALSE,
                       width = 12,
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
                     ),
                     box(
                       title = "Cutoffs and Mixing Proportion",
                       status = "primary",
                       solidHeader = FALSE,
                       width = 12,
                       materialSwitch("usepropsel", "Selection based on percentage?", status = "primary", FALSE),
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
                     ),
                     box(
                       title = "Output",
                       status = "primary",
                       solidHeader = FALSE,
                       width = 12,
                       materialSwitch("tab_strict", "Show strict invariance output?", status = "primary", FALSE),
                     ),
              ),
              box(
                id = "outputBox",
                title = "Relationship Between True Latent Construct Scores
               and Observed Test Scores for Partial Invariance",
               status = "primary",
               solidHeader = FALSE,
               width = 8,
               plotOutput("distPlot")
              ),
              box(
                id = "outputPlot_strict",
                title = "Relationship Between True Latent Construct Scores
               and Observed Test Scores for Strict Invariance",
                status = "primary",
                solidHeader = FALSE,
                width = 8,
                plotOutput("distPlotstrict")
              ),
              box(
                id = "outputTableBox",
                title = "Impact of Item Bias on Selection Accuracy Indices for Partial Strict Invariance",
                status = "primary",
                solidHeader = FALSE,
                width = 8,
                tableOutput("table")
              ),
              box(
                id = "outputTable_strict",
                title = "Impact of Item Bias on Selection Accuracy Indices for Strict Invariance",
                status = "primary",
                solidHeader = FALSE,
                width = 8,
                tableOutput("tablestrict")
              )
            )
          )
          
        )),
        #second page
        tabItem(tabName = "Multiple", fluidPage(
          #wrap all UI code in list command so it can be accessed from main app.R file
          list(
            #withMathJax adds HTML for greek characters
            withMathJax(),
            #first row which contains the directions
            fluidRow(
              #no column needed for this row because there is only one box in the row
              box(
                title = "Directions",
                #status controls the color
                status = "primary",
                #using a 12-column responsive grid, so this box (width 12) takes up the full container its in, in this case the full row.
                width = 12,
                solidHeader = FALSE,
                p(
                  "This application is designed for the use of the Multidimensional Classification Accuracy Analysis (MCAA) Framework
                      for evaluating measurement invariance in the personnel selection context. By entering the parameter estimates from the factor model,
                      such as the factor loadings and intercepts, you will be able to visualize the impact of item bias on selection accuracy indices and
                      get a table that summarize the change of selection indices."
                )
              )
            ),
            fluidRow(
              #input column (width 4/12 of the page)
              column(width = 4,
                     box(
                       title = "Inputs",
                       status = "primary",
                       solidHeader = FALSE,
                       #all boxes in this column take up the full width of the column (12/12)
                       width = 12,
                       #HTML styling to increase bottom padding of reset button
                       div(style = "padding-bottom: 10px;",
                           #reset button
                           actionButton("resetButton", "reset inputs")),
                       #text inputs for graph labels
                       textInput('legend_r',
                                 'Input reference group label',
                                 #default value
                                 value = "Reference group"),
                       textInput('legend_f',
                                 'Input focal group label',
                                 value = "Focal group")
                     ),
                     box(
                       title = "Intercepts and Loadings",
                       status = "primary",
                       solidHeader = FALSE,
                       width = 12,
                       strong(
                         id = "lambda_rmTitle",
                         "Input the loading matrix \\( \\lambda\\) for the referance group"
                       ),
                       #uiOutput used for dynamic inputs, in this case matrices with variable size
                       #logic defined in renderUI in pageOneServer.R
                       uiOutput("lambda_rmUI"),
                       strong(
                         id = "lambda_fmTitle",
                         "Input the loading matrix \\( \\lambda \\) for the focal group"
                       ),
                       uiOutput("lambda_fmUI"),
                       materialSwitch("uselambda_fm", "Focal group?", status = "primary", FALSE),
                       textInput(
                         'tau_rm',
                         'Input measurement intercepts \\( \\tau \\) for the reference group',
                         placeholder = "1.54, 1.36, 1.16, 1.08"
                       ),
                       textInput(
                         'tau_fm',
                         'Input measurement intercepts \\( \\tau \\) for the focal group',
                         placeholder = "0.68, 1.36, 1.16, 1.08"
                       ),
                       materialSwitch("usetau_fm", "Focal group?", status = "primary", FALSE),
                     ),
              
              box(
                title = "Unique Factor Variance-Covariance",
                status = "primary",
                solidHeader = FALSE,
                width = 12,
                #HTML style buttons side by side
                div(style = "display:inline-block, float:right",
                    materialSwitch(
                      "useMatrixm",
                      "Matrix input?",
                      status = "primary",
                      FALSE,
                      inline = TRUE
                    ),
                    materialSwitch(
                      "usetheta_fm",
                      "Focal group?",
                      status = "primary",
                      FALSE,
                      inline = TRUE
                    ),
                    sliderInput(
                      "matrixSliderm",
                      "Matrix Size",
                      min = 2,
                      max = 10,
                      value = 4
                    )
                ),
                textInput(
                  'theta_rm',
                  'Input the diagonal of the unique factor variance-covariance matrix \\( \\Theta \\) for the reference group',
                  placeholder = "1.20, 0.81, 0.32, 0.32"
                ),
                textInput(
                  'theta_fm',
                  'Input the diagonal of the unique factor variance-covariance matrix \\( \\Theta \\) for the focal group',
                  placeholder = "0.72, 0.81, 0.32, 0.32"
                ),
                #strong is a text output in bold
                strong(
                  id = "theta_rMatrixm",
                  "Input the unique factor variance-covariance matrix \\( \\Theta \\) for the referance group"
                ),
                #uiOutput used for dynamic inputs, in this case matrices with variable size
                #logic defined in renderUI in pageOneServer.R
                uiOutput("theta_rMatrixmUI"),
                strong(
                  id = "theta_fMatrixm",
                  "Input the unique factor variance-covariance matrix \\( \\Theta \\) for the focal group"
                ),
                uiOutput("theta_fMatrixmUI")
              ),
              box(
                title = "Mean and Variances",
                status = "primary",
                solidHeader = FALSE,
                width = 12,
                numericInput(
                  "alpha_rm",
                  "Latent factor mean \\( \\alpha \\) for the reference group:",
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.01
                ),
                numericInput(
                  "alpha_fm",
                  "Latent factor mean \\( \\alpha \\) for the focal group:",
                  value = 0.0,
                  min = 0,
                  max = 1,
                  step = 0.01
                ),
                materialSwitch("usealpha_fm", "Focal group?", status = "primary", FALSE),
                numericInput(
                  "psi_rm",
                  "Latent factor variance \\( \\psi \\) for the reference group:",
                  value = 1.,
                  min = 0,
                  max = 1,
                  step = 0.01
                ),
                numericInput(
                  "psi_fm",
                  "Latent factor variance \\( \\psi \\) for the focal group:",
                  value = 1.,
                  min = 0,
                  max = 1,
                  step = 0.01
                ),
                materialSwitch("usepsi_fm", "Focal group?", status = "primary", FALSE),
              ),
              box(
                title = "Cutoffs and Mixing Proportion",
                status = "primary",
                solidHeader = FALSE,
                width = 12,
                materialSwitch("usepropsel_m", "Selection based on percentage?", status = "primary", FALSE),
                #numeric input for single number values
                numericInput(
                  "cut_z_m",
                  "Cutoff score on the observed composite:",
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.01
                ),
                numericInput(
                  "prop_m",
                  "Selection proportion:",
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.01
                ),
                numericInput(
                  "pmix_m",
                  "Mixing proportion:",
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.01
                ),
              ),
              box(
                title = "Output",
                status = "primary",
                solidHeader = FALSE,
                width = 12,
                materialSwitch("tab_strict_m", "Show strict invariance output?", status = "primary", FALSE),
              ),
          ),
          box(
            id = "outputBox_m",
            title = "Relationship Between True Latent Construct Scores
               and Observed Test Scores",
            status = "primary",
            solidHeader = FALSE,
            width = 8,
            plotOutput("distPlot_m")
          ),
          box(
            id = "outputTable_m",
            title = "Impact of Item Bias on Selection Accuracy Indices",
            status = "primary",
            solidHeader = FALSE,
            width = 8,
            tableOutput("table_m")
          ),
          box(
            id = "outputTablestrict_m",
            title = "Impact of Item Bias on Selection Accuracy Indices",
            status = "primary",
            solidHeader = FALSE,
            width = 8,
            tableOutput("stricttable_m")
          )
          )
          ))
        
          )
      )
    )
  )
  server <- function(input, output) {
    
    #renderUI output for Matrix Inputs
    output$theta_fMatrixUI <- renderUI({
      #matrixInput for focal
      matrixInput(
        "theta_fMatrixInput",
        #use slider value to determine dimensions of matrix input
        value = matrix("0", input$matrixSlider, input$matrixSlider),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric",
      )
    })
    
    #renderUI output for Matrix Inputs
    output$theta_rMatrixUI <- renderUI({
      #matrixInput for referance
      matrixInput(
        inputId = "theta_rMatrixInput",
        value = matrix("0", input$matrixSlider, input$matrixSlider),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric"
      )
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

    
    #if either button press is observed
    observeEvent(list(input$usetheta_f, input$useMatrix), {
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
    
    observeEvent(input$tab_strict, {
      if (input$tab_strict == TRUE){
        shinyjs::show(id = "outputTable_strict")
      } else{
        shinyjs::hide(id = "outputTable_strict")
      }
    })
    
    observeEvent(input$tab_strict, {
      if (input$tab_strict == TRUE){
        shinyjs::show(id = "outputPlot_strict")
      } else{
        shinyjs::hide(id = "outputPlot_strict")
      }
    })
    # 
    #if resetButton is pressed
    observeEvent(input$resetButton, {
      #reset all
      reset("matrixSlider")
      reset("usepropsel")
      reset("tab_strict")
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
      reset("uselambda_fm")
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
        theta_f = input$theta_fMatrixInput
      }
      else{
        theta_f = input$theta_rMatrixInput
      }
    })
    
    theta_r <- reactive({
      if (input$useMatrix == TRUE) {
        theta_r = input$theta_rMatrixInput
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
    
    ####multiple dimensions#################
    output$lambda_rmUI <- renderUI({
      #matrixInput for focal
      matrixInput(
        "lambda_rmInput",
        #use slider value to determine dimensions of matrix input
        value = matrix("0", input$matrixSlider, input$matrixSlider),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric",
      )
    })
    output$lambda_fmUI <- renderUI({
      #matrixInput for focal
      matrixInput(
        "lambda_fmInput",
        #use slider value to determine dimensions of matrix input
        value = matrix("0", input$matrixSlider, input$matrixSlider),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric",
      )
    })
    
    #when a button press is observed for usepropsel
    observeEvent(input$usepropsel_m, {
      #this function evaluates this statement
      if (input$usepropsel_m == TRUE) {
        #and shows/hides the appropriate inputs
        shinyjs::hide(id = "cut_z")
        shinyjs::show(id = "prop")
      } else{
        shinyjs::show(id = "cut_z")
        shinyjs::hide(id = "prop")
      }
    })
    #if either button press is observed
    observeEvent(list(input$usetheta_fm, input$useMatrixm), {
      #for every combination of the two buttons, show/hide the appropriate inputs
      if (input$useMatrixm == FALSE & input$usetheta_fm == TRUE) {
        shinyjs::show(id = "theta_fm")
        shinyjs::show(id = "theta_rm")
        shinyjs::hide(id = "theta_rMatrixm")
        shinyjs::hide(id = "theta_rMatrixmUI")
        shinyjs::hide(id = "theta_fMatrixm")
        shinyjs::hide(id = "theta_fMatrixmUI")
        shinyjs::hide(id = "matrixSliderm")
        
      }
      else if (input$useMatrixm == FALSE & input$usetheta_fm == FALSE) {
        shinyjs::hide(id = "theta_fm")
        shinyjs::show(id = "theta_rm")
        shinyjs::hide(id = "theta_rMatrixm")
        shinyjs::hide(id = "theta_rMatrixmUI")
        shinyjs::hide(id = "theta_fMatrixm")
        shinyjs::hide(id = "theta_fMatrixmUI")
        shinyjs::hide(id = "matrixSliderm")
      }
      else if (input$useMatrixm == TRUE & input$usetheta_fm == FALSE) {
        shinyjs::hide(id = "theta_fm")
        shinyjs::hide(id = "theta_rm")
        shinyjs::show(id = "theta_rMatrixm")
        shinyjs::show(id = "theta_rMatrixmUI")
        shinyjs::hide(id = "theta_fMatrixm")
        shinyjs::hide(id = "theta_fMatrixmUI")
        shinyjs::show(id = "matrixSliderm")
      }
      else{
        shinyjs::hide(id = "theta_fm")
        shinyjs::hide(id = "theta_rm")
        shinyjs::show(id = "theta_rMatrixm")
        shinyjs::show(id = "theta_rMatrixmUI")
        shinyjs::show(id = "theta_fMatrixm")
        shinyjs::show(id = "theta_fMatrixmUI")
        shinyjs::show(id = "matrixSliderm")
      }
    })
    
    observeEvent(input$tab_strict_m, {
      if (input$tab_strict_m == TRUE){
        shinyjs::show(id = "outputTablestrict_m")
      } else{
        shinyjs::hide(id = "outputTablestrict_m")
      }
    })
    
    #when a button press is observed for uselambda_f
    observeEvent(input$uselambda_fm, {
      if(input$uselambda_fm == TRUE){
        shinyjs::show(id = "lambda_fmUI")
        shinyjs::show(id = "lambda_fmTitle")
      } else{
        shinyjs::hide(id = "lambda_fmUI")
        shinyjs::hide(id = "lambda_fmTitle")
      }
    })
    observeEvent(input$usetau_fm, {
      shinyjs::toggle(id = "tau_fm")
    })
    observeEvent(input$usealpha_fm, {
      shinyjs::toggle(id = "alpha_fm")
    })
    observeEvent(input$usepsi_fm, {
      shinyjs::toggle(id = "psi_fm")
    })

    tau_rNumeric_m <- reactive({
      as.numeric(unlist(strsplit(input$tau_rm, ",")))
    })
    tau_fNumeric_m <- reactive({
      as.numeric(unlist(strsplit(input$tau_fm, ",")))
    })
    theta_rNumeric_m <- reactive({
      as.numeric(unlist(strsplit(input$theta_rm, ",")))
    })
    theta_fNumeric_m <- reactive({
      as.numeric(unlist(strsplit(input$theta_fm, ",")))
    })
    
    #set lambda_f to lambda_r input or lambda_f input depending on button press
    lambda_fm <- reactive({
      if (input$uselambda_fm == FALSE) {
        lambda_f = lambda_rNumeric_m()
      }
      else{
        lambda_f = lambda_fNumeric_m()
      }
    })
    tau_fm <- reactive({
      if (input$usetau_f == FALSE) {
        tau_f = tau_rNumeric_m()
      }
      else{
        tau_f = tau_fNumeric_m()
      }
    })
    theta_fm <- reactive({
      if (input$usetheta_fm == FALSE & input$useMatrixm == FALSE) {
        theta_f = diag(theta_rNumeric_m())
      }
      else if (input$usetheta_fm == TRUE & input$useMatrixm == FALSE) {
        theta_f = diag(theta_fNumeric_m())
      }
      else if (input$usetheta_fm == TRUE & input$useMatrixm == TRUE) {
        theta_f = input$theta_fMatrixm
      }
      else{
        theta_f = input$theta_rMatrixm
      }
    })
    
    theta_rm <- reactive({
      if (input$useMatrixm == TRUE) {
        theta_r = input$theta_rMatrixm
      }
      else{
        theta_r = diag(theta_rNumeric_m())
      }
    })
    alpha_fm <- reactive({
      if (input$usealpha_fm == FALSE) {
        alpha_f = input$alpha_rm
      }
      else{
        alpha_f = input$alpha_fm
      }
    })
    psi_fm <- reactive({
      if (input$usepsi_fm == FALSE) {
        psi_f = input$psi_rm
      }
      else{
        psi_f = input$psi_fm
      }
    })
    
    #distribution plot output
    
    validations <- reactive({
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
        if (input$uselambda_f == TRUE) {
          need(
            length(lambda_rNumeric()) == length(lambda_fNumeric()),
            "factor loadings for referance and focal groups need to have the same number of values\n"
          )
        },
        if (input$usetau_f == TRUE) {
          need(
            length(tau_rNumeric()) == length(tau_fNumeric()),
            "intercepts for referance and focal groups need to have the same number of values\n"
          )
        },
        if (input$useMatrix == FALSE && input$usetheta_f == TRUE) {
          need(
            length(theta_rNumeric()) == length(theta_fNumeric()),
            "(placeholder) diagonals of referance and focal groups must match"
          )
        },
        #only checks for numeric input of theta_r when matrix is not being used as input
        if (input$useMatrix == FALSE) {
          need(
            input$theta_r,
            "Input for unique variance-covariance matrix of reference group is missing\n"
          )
        },
        if (input$useMatrix == FALSE && length(theta_rNumeric()) > 0) {
          need(
            length(theta_rNumeric()) == length(lambda_rNumeric()),
            "number of inputs for measurement intercepts must match factor loadings"
          )
        },
        if (input$useMatrix == TRUE) {
          need(
            input$matrixSlider == length(lambda_rNumeric()),
            "Matrix dimensions must match # loadings and intercepts\n"
          )
        },
        if (input$useMatrix == TRUE) {
          need(length(unique(theta_r())) / length(lambda_rNumeric())[1] != 1,
               "reference matrix empty")
        },
        if (input$useMatrix == TRUE && input$usetheta_f == TRUE) {
          need(length(unique(theta_f())) / length(lambda_rNumeric())[1] != 1,
               "focal matrix empty")
        },
        if (input$useMatrix == TRUE) {
          need(isSymmetric(input$theta_rMatrixInput) &&
                 is_symmetric_posdef(input$theta_rMatrixInput),
               "reference Theta matrix not positive definite")
        },
        if (input$useMatrix == TRUE && input$usetheta_f == TRUE) {
          need(isSymmetric(input$theta_fMatrixInput) &&
                 is_symmetric_posdef(input$theta_fMatrixInput),
               "focal Theta matrix not positive definite")
        }
        
      )})
    
    partInvOutput <- reactive({
      if (input$usepropsel == FALSE) {
        #plug everything into PartInv function
        #calls to reactive functions have () brackets
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
          labels = c(input$legend_r, input$legend_f),
          show_mi_result = input$tab_strict
        )
      } else if (input$usepropsel == TRUE) {
        PartInv(
          #propsel adds value as input if true
          propsel = input$prop,
          plot_contour = FALSE,
          # cut_z = input$cut_z,
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
          labels = c(input$legend_r, input$legend_f),
          show_mi_result = input$tab_strict
        )
      }
    })
    
    output$distPlot <- renderPlot({
      validations()
      plot(partInvOutput())
    })
    
    output$table <- renderTable(rownames = TRUE, {
      validations()
<<<<<<< HEAD
      partInvOutput()[[1]]
=======
      partInvOutput()$summary
>>>>>>> 5a0746a781aec4a6a49a63855d9fbb1c14404a0a
    })
    
    output$tablestrict <- renderTable(rownames = TRUE, {
      validations()
<<<<<<< HEAD
      partInvOutput()[[3]]
=======
      partInvOutput()$summary_mi
>>>>>>> 5a0746a781aec4a6a49a63855d9fbb1c14404a0a
    })
    
    output$distPlot_m <- renderTable(rownames = TRUE, {
      validations()
      partInvOutput()
    })
    output$table_m <- renderTable(rownames = TRUE, {
      validations()
      if (input$tab_strictm == FALSE) {
        partInvOutput()
      }else{
        partInvOutput()[1]
      }
    })
    output$stricttable_m <- renderTable(rownames = TRUE, {
      validations()
      partInvOutput()[2]
    })

  }
  shinyApp(ui, server = server, ...)
}
