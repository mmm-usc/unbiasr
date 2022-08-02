#' Launching Partial Invariance Evaluation Shinyapp
#'
#' \code{launch, myApp} launch the Shinyapp designed for the use of the
#' Multidimensional Classification Accuracy Analysis
#' (MCAA) Framework for evaluating measurement invariance in the
#' personnel selection context.
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs inlineCSS reset
#' @importFrom shinyWidgets materialSwitch
#' @importFrom shinyMatrix matrixInput
#' @return The output is an R Shinyapp with three pages
#'        \item{Instruction}{Provides direction for this Shinyapp}
#'        \item{Single dimension}{Implements the MCAA framework for 
#'        single dimensional constructs}
#'        \item{Multidimensions}{Implements the MCAA framework for 
#'        multidimensional constructs}
#'@examples
#' myApp()
#' launch()
#'@export
myApp <- function(...) {
  ui <- dashboardPage(
    dashboardHeader(title = "Selection Accuracy Indexes",
                    titleWidth = 400),
    dashboardSidebar(width = 150,
                     sidebarMenu(
                       menuItem(
                         "Instruction",
                         tabName = "Direction_page",
                         icon = icon("dashboard")
                       ),
                       menuItem(
                         "Single dimension",
                         tabName = "Single",
                         icon = icon("dashboard")
                       )
                     )),
    dashboardBody(
      #include for shinyjs features to work
      useShinyjs(),
      #inlineCSS using shinyjs to style margins and padding as well as size of box
      inlineCSS(
        ' #outputTableBox {overflow-y: auto; height: 350px}
          .box {margin:5px;}
          .col-sm-4 {padding:6px !important;}'
      ),
      #pages
      tabItems(
        #page one
        tabItem(tabName = "Direction_page",
                fluidPage(fluidRow(
                  box(
                    title = "Summary of the framework",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    p(
                      "The multidimensional classification accuracy analysis 
                      framework (MCAA) is an extension of the selection accuracy
                    framework proposed by Millsap & Kwok (2004). Since real-world 
                    selection usually involves multiple tests or subtests
                    with different weights assigned to each dimension, the MCAA 
                    framework is proposed to quantify the impact of item bias on 
                    selection accuracy by examining the changes in 
                    selection accuracy indices (proportion selected, success ratio, 
                    sensitivity, specificity) for each subgroup. The adverse impact 
                    (AI) ratio (i.e., the ratio of the proportions selected between 
                    a minority and a majority group with matching latent trait levels) 
                    is also provided in results. Further details are provided in 
                    Lai & Zhang (2022)."
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
                    p("Third, input unique factor variance-covariance matrix. 
                    You can choose to enter the diagonal of the covariance matrix, 
                    or upload the covariance matrix. Note the header should not be included. "
                    ), 
                    img(src="step3.png", align = "center",height='400px',width='270px'),
                    p("Then, input latent factor means and variances. Last, 
                      set selection parameters such as mixing proportion and selection proportion."
                    ), 
                    img(src="step4.png", align = "center",height='500px',width='270px'),
                    p("Lastly, set selection parameters such as mixing proportion 
                      and selection proportion."
                    ),
                    img(src="step5.png", align = "center",height='300px',width='270px'),
                    p("Here are the graph and the selection accuracy table"
                    ), 
                    img(src="graph.png", align = "left",height='300px',width='290px'),
                    img(src="table.png", align = "right",height='300px',width='290px'),
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
                    title = "Statement",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    p("This work was sponsored by the U.S. Army Research Institute 
                    for the Behavioral and Social Sciences (ARI) and was accomplished
                    under Grant #W911NF-20-1-0282. The views, opinions, and/or
                    findings contained in this paper are those of the authors 
                    and shall not be construed as an official Department of the 
                    Army position, policy, or decision, unless so designated by 
                    other documents."), 
                   ),

                  box(
                    title = "Note",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    p(
                      "If you have any questions, please email us at 
                      mmm.lab.usc@zohomail.com ."
                    ))
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
                  "This application is designed for the use of the 
                  Multidimensional Classification Accuracy Analysis (MCAA) Framework
                  for evaluating measurement invariance in the personnel 
                  selection context. By entering the parameter estimates from the 
                  factor model, such as the factor loadings and intercepts, 
                  you will be able to visualize the impact of item bias on 
                  selection accuracy indices and get a table that summarize 
                  the change of selection indices."
                ))
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
                             "use_mat_theta_f",
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
                             "mat_size_theta_f",
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
                         'Input the diagonal of the unique factor 
                         variance-covariance matrix \\( \\Theta \\) for the focal group',
                         placeholder = "0.72, 0.81, 0.32, 0.32"
                       ),
                       #strong is a text output in bold
                       strong(
                         id = "ins_mat_theta_r",
                         "Input the unique factor variance-covariance matrix 
                         \\( \\Theta \\) for the referance group"
                       ),
                       #uiOutput used for dynamic inputs, in this case matrices with variable size
                       #logic defined in renderUI in pageOneServer.R
                       uiOutput("ins_mat_theta_rUI"),
                       strong(
                         id = "ins_mat_theta_f",
                         "Input the unique factor variance-covariance matrix 
                         \\( \\Theta \\) for the focal group"
                       ),
                       uiOutput("ins_mat_theta_fUI"),
                       
                     ),
                     box(
                       title = "Mean and Variances",
                       status = "primary",
                       solidHeader = FALSE,
                       width = 12,
                       numericInput(
                         "kappa_r",
                         "Latent factor mean \\( \\kappa \\) for the reference group:",
                         value = 0.0,
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
                       materialSwitch("usepropsel", "Selection based on percentage?", 
                                      status = "primary", FALSE),
                       #numeric input for single number values
                       numericInput(
                         "cut_z",
                         "Cutoff score on the observed composite:",
                         value = NULL,
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
                       materialSwitch("tab_strict", "Show strict invariance output?", 
                                      status = "primary", FALSE),
                     ),
              ),
              box(
                id = "outputPlot",
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
                id = "outputTable",
                title = "Impact of Item Bias on Selection Accuracy Indices 
                for Partial Invariance",
                status = "primary",
                solidHeader = FALSE,
                width = 8,
                tableOutput("table")
              ),
              box(
                id = "outputTable_strict",
                title = "Impact of Item Bias on Selection Accuracy Indices 
                for Strict Invariance",
                status = "primary",
                solidHeader = FALSE,
                width = 8,
                tableOutput("tablestrict")
              )
            ))
        ))
      )
    )
  )
  
  server <- function(input, output) {
    
    #renderUI output for Matrix Inputs
    output$ins_mat_theta_fUI <- renderUI({
      #matrixInput for focal
      matrixInput(
        "ins_mat_theta_fInput",
        #use slider value to determine dimensions of matrix input
        value = matrix("0", input$mat_size_theta_f, input$mat_size_theta_f),
        rows = list(names = FALSE),
        cols = list(names = FALSE),
        class = "numeric",
      )
    })
    
    #renderUI output for Matrix Inputs
    output$ins_mat_theta_rUI <- renderUI({
      #matrixInput for referance
      matrixInput(
        inputId = "ins_mat_theta_rInput",
        value = matrix("0", input$mat_size_theta_f, input$mat_size_theta_f),
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
    observeEvent(list(input$usetheta_f, input$use_mat_theta_f), {
      #for every combination of the two buttons, show/hide the appropriate inputs
      if (input$use_mat_theta_f == FALSE & input$usetheta_f == TRUE) {
        shinyjs::show(id = "theta_f")
        shinyjs::show(id = "theta_r")
        shinyjs::hide(id = "ins_mat_theta_r")
        shinyjs::hide(id = "ins_mat_theta_rUI")
        shinyjs::hide(id = "ins_mat_theta_f")
        shinyjs::hide(id = "ins_mat_theta_fUI")
        shinyjs::hide(id = "mat_size_theta_f")
        
      }
      else if (input$use_mat_theta_f == FALSE & input$usetheta_f == FALSE) {
        shinyjs::hide(id = "theta_f")
        shinyjs::show(id = "theta_r")
        shinyjs::hide(id = "ins_mat_theta_r")
        shinyjs::hide(id = "ins_mat_theta_rUI")
        shinyjs::hide(id = "ins_mat_theta_f")
        shinyjs::hide(id = "ins_mat_theta_fUI")
        shinyjs::hide(id = "mat_size_theta_f")
      }
      else if (input$use_mat_theta_f == TRUE & input$usetheta_f == FALSE) {
        shinyjs::hide(id = "theta_f")
        shinyjs::hide(id = "theta_r")
        shinyjs::show(id = "ins_mat_theta_r")
        shinyjs::show(id = "ins_mat_theta_rUI")
        shinyjs::hide(id = "ins_mat_theta_f")
        shinyjs::hide(id = "ins_mat_theta_fUI")
        shinyjs::show(id = "mat_size_theta_f")
      }
      else{
        shinyjs::hide(id = "theta_f")
        shinyjs::hide(id = "theta_r")
        shinyjs::show(id = "ins_mat_theta_r")
        shinyjs::show(id = "ins_mat_theta_rUI")
        shinyjs::show(id = "ins_mat_theta_f")
        shinyjs::show(id = "ins_mat_theta_fUI")
        shinyjs::show(id = "mat_size_theta_f")
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
      reset("mat_size_theta_f")
      reset("usepropsel")
      reset("tab_strict")
      reset("uselambda_f")
      reset("usetau_f")
      reset("usetheta_f")
      reset("usekappa_f")
      reset("usephi_f")
      reset("use_mat_theta_f")
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
      if (input$usetheta_f == FALSE & input$use_mat_theta_f == FALSE) {
        theta_f = diag(theta_rNumeric())
      }
      else if (input$usetheta_f == TRUE & input$use_mat_theta_f == FALSE) {
        theta_f = diag(theta_fNumeric())
      }
      else if (input$usetheta_f == TRUE & input$use_mat_theta_f == TRUE) {
        theta_f = input$ins_mat_theta_fInput
      }
      else{
        theta_f = input$ins_mat_theta_rInput
      }
    })
    
    theta_r <- reactive({
      if (input$use_mat_theta_f == TRUE) {
        theta_r = input$ins_mat_theta_rInput
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
        if (input$use_mat_theta_f == FALSE && input$usetheta_f == TRUE) {
          need(
            length(theta_rNumeric()) == length(theta_fNumeric()),
            "(placeholder) diagonals of referance and focal groups must match"
          )
        },
        #only checks for numeric input of theta_r when matrix is not being used as input
        if (input$use_mat_theta_f == FALSE) {
          need(
            input$theta_r,
            "Input for unique variance-covariance matrix of reference group is missing\n"
          )
        },
        if (input$use_mat_theta_f == FALSE && length(theta_rNumeric()) > 0) {
          need(
            length(theta_rNumeric()) == length(lambda_rNumeric()),
            "number of inputs for measurement intercepts must match factor loadings"
          )
        },
        if (input$use_mat_theta_f == TRUE) {
          need(
            input$mat_size_theta_f == length(lambda_rNumeric()),
            "Matrix dimensions must match # loadings and intercepts\n"
          )
        },
        if (input$use_mat_theta_f == TRUE) {
          need(length(unique(theta_r())) / length(lambda_rNumeric())[1] != 1,
               "reference matrix empty")
        },
        if (input$use_mat_theta_f == TRUE && input$usetheta_f == TRUE) {
          need(length(unique(theta_f())) / length(lambda_rNumeric())[1] != 1,
               "focal matrix empty")
        },
        if (input$use_mat_theta_f == TRUE) {
          need(isSymmetric(input$ins_mat_theta_rInput) &&
                 is_symmetric_posdef(input$ins_mat_theta_rInput),
               "reference Theta matrix not positive definite")
        },
        if (input$use_mat_theta_f == TRUE && input$usetheta_f == TRUE) {
          need(isSymmetric(input$ins_mat_theta_fInput) &&
                 is_symmetric_posdef(input$ins_mat_theta_fInput),
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
      plot(partInvOutput(), which_result = "pi")
    })
    
    output$distPlotstrict <- renderPlot({
      validations()
      plot(partInvOutput(), which_result = "mi")
    })
    
    output$table <- renderTable(rownames = TRUE, {
      validations()
      partInvOutput()$summary
    })
    
    output$tablestrict <- renderTable(rownames = TRUE, {
      validations()
      partInvOutput()$summary_mi
    })
}
  shinyApp(ui, server = server, ...)
}

#' @rdname myApp
#' @export
launch <- myApp
