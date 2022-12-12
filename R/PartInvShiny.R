partinvUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    # Input column (width 4/12 of the page)
    column(
      width = 4,
      box(
        title = "Inputs",
        status = "primary",
        solidHeader = FALSE,
        #all boxes in this column take up the full width of the column (12/12)
        width = 12,
        #HTML styling to increase bottom padding of reset button
        div(style = "padding-bottom: 10px;",
            #reset button
            actionButton("resetButton", label = "reset inputs")),
        #text inputs for graph labels
        textInput("legend_r",
                  label = "Input reference group label",
                  # default value
                  value = "Reference group"),
        textInput("legend_f",
                  label = "Input focal group label",
                  value = "Focal group")
      ),
      # Intercepts and loadings
      box(
        title = "Intercepts and Loadings",
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        textInput(
          ns("lambda_r"),
          label = "Reference group factor loadings \\( \\lambda \\)",
          placeholder = "1.00, 1.66, 2.30, 2.29"
        ),
        textInput(
          ns("lambda_f"),
          label = "Focal group factor loadings \\( \\lambda \\)",
          placeholder = "1.00, 1.66, 2.30, 2.29"
        ),
        # materialSwitch button using shinyWidgets
        materialSwitch(ns("uselambda_f"), label = "Focal group?",
                       status = "primary"),
        textInput(
          ns("tau_r"),
          label = "Reference group measurement intercepts \\( \\tau \\)",
          placeholder = "1.54, 1.36, 1.16, 1.08"
        ),
        textInput(
          ns("tau_f"),
          label = "Focal group measurement intercepts \\( \\tau \\)",
          placeholder = "0.68, 1.36, 1.16, 1.08"
        ),
        materialSwitch(ns("usetau_f"), label = "Focal group?",
                       status = "primary"),
      ),
      # Unique factor
      box(
        title = "Unique Factor Variance-Covariance",
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        # HTML style buttons side by side
        div(style = "display:inline-block, float:right",
            materialSwitch(
              ns("use_mat_theta_f"),
              "Matrix input?",
              status = "primary",
              inline = TRUE
            ),
            materialSwitch(
              ns("usetheta_f"),
              label = "Focal group?",
              status = "primary",
              inline = TRUE
            ),
            sliderInput(
              ns("mat_size_theta_f"),
              label = "Matrix Size",
              min = 2,
              max = 10,
              value = 4
            )
        ),
        textInput(
          ns("theta_r"),
          label = "Reference group uniqueness \\( \\theta \\)",
          placeholder = "1.20, 0.81, 0.32, 0.32"
        ),
        textInput(
          ns("theta_f"),
          label = "Focal group uniqueness \\( \\theta \\)",
          placeholder = "0.72, 0.81, 0.32, 0.32"
        ),
        # strong is a text output in bold
        strong(
          ns("ins_mat_theta_r"),
          "Input the unique factor variance-covariance matrix 
                         \\( \\Theta \\) for the referance group"
        ),
        #uiOutput used for dynamic inputs, in this case matrices with variable size
        #logic defined in renderUI in pageOneServer.R
        uiOutput(ns("ins_mat_theta_rUI")),
        strong(
          ns("ins_mat_theta_f"),
          "Input the unique factor variance-covariance matrix 
                         \\( \\Theta \\) for the focal group"
        ),
        uiOutput("ins_mat_theta_fUI")
      ),
      # Latent means and variances
      box(
        title = "Latent Mean and Variances",
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        numericInput(
          ns("kappa_r"),
          "Referece group latent factor mean \\( \\kappa \\)",
          value = 0.0,
          min = 0,
          max = 1,
          step = 0.01
        ),
        numericInput(
          ns("kappa_f"),
          "Focal group latent factor mean \\( \\kappa \\)",
          value = 0.0,
          min = 0,
          max = 1,
          step = 0.01
        ),
        materialSwitch(ns("usekappa_f"), label = "Focal group?",
                       status = "primary"),
        numericInput(
          ns("phi_r"),
          label = "Reference group latent factor variance \\( \\phi \\)",
          value = 1,
          min = 0,
          max = 1,
          step = 0.01
        ),
        numericInput(
          ns("phi_f"),
          label = "Focal group latent factor variance \\( \\phi \\)",
          value = 1,
          min = 0,
          max = 1,
          step = 0.01
        ),
        materialSwitch(ns("usephi_f"), label = "Focal group?",
                       status = "primary"),
      ),
      # Selection parameters
      box(
        title = "Cutoffs and Mixing Proportion",
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        materialSwitch(ns("usepropsel"),
                       label = "Selection based on percentage?", 
                       status = "primary"),
        numericInput(
          ns("cut_z"),
          label = "Cutoff score on the observed composite:",
          value = NULL,
          min = -100,
          max = 100,
          step = 0.01
        ),
        numericInput(
          ns("prop"),
          label = "Selection proportion:",
          value = 0.25,
          min = 0,
          max = 1,
          step = 0.01
        ),
        numericInput(
          ns("pmix"),
          label = "Mixing proportion (reference group; 0 to 1):",
          value = 0.5,
          min = 0,
          max = 1,
          step = 0.01
        ),
      ),
      box(
        title = "Output option",
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        checkboxInput(ns("tab_strict"),
                      label = "Show strict invariance"),
      ),
    ),
    
    # Output
    h3("Results Under Partial Invariance"),
    box(
      id = "outputPlot",
      title = "Relationship Between True Latent Construct Scores
               and Observed Test Scores",
      status = "primary",
      solidHeader = FALSE,
      width = 8,
      plotOutput(ns("distPlot"))
    ),
    box(
      id = "outputTable",
      title = "Impact of Item Bias on Selection Accuracy Indices 
                for Partial Invariance",
      status = "primary",
      solidHeader = FALSE,
      width = 8,
      tableOutput(ns("table"))
    ),
    h3("Results Under Strict Invariance"),
    box(
      id = "outputPlot_strict",
      title = "Relationship Between True Latent Construct Scores
               and Observed Test Scores for Strict Invariance",
      status = "primary",
      solidHeader = FALSE,
      width = 8,
      plotOutput(ns("distPlotstrict"))
    ),
    box(
      id = "outputTable_strict",
      title = "Impact of Item Bias on Selection Accuracy Indices 
                for Strict Invariance",
      status = "primary",
      solidHeader = FALSE,
      width = 8,
      tableOutput(ns("tablestrict"))
    )
  )
}

partinvServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      # renderUI output for Matrix Inputs
      output$ins_mat_theta_fUI <- renderUI({
        ns <- session$ns
        # matrixInput for focal
        matrixInput(
          ns("ins_mat_theta_fInput"),
          #use slider value to determine dimensions of matrix input
          value = matrix("0", nrow = input$mat_size_theta_f,
                         ncol = input$mat_size_theta_f),
          rows = list(names = FALSE),
          cols = list(names = FALSE),
          class = "numeric",
        )
      })
      
      #renderUI output for Matrix Inputs
      output$ins_mat_theta_rUI <- renderUI({
        ns <- session$ns
        # matrixInput for referance
        matrixInput(
          ns("ins_mat_theta_rInput"),
          value = matrix("0", nrow = input$mat_size_theta_f,
                         ncol = input$mat_size_theta_f),
          rows = list(names = FALSE),
          cols = list(names = FALSE),
          class = "numeric"
        )
      })
      
      # when a button press is observed for usepropsel
      observeEvent(input$usepropsel, {
        # this function evaluates this statement
        if (input$usepropsel == TRUE) {
          # and shows/hides the appropriate inputs
          shinyjs::hide("cut_z")
          shinyjs::show("prop")
        } else{
          shinyjs::show("cut_z")
          shinyjs::hide("prop")
        }
      })
      
      # when a button press is observed for uselambda_f
      observeEvent(input$uselambda_f, shinyjs::toggle("lambda_f"))
      observeEvent(input$usetau_f, shinyjs::toggle("tau_f"))
      observeEvent(input$usekappa_f, shinyjs::toggle("kappa_f"))
      observeEvent(input$usephi_f, shinyjs::toggle("phi_f"))
      
      
      #if either button press is observed
      observeEvent(list(input$usetheta_f, input$use_mat_theta_f), {
        #for every combination of the two buttons, show/hide the appropriate inputs
        if (input$use_mat_theta_f == FALSE & input$usetheta_f == TRUE) {
          shinyjs::show("theta_f")
          shinyjs::show("theta_r")
          shinyjs::hide("ins_mat_theta_r")
          shinyjs::hide("ins_mat_theta_rUI")
          shinyjs::hide("ins_mat_theta_f")
          shinyjs::hide("ins_mat_theta_fUI")
          shinyjs::hide("mat_size_theta_f")
        }
        else if (input$use_mat_theta_f == FALSE & input$usetheta_f == FALSE) {
          shinyjs::hide("theta_f")
          shinyjs::show("theta_r")
          shinyjs::hide("ins_mat_theta_r")
          shinyjs::hide("ins_mat_theta_rUI")
          shinyjs::hide("ins_mat_theta_f")
          shinyjs::hide("ins_mat_theta_fUI")
          shinyjs::hide("mat_size_theta_f")
        }
        else if (input$use_mat_theta_f == TRUE & input$usetheta_f == FALSE) {
          shinyjs::hide("theta_f")
          shinyjs::hide("theta_r")
          shinyjs::show("ins_mat_theta_r")
          shinyjs::show("ins_mat_theta_rUI")
          shinyjs::hide("ins_mat_theta_f")
          shinyjs::hide("ins_mat_theta_fUI")
          shinyjs::show("mat_size_theta_f")
        }
        else{
          shinyjs::hide("theta_f")
          shinyjs::hide("theta_r")
          shinyjs::show("ins_mat_theta_r")
          shinyjs::show("ins_mat_theta_rUI")
          shinyjs::show("ins_mat_theta_f")
          shinyjs::show("ins_mat_theta_fUI")
          shinyjs::show("mat_size_theta_f")
        }
      })
      
      observeEvent(input$tab_strict, {
        shinyjs::toggle("outputTable_strict")
        shinyjs::toggle("outputPlot_strict")
      })
      # 
      #if resetButton is pressed
      # observeEvent(input$resetButton, {
      #   #reset all
      #   reset("mat_size_theta_f")
      #   reset("usepropsel")
      #   reset("tab_strict")
      #   reset("uselambda_f")
      #   reset("usetau_f")
      #   reset("usetheta_f")
      #   reset("usekappa_f")
      #   reset("usephi_f")
      #   reset("use_mat_theta_f")
      #   reset("prop")
      #   reset("cut_z")
      #   reset("pmix")
      #   reset("lambda_f")
      #   reset("lambda_r")
      #   reset("tau_f")
      #   reset("tau_r")
      #   reset("theta_f")
      #   reset("theta_r")
      #   reset("kappa_f")
      #   reset("kappa_r")
      #   reset("phi_f")
      #   reset("phi_r")
      #   reset("legend_r")
      #   reset("legend_f")
      # })
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
      
      # set lambda_f to lambda_r input or lambda_f input depending on button press
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
      
      # validations <- reactive({
      #   validate(
      #     need(
      #       input$lambda_r,
      #       "Input for factor loadings of reference group is missing\n"
      #     ),
      #     need(
      #       input$tau_r,
      #       "Input for factor variance-covariance matrix of reference group is missing\n"
      #     ),
      #     need(
      #       length(lambda_rNumeric()) == length(tau_rNumeric()),
      #       "Factor loadings and intercepts need to have the same value\n"
      #     ),
      #     need(
      #       input$usepropsel || !is.na(input$cut_z),
      #       "Input for cutoff score is missing\n"
      #     ),
      #     if (input$uselambda_f == TRUE) {
      #       need(
      #         length(lambda_rNumeric()) == length(lambda_fNumeric()),
      #         "factor loadings for referance and focal groups need to have the same number of values\n"
      #       )
      #     },
      #     if (input$usetau_f == TRUE) {
      #       need(
      #         length(tau_rNumeric()) == length(tau_fNumeric()),
      #         "intercepts for referance and focal groups need to have the same number of values\n"
      #       )
      #     },
      #     if (input$use_mat_theta_f == FALSE && input$usetheta_f == TRUE) {
      #       need(
      #         length(theta_rNumeric()) == length(theta_fNumeric()),
      #         "(placeholder) diagonals of referance and focal groups must match"
      #       )
      #     },
      #     #only checks for numeric input of theta_r when matrix is not being used as input
      #     if (input$use_mat_theta_f == FALSE) {
      #       need(
      #         input$theta_r,
      #         "Input for unique variance-covariance matrix of reference group is missing\n"
      #       )
      #     },
      #     if (input$use_mat_theta_f == FALSE && length(theta_rNumeric()) > 0) {
      #       need(
      #         length(theta_rNumeric()) == length(lambda_rNumeric()),
      #         "number of inputs for measurement intercepts must match factor loadings"
      #       )
      #     },
      #     if (input$use_mat_theta_f == TRUE) {
      #       need(
      #         input$mat_size_theta_f == length(lambda_rNumeric()),
      #         "Matrix dimensions must match # loadings and intercepts\n"
      #       )
      #     },
      #     if (input$use_mat_theta_f == TRUE) {
      #       need(length(unique(theta_r())) / length(lambda_rNumeric())[1] != 1,
      #            "reference matrix empty")
      #     },
      #     if (input$use_mat_theta_f == TRUE && input$usetheta_f == TRUE) {
      #       need(length(unique(theta_f())) / length(lambda_rNumeric())[1] != 1,
      #            "focal matrix empty")
      #     },
      #     if (input$use_mat_theta_f == TRUE) {
      #       need(isSymmetric(input$ins_mat_theta_rInput) &&
      #              is_symmetric_posdef(input$ins_mat_theta_rInput),
      #            "reference Theta matrix not positive definite")
      #     },
      #     if (input$use_mat_theta_f == TRUE && input$usetheta_f == TRUE) {
      #       need(isSymmetric(input$ins_mat_theta_fInput) &&
      #              is_symmetric_posdef(input$ins_mat_theta_fInput),
      #            "focal Theta matrix not positive definite")
      #     }
      #     
      #   )})
      
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
        # validations()
        plot(partInvOutput(), which_result = "pi")
      })
      
      output$distPlotstrict <- renderPlot({
        # validations()
        plot(partInvOutput(), which_result = "mi")
      })
      
      output$table <- renderTable(rownames = TRUE, {
        # validations()
        partInvOutput()$summary
      })
      
      output$tablestrict <- renderTable(rownames = TRUE, {
        # validations()
        partInvOutput()$summary_mi
      })
    }
  )
}

#' Launching Partial Invariance Evaluation Shinyapp
#'
#' \code{launch, myApp} launch the Shinyapp designed for the use of the
#' Multidimensional Classification Accuracy Analysis
#' (MCAA) Framework for evaluating measurement invariance in the
#' personnel selection context.
#' 
#' @details The app has three pages
#' * Instruction: Provides direction for this Shinyapp
#' * Single dimension: Implements the CAA framework for
#'     single dimensional constructs
#' * Multiple dimensions: Implements the MCAA framework for
#'     multidimensional constructs
#'
#' @param ... Currently not used.
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs inlineCSS reset
#' @importFrom shinyWidgets materialSwitch
#' @importFrom shinyMatrix matrixInput
#'
#' @examples
#' \dontrun{
#' myApp()
#' launch()
#' }
#'@export
myApp <- function(...) {
  shiny::addResourcePath("images",
                         system.file("www", "images",
                                     package = "unbiasr"))
  # Text summary
  summ_text <- "
  The multidimensional classification accuracy analysis 
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
  ui <- dashboardPage(
    dashboardHeader(title = "unbiasr",
                    titleWidth = 400),
    dashboardSidebar(width = 150,
                     sidebarMenu(
                       menuItem(
                         "Single dimension",
                         tabName = "Single",
                         icon = icon("chart-simple")
                       ),
                       menuItem(
                         "Instruction",
                         tabName = "Direction_page",
                         icon = icon("circle-info")
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
      # pages
      tabItems(
        # page one
        tabItem(tabName = "Direction_page",
                fluidPage(fluidRow(
                  box(
                    title = "Summary of the framework",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    p(summ_text)),
                  box(
                    title = "Example",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    p(
                      "Here is an example for how to use the app."),
                    p("The first step is to input group labels for the graph."
                    ),
                    img(src="images/step1.png", align = "center",height='300px',width='270px'),
                    p("Next, input factor loadings and intercepts
                       for each group."
                    ), 
                    img(src="images/step2.png", align = "center",height='400px',width='270px'),
                    p("Third, input unique factor variance-covariance matrix. 
                    You can choose to enter the diagonal of the covariance matrix, 
                    or upload the covariance matrix. Note the header should not be included. "
                    ), 
                    img(src="images/step3.png", align = "center",height='400px',width='270px'),
                    p("Then, input latent factor means and variances. Last, 
                      set selection parameters such as mixing proportion and selection proportion."
                    ), 
                    img(src="images/step4.png", align = "center",height='500px',width='270px'),
                    p("Lastly, set selection parameters such as mixing proportion 
                      and selection proportion."
                    ),
                    img(src="images/step5.png", align = "center",height='300px',width='270px'),
                    p("Here are the graph and the selection accuracy table"
                    ), 
                    img(src="images/graph.png", align = "left",height='300px',width='290px'),
                    img(src="images/table.png", align = "right",height='300px',width='290px'),
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
        # second page
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
            # second row which contains input and output columns
            partinvUI("pinv1")
          )
        ))
      )
    )
  )
  
  server <- function(input, output, session) {
    partinvServer("pinv1")
  }
  shinyApp(ui, server = server, ...)
}

#' @rdname myApp
#' @export
launch <- myApp
