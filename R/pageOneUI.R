#ui components for page one
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
          .col-sm-4 {padding:6px !important;}
          .col-sm-8 {padding:6px !important;}'
    ),
    #pages
    tabItems(
      #page one
      tabItem(tabName = "firstpage", fluidPage(
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
                   ),
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
          # ,
          # downloadButton("downloadData", "Download")
        )
        
      )),
      #second page
      tabItem(tabName = "other", fluidPage(fluidRow()))
    )
  )
)
