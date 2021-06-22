#ui components for page one
list(
     #withMathJax() for greek char display
       withMathJax(), 
       fluidRow(
         box(
           title = "Directions",
           status = "primary",
           width = 12,
           solidHeader = FALSE,
           p(
             "This application is designed for the use of the Multidimensional Classification Accuracy Analysis (MCAA) Framework 
                      for evaluating measurement invariance in the personnel selection context. By entering the parameter estimates from the factor model,
                      such as the factor loadings and intercepts, you will be able to visualize the impact of item bias on selection accuracy indices and 
                      get a table that summarize the change of selection indices."
           )
         ),
         column(width = 4,
                box(
                  title = "inputs",
                  status = "primary",
                  solidHeader = FALSE,
                  width = 12,
                  #style for alignment of reset button
                  div(style = "display:inline-block;margin-right: 52%;padding-bottom: 10px;",
                      actionButton("resetButton", "reset inputs")),
                  textInput('legend_r',
                            'Input reference group label',
                            value = "Reference group"),
                  textInput('legend_f',
                            'Input label for the focal group',
                            value = "Focal group")
                ),
                box(
                  title = "intercepts and loadings",
                  status = "primary",
                  solidHeader = FALSE,
                  width = 12,
                  #textInput for multi-value inputs
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
                  title = "unique factor variance-covariance",
                  status = "primary",
                  solidHeader = FALSE,
                  width = 12,
                  
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
                    ),
                    sliderInput("matrixSlider", "Matrix Size",
                                min = 2, max = 5, value = 4
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
                  #uiOutput used for dynamic inputs
                  #Outputs matrix rows & cols based on number of values in lambda_r
                  #logic defined in renderUI
                  uiOutput("theta_rMatrixUI"),
                  strong(
                    id = "theta_fMatrixTitle",
                    "input the unique factor variance-covariance matrix \\( \\Theta \\) for the focal group"
                  ),
                  uiOutput("theta_fMatrixUI"),
                  
                ),
                
                
                
                box(
                  title = "mean and variances",
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
                  title = "cutoffs and mixing proportion",
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
     )
