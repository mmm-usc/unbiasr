library(shiny)
library(shinydashboard) #UI
library(shinyjs) #toggle hide, reset
library(shinyWidgets) #using for a single button

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
                                'lambda_r: enter a vector of factor loadings for the reference group (comma delimited)',
                                "0,1,2,3"
                              ),
                              textInput(
                                'lambda_f',
                                'lambda_f: enter a vector of factor loadings for the focal group (comma delimited)',
                                "0,1,2,3"
                              ),
                              switchInput("uselambda_f", "Include lambda_f?", FALSE),
                              textInput(
                                'tau_r',
                                'tau_r: enter a vector of measurement intercepts for the reference group (comma delimited)',
                                "0,1,2,3"
                              ),
                              textInput(
                                'tau_f',
                                'tau_f: enter a vector of measurement intercepts for the focal group (comma delimited)',
                                "0,1,2,3"
                              ),
                              switchInput("usetau_f", "Include tau_f?", FALSE),
                              textInput(
                                'theta_r',
                                'theta_r: enter a matrix of the unique factor variances and covariances
  #            for the reference group (comma delimited)',
                                "0,1,2,3"
                              ),
                              textInput(
                                'theta_f',
                                'theta_f: enter a matrix of the unique factor variances and covariances
  #            for the focal group (comma delimited)',
                                "0,1,2,3"
                              ),
                              switchInput("usetheta_f", "Include theta_f?", FALSE)
                            ),
                            box(
                              #use propsel
                              switchInput("usepropsel", "Use propsel?", FALSE),
                              #plot contour TF
                              numericInput(
                                "cut_z",
                                "cut_z: ",
                                value = 0.5,
                                min = 0,
                                max = 1,
                                step = 0.01
                              ),
                              numericInput(
                                "prop",
                                "propsel:",
                                value = 0.5,
                                min = 0,
                                max = 1,
                                step = 0.01
                              ),
                              numericInput(
                                "pmix",
                                "pmix_ref:",
                                value = 0.5,
                                min = 0,
                                max = 1,
                                step = 0.01
                              ),
                              numericInput(
                                "kappa_r",
                                "kappa_r:",
                                value = 0.5,
                                min = 0,
                                max = 1,
                                step = 0.01
                              ),
                              numericInput(
                                "kappa_f",
                                "kappa_f:",
                                value = 0.0,
                                min = 0,
                                max = 1,
                                step = 0.01
                              ),
                              switchInput("usekappa_f", "Include kappa_f?", FALSE),
                              numericInput(
                                "phi_r",
                                "phi_r:",
                                value = 1.,
                                min = 0,
                                max = 1,
                                step = 0.01
                              ),
                              numericInput(
                                "phi_f",
                                "phi_f:",
                                value = 1.,
                                min = 0,
                                max = 1,
                                step = 0.01
                              ),
                              switchInput("usephi_f", "Include phi_f?", FALSE)
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
        tau_f = tau_f(),
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
