#
#
#
#
#
library(shiny)
#library(shinyMatrix)
library(shinyjs) #toggle hide
library(shinyWidgets) #using for a single button
library(shinydashboard) #boxes
library(bslib) #theme



#defining m as particular matrix
#m <- matrix(runif(4), 4, 1, dimnames = list(NULL, c("lambda_r")))

ui <- fluidPage(
    
    useShinyjs(),  # Include shinyjs (need for shinyjs)
    
    theme = bs_theme(version = 4, bootswatch = "minty"), #font and colors
    
    titlePanel("Computation of Selection Accuracy Indexes"),

    sidebarLayout(
        
        sidebarPanel(
            
            # matrixInput(
            #     "matrixID",
            #     
            #     #value is optional initial matrix
            #     value = m,
            #     
            #     rows = list(
            #         #this is giving me problems and thats a problem
            #         extend = TRUE
            #     ),
            #     
            #     cols = list(
            #         names = TRUE
            #     ),
            #     
            #     class = "numeric"
            # ),
            
            textInput('lambda_r', 'lambda_r: enter a vector of factor loadings for the reference group (comma delimited)', "0,1,2,3"),
            textInput('lambda_f', 'lambda_f: enter a vector of factor loadings for the focal group (comma delimited)', "0,1,2,3"),
            switchInput("uselambda_f", "Use lambda_f?", TRUE),
            textInput('tau_r', 'tau_r: enter a vector of measurement intercepts for the reference group (comma delimited)', "0,1,2,3"),
            textInput('tau_f', 'tau_f: enter a vector of measurement intercepts for the focal group (comma delimited)', "0,1,2,3"),
            switchInput("usetau_f", "Use tau_f?", TRUE),
            textInput('theta_r', 'theta_r: enter a matrix of the unique factor variances and covariances 
  #            for the reference group (comma delimited)', "0,1,2,3"),
            textInput('theta_f', 'theta_f: enter a matrix of the unique factor variances and covariances 
  #            for the focal group (comma delimited)', "0,1,2,3"),
            switchInput("usetheta_f", "Use theta_f?", TRUE),
            
            #plot contour TF
            switchInput("plot_contour", "Use plot contour?", TRUE),
            #use propsel
            switchInput("usepropsel", "Use propsel?", TRUE),
            #plot contour TF
            
            
            box(id = "cut_zBox", width = '800px',
                numericInput("cut_z", "cut_z: ", value = 0.5, min = 0, max = 1, step = 0.01)
            ),
            
            box(id = "propBox", width = '800px',
                numericInput("prop", "propsel:", value = 0.5, min = 0, max = 1, step = 0.01),
            ),
            
            numericInput("pmix", "pmix_ref:", value = 0.5, min = 0, max = 1, step = 0.01),
            numericInput("kappa_r", "kappa_r:", value = 0.5, min = 0, max = 1, step = 0.01),
            numericInput("kappa_f", "kappa_f:", value = 0.0, min = 0, max = 1, step = 0.01),
            switchInput("usekappa_f", "Use kappa_f?", TRUE),
            numericInput("phi_r", "phi_r:", value = 1., min = 0, max = 1, step = 0.01),
            numericInput("phi_f", "phi_f:", value = 1., min = 0, max = 1, step = 0.01),
            switchInput("usephi_f", "Use phi_f?", TRUE),
            
        ),

        mainPanel(
           plotOutput("distPlot"),
           verbatimTextOutput("text")
        )
    )
)

server <- function(input, output) {
    
    source('PartInv.R', local = TRUE)
    
    observeEvent(input$usepropsel, {
        if(input$usepropsel == TRUE){
            shinyjs::hide(id = "cut_zBox")
            shinyjs::show(id = "propBox")
        }else{
            shinyjs::show(id = "cut_zBox")
            shinyjs::hide(id = "propBox")
        }
    })

    output$distPlot <- renderPlot({
        
        lambda_rNumeric <- as.numeric(unlist(strsplit(input$lambda_r,",")))
        lambda_fNumeric <- as.numeric(unlist(strsplit(input$lambda_f,",")))
        tau_rNumeric <- as.numeric(unlist(strsplit(input$tau_r,",")))
        tau_fNumeric <- as.numeric(unlist(strsplit(input$tau_f,",")))
        theta_rNumeric <- as.numeric(unlist(strsplit(input$theta_r,",")))
        theta_fNumeric <- as.numeric(unlist(strsplit(input$theta_f,",")))
        
        if(input$usepropsel == FALSE){
          propsel = NULL
        }
        else{
          propsel = input$prop
        }
        
        #if(input$kappa_f == 0){
         # kappa_f = NULL
       # }
       # else{
       #   kappa_f = input$kappa_f
       # }
        
            PartInv(propsel, plot_contour = input$plot_contour, cut_z = input$cut_z, pmix_ref = input$pmix ,kappa_r = input$kappa_r, 
                    kappa_f = input$kappa_f, phi_r = input$phi_r,  phi_f = input$phi_f,
                    lambda_r = c(lambda_rNumeric), lambda_f = c(lambda_fNumeric), tau_r = c(tau_rNumeric), 
                    Theta_r = diag(theta_rNumeric))
        
       
    })
    
    output$text <- renderPrint({
         lambda_rNumeric <- as.numeric(unlist(strsplit(input$lambda_r,",")))
         lambda_fNumeric <- as.numeric(unlist(strsplit(input$lambda_f,",")))
         tau_rNumeric <- as.numeric(unlist(strsplit(input$tau_r,",")))
         tau_fNumeric <- as.numeric(unlist(strsplit(input$tau_f,",")))
         theta_rNumeric <- as.numeric(unlist(strsplit(input$theta_r,",")))
         theta_fNumeric <- as.numeric(unlist(strsplit(input$theta_f,",")))
         
         if(!input$usepropsel){
           propsel = NULL
         }
         else{
           propsel = input$prop
         }
      
        if(input$usepropsel){
            PartInv(propsel, cut_z = input$cut_z, pmix_ref = input$pmix ,kappa_r = input$kappa_r, 
                    kappa_f = input$kappa_f, phi_r = input$phi_r,  phi_f = input$phi_f,
                    lambda_r = c(lambda_rNumeric), tau_r = c(tau_rNumeric), 
                    Theta_r = diag(theta_rNumeric)) [[4]]
        }
        
        
        #for testing matrix print(input$matrixID);
        #for testing conditional print(input$somevalue);
        
        
    })
    
}

shinyApp(ui = ui, server = server)


