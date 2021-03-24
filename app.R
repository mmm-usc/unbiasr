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
    
    titlePanel("Impact of Item biases on Selection Accuracy Indices"),

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
            textInput('lambda_r', 'Input factor loadings for the reference group', "1.00, 1.66, 2.30, 2.29"),
            textInput('lambda_f', 'Input factor loadings for the focal group', "1.00, 1.66, 2.30, 2.29"),
            textInput('tau_r', 'Input measurement intercepts for the reference group', "1.54, 1.36, 1.16, 1.08"),
            textInput('tau_f', 'Input measurement intercepts for the focal group', "0.68, 1.36, 1.16, 1.08"),
            textInput('theta_r', 'Input the diagonal of the unique factor variance-covariance matrix for the reference group', "1.20, 0.81, 0.32, 0.32"),
            textInput('theta_f', 'Input the diagonal of the unique factor variance-covariance matrix for the focal group', "0.72, 0.81, 0.32, 0.32"),
            
            #might need some javascript to limit the range
            switchInput("usepropsel", "Select 10% population?", TRUE),
            
            box(id = "cut_zBox", width = '800px',
                numericInput("cut_z", "Cutoff score on the observed composite:", value = 0.5, min = 0, max = 1, step = 0.01)
            ),
            
            box(id = "propBox", width = '800px',
                numericInput("prop", "Selection proportion:", value = 0.1, min = 0, max = 1, step = 0.01),
            ),
            
            numericInput("pmix", "Mixing proportion:", value = 0.5, min = 0, max = 1, step = 0.01),
            numericInput("kappa_r", "Latent factor mean (reference):", value = 0.0, min = 0, max = 1, step = 0.01),
            numericInput("kappa_f", "Latent factor mean (focal):", value = 0.0, min = 0, max = 1, step = 0.01),
            numericInput("phi_r", "Latent factor variance (reference):", value =  0.354^2., min = 0, max = 1, step = 0.01),
            numericInput("phi_f", "Latent factor variance (focal):", value =  0.354^2., min = 0, max = 1, step = 0.01),
            
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
        
        if(input$usepropsel){
          PartInv(propsel = input$prop, pmix_ref = input$pmix ,kappa_r = input$kappa_r, 
                  kappa_f = input$kappa_f, phi_r = input$phi_r,  phi_f = input$phi_f,
                  lambda_r = c(lambda_rNumeric),lambda_f = c(lambda_fNumeric), tau_r = c(tau_rNumeric), 
                  tau_f = c(tau_fNumeric),
                  Theta_r = diag(theta_rNumeric),Theta_f = diag(theta_rNumeric))
        }
        else{
            PartInv(cut_z = input$cut_z , pmix_ref = input$pmix ,kappa_r = input$kappa_r, 
                    kappa_f = input$kappa_f, phi_r = input$phi_r,  phi_f = input$phi_f,
                    lambda_r = c(lambda_rNumeric),lambda_f = c(lambda_fNumeric), tau_r = c(tau_rNumeric), 
                    tau_f = c(tau_fNumeric),
                    Theta_r = diag(theta_rNumeric),Theta_f = diag(theta_rNumeric))
        }
       
        
        
    })
    
    output$text <- renderPrint({
        
        lambda_rNumeric <- as.numeric(unlist(strsplit(input$lambda_r,",")))
        lambda_fNumeric <- as.numeric(unlist(strsplit(input$lambda_f,",")))
        tau_rNumeric <- as.numeric(unlist(strsplit(input$tau_r,",")))
        tau_fNumeric <- as.numeric(unlist(strsplit(input$tau_f,",")))
        theta_rNumeric <- as.numeric(unlist(strsplit(input$theta_r,",")))
        theta_fNumeric <- as.numeric(unlist(strsplit(input$theta_f,",")))
        
        if(input$usepropsel){
            PartInv(propsel = input$prop, pmix_ref = input$pmix ,kappa_r = input$kappa_r, 
                    kappa_f = input$kappa_f, phi_r = input$phi_r,  phi_f = input$phi_f,
                    lambda_r = c(lambda_rNumeric),lambda_f = c(lambda_fNumeric), tau_r = c(tau_rNumeric), 
                    tau_f = c(tau_fNumeric),
                    Theta_r = diag(theta_rNumeric),Theta_f = diag(theta_rNumeric)) [[4]]
        }
        else{
            PartInv(cut_z = input$cut_z , pmix_ref = input$pmix ,kappa_r = input$kappa_r, 
                    kappa_f = input$kappa_f, phi_r = input$phi_r,  phi_f = input$phi_f,
                    lambda_r = c(lambda_rNumeric),lambda_f = c(lambda_fNumeric), tau_r = c(tau_rNumeric), 
                    tau_f = c(tau_fNumeric),
                    Theta_r = diag(theta_rNumeric),Theta_f = diag(theta_rNumeric))[[4]]
        }
        
        
        #for testing matrix print(input$matrixID);
        #for testing conditional print(input$somevalue);
        
        
    })
    
}

shinyApp(ui = ui, server = server)
