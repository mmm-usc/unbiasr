# validations <- function(input){
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
#     if (input$usetheta_f == TRUE) {
#       need(
#         length(theta_rNumeric()) == length(theta_fNumeric()),
#         "(placeholder) diagonals of referance and focal groups must match"
#       )
#     },
#     #only checks for numeric input of theta_r when matrix is not being used as input
#     if (input$useMatrix == FALSE) {
#       need(
#         input$theta_r,
#         "Input for unique variance-covariance matrix of reference group is missing\n"
#       )
#     },
#     if (input$useMatrix == FALSE && length(theta_rNumeric()) > 0) {
#       need(
#         length(theta_rNumeric()) == length(lambda_rNumeric()),
#         "number of inputs for measurement intercepts must match factor loadings"
#       )
#     },
#     if (input$useMatrix == TRUE) {
#       need(
#         input$matrixSlider == length(lambda_rNumeric()),
#         "Matrix dimensions must match # loadings and intercepts\n"
#       )
#     },
#     if (input$useMatrix == TRUE) {
#       need(length(unique(theta_r())) / length(lambda_rNumeric())[1] != 1,
#            "matrix empty")
#     }
#   )
# }
#   
