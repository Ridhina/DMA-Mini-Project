data <- read.csv("counterfeit_products.csv") 
# Split dataset into training (80%) and testing (20%) 
set.seed(123) 
train_index <- sample(1:nrow(data), 0.8 * nrow(data)) 
train_data <- data[train_index, ] 
test_data <- data[-train_index, ] 
 
# Random Forest Model 
rf_model <- randomForest(Label ~ Price + Feature_Mismatch + Seller_Rating,  
                         data = train_data,  
                         ntree = 100) 
 
# SVM Model 
svm_model <- svm(Label ~ Price + Feature_Mismatch + Seller_Rating,  
                 data = train_data,  
                 kernel = "radial") 
 
# Predict and evaluate Random Forest 
rf_pred <- predict(rf_model, test_data) 
rf_accuracy <- sum(rf_pred == test_data$Label) / nrow(test_data) 
 
# Predict and evaluate SVM 
svm_pred <- predict(svm_model, test_data) 
svm_accuracy <- sum(svm_pred == test_data$Label) / nrow(test_data) 
 
print(paste("Random Forest Accuracy:", round(rf_accuracy*100,2), "%")) 
print(paste("SVM Accuracy:", round(svm_accuracy*100,2), "%")) 
 
 
# Shiny Dashboard 
ui <- fluidPage( 
  titlePanel("Counterfeit Product Detector"), 
  sidebarLayout( 
    sidebarPanel( 
      numericInput("price", "Price:", value = 1000, min = 0), 
      numericInput("feature", "Feature Mismatch Score:", value = 0, min = 0), 
      numericInput("rating", "Seller Rating:", value = 5, min = 0, max = 5), 
      actionButton("predict", "Predict") 
    ), 
    mainPanel( 
      textOutput("result") 
    ) 
  ) 
) 
 
server <- function(input, output) { 
  observeEvent(input$predict, { 
    new_data <- data.frame( 
      Price = input$price, 
      Feature_Mismatch = input$feature, 
      Seller_Rating = input$rating 
    ) 
    pred <- predict(rf_model, new_data) 
 
output$result <- renderText({ 
if(pred == "Genuine"){ 
"Result: Likely Genuine " 
} else { 
"Result: Potentially Fake " 
} 
}) 
}) 
} 
shinyApp(ui = ui, server = server)
