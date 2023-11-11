#===========================================================================================#
# Module: ICT619 - Artificial Intelligence
# Name: Muhammad Sulianto Bin Sulaiman, Oo Hteik Tin
# Murdoch Student ID: 32817608, 34673851
# Project: Predictive Analytics for Student Performance
# Date: 15th November 2023
#==========================================================================================#

# Import library dependencies #
#=============================#
#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("randomForest")
#install.packages("caret")
library(shiny)
library(tidyverse)
library(randomForest)
library(caret)

# Set the working directory (Note: Set the working directory according to own preferences) #
#==========================================================================================#
#setwd("C:/Users/Sulianto/Documents/Sulianto Folder/School/Masters Degree/TSA2023/Artificial Intelligence/Project")

# Load the student dataset #
#==========================#
stud_perf <- read.csv("StudentsPerformance.csv")

# View dataset structure #
#========================#
glimpse(stud_perf)
str(stud_perf)
summary(stud_perf)

# Data Cleansing 
#==============#

# Check for missing NA values
#===========================#
sum(is.na(stud_perf))

# Encode categorical variables (Gender, Race Ethnicity, Parental level of education) into labels #
#==========================================#
stud_perf$gender <- as.factor(stud_perf$gender)
stud_perf$race.ethnicity <- as.factor(stud_perf$race.ethnicity)
stud_perf$parental.level.of.education <- as.factor(stud_perf$parental.level.of.education)

# Create a column for overall score for Maths, Reading and Writing subjects #
#===========================================================================#
stud_perf$overall.score <- rowMeans(select(stud_perf, math.score, reading.score, writing.score))

# Round off the overall score column into 2 decimal places #
#==========================================================#
stud_perf$overall.score <- round(stud_perf$overall.score, digits = 2)

# View dataset structure #
#========================#
glimpse(stud_perf)
str(stud_perf)
summary(stud_perf)

# Split the data into training and testing sets #
#===============================================#
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(stud_perf), 0.7 * nrow(stud_perf))
train_data <- stud_perf[train_index, ]
test_data <- stud_perf[-train_index, ]

# Define cross-validation settings #
#==================================#
ctrl <- trainControl(
  method = "cv",        # Cross-validation method
  number = 5,           # Number of folds
  verboseIter = TRUE    # Display progress
)

# Fit the Random Forest model using cross-validation #
#===================================================#
model <- train(
  overall.score ~ gender + race.ethnicity + parental.level.of.education, 
  data = train_data,
  method = "rf",
  trControl = ctrl
)

# Make predictions on the test data #
#==================================#
predictions <- predict(model, test_data)

# Calculate Mean Squared Error (MSE) #
#===================================#
mse <- mean((test_data$overall.score - predictions)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Create a histogram of overall scores #
#======================================#
hist(test_data$overall.score, main = "Overall Score Distribution",
     xlab = "Overall Score", col = "blue", border = "black")

# Define UI #
#===========#
ui <- fluidPage(
  titlePanel("Student Performance Predictor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Select Gender:", choices = unique(stud_perf$gender)),
      selectInput("race_ethnicity", "Select Race/Ethnicity:", choices = unique(stud_perf$race.ethnicity)),
      selectInput("parent_education", "Select Parental Education Level:", choices = unique(stud_perf$parental.level.of.education)),
      actionButton("predict_button", "Predict Score")
    ),
    mainPanel(
      textOutput("predicted_score"),
      plotOutput("score_distribution")
      
    )
  )
)

# Define Server #
#==============#
server <- function(input, output) {
  observeEvent(input$predict_button, {
    # Get user inputs
    user_data <- data.frame(
      gender = factor(input$gender, levels = levels(train_data$gender)),
      race.ethnicity = factor(input$race_ethnicity, levels = levels(train_data$race.ethnicity)),
      parental.level.of.education = factor(input$parent_education, levels = levels(train_data$parental.level.of.education))
    )
    
    # Make predictions using the Random Forest model
    prediction <- predict(model, user_data)
    
    # Display the prediction
    output$predicted_score <- renderText({
      paste("Predicted Score: ", round(prediction, 2))
    })
    
    # Create a histogram of overall scores
    output$score_distribution <- renderPlot({
      hist(test_data$overall.score, main = "Overall Score Distribution",
           xlab = "Overall Score", col = "blue", border = "black")
      abline(v = prediction, col = "red", lty = 2, lwd = 2)
    })
  })
}

# Run the Shiny app #
#===================#
shinyApp(ui = ui, server = server)


