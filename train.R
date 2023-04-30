
library(lattice)
library(caret)
library(e1071)

#loading the datasets
table <- read.csv("england_premier_league_table_22.csv", sep = ";")
goal_keeping <- read.csv("england_premier_league_squad_goalkeeping_22.csv", sep = ";")
defensive_actions <- read.csv("england_premier_league_squad_defensive_actions_22.csv", sep = ";")
squad_shooting <- read.csv("england_premier_league_squad_shooting_22.csv", sep = ";")
possession <- read.csv("england_premier_league_squad_possession_22.csv", sep = ";")
passing <- read.csv("england_premier_league_squad_passing_stats_22.csv", sep = ";")
goal <- read.csv("england_premier_league_squad_goal_shot_creation_22.csv", sep = ";")

# data Cleaning. 
table_ <- select(table, c(1,2,4,5,6,7))
goal_keeping_ <- select(goal_keeping, c(1,8))
squad_shooting_ <- select(squad_shooting, c(1,5,6))
possession_ <- select(possession, c(1,4,5,6,7,8,9))
passing_ <- select(passing, c(1,21))
goal_ <- select(goal, c(1,3,11))
defensive_actions_ <- select(defensive_actions, c(1,15,17))

#merging the datasets.
squad_data <- merge(table_, goal_keeping_, by="Squad")
squad_data <- merge(squad_data, squad_shooting_, by="Squad")
squad_data <- merge(squad_data, possession_, by="Squad")
squad_data <- merge(squad_data, passing_, by="Squad")
squad_data <- merge(squad_data, goal_, by="Squad")
squad_data <- merge(squad_data, defensive_actions_, by= "Squad")
squad_data

# Create a new variable "Style" to label teams as attacking or defensive based on their Def3rdPres and Att3rdPres values
squad_data$Style <- ifelse(squad_data$Att3rdPres > squad_data$Def3rdPres, "Attacking", "Defensive")

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(squad_data$Style, p = 0.7, list = FALSE)
train_data <- squad_data[train_index, ]
test_data <- squad_data[-train_index, ]

# Train logistic regression model
logit_model <- train(Style ~ GF + Att3rdPres + Def3rdPres + SaveRt+TotSh + TotShTg + DefPenTouc + Def3rdTouc + Mid3rdTouc + Att3rdTouc + AttPenTouc+KP +SCAT +GCAT+Def3rdPres+Att3rdPres, data = train_data, method = "glm", family = "binomial")

# Predict on test data
pred <- predict(logit_model, newdata = test_data)

# Evaluate model accuracy
confusionMatrix(pred, test_data$Style)




file_path <- "att_def_model.rds"

if (file.exists(file_path)) {
  # Load the previously stored model from the file
  old_model <- readRDS(file_path)
  
  # Compare the accuracy of the new model with the old one
  old_accuracy <- mean(predict(old_model, squad_data) == squad_data$Style)
  new_accuracy <- mean(predict(logit_model, squad_data) == squad_data$Style)
  
  # If the new model has higher accuracy, store it in the file
  if (new_accuracy > old_accuracy) {
    saveRDS(logit_model, file_path)
  }
}else{
  saveRDS(logit_model, file_path)
}

