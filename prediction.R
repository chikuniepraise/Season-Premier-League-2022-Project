library(dplyr)
library(caret)
# Load the saved model
loaded_model <- readRDS("att_def_model.rds")

# Create a new data frame with the features of the team you want to predict
new_data <- data.frame(
  Squad="Real Madrid",
  Def3rdPres = 1500,
  Att3rdPres = 2000,
  SaveRt = 70
)

# Use the predict function to get the prediction
prediction <- predict(loaded_model, new_data)
certainty <- max(loaded_model$finalModel$fitted.values)
sentence <- paste(new_data$Squad,"is a/an", prediction[1], "team with", round(certainty * 100, 2), "% certainty.")
# Print the prediction
sentence
