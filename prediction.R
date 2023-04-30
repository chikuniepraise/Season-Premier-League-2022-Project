library(dplyr)
library(caret)
# Load the saved model
loaded_model <- readRDS("att_def_model.rds")

# Create a new data frame with the features of the team you want to predict
new_data <- data.frame(
  Squad="Real Madrid",
  GF = 61,
  Def3rdPres = 1500,
  Att3rdPres = 2000,
  SaveRt = 69.5,
  TotSh = 580,
  TotShTg =185,
  TotTouc =23735,
  DefPenTouc=2447,
  Def3rdTouc=7398,
  Mid3rdTouc=11003,
  Att3rdTouc= 6845,
  AttPenTouc =1093, 
  KP =425,
  SCAT =921,
  GCAT =88,
  Def3rdPres=1398,
  Att3rdPres=1464
)

# Use the predict function to get the prediction
prediction <- predict(loaded_model, new_data)
certainty <- max(loaded_model$finalModel$fitted.values)
sentence <- paste(new_data$Squad,"is a/an", prediction[1], "team with", round(certainty * 100, 2), "% certainty.")
# Print the prediction
sentence
