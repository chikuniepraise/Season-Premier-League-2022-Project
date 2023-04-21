# Loading the necessary libraries
library(dplyr)
library(class)
library(cluster)
library(ggplot2)
library(readr)
library(stringr)
library(caret)
library(Hmisc)
library(tidyselect)
library(knitr)

#loading the datasets
table <- read.csv("england_premier_league_table_22.csv", sep = ";")
goal_keeping <- read.csv("england_premier_league_squad_goalkeeping_22.csv", sep = ";")
defensive_actions <- read.csv("england_premier_league_squad_defensive_actions_22.csv", sep = ";")

# data Cleaning. 
table_ <- select(table, c(1,2,3,4,5,6))
goal_keeping_ <- select(goal_keeping, c(1,8))
defensive_actions_ <- select(defensive_actions, c(1,15,17))

#merging the datasets.
League <- merge(table_, goal_keeping_, by="Squad")
League <- merge(League, defensive_actions_, by= "Squad")

League
#Analyzing the dataset.
# 1. Finding the most offensive and defensive team
# z. The teams with most wins and loses
# 3. What does qualified teams have in common
# 4. The effectiveness of the goalkeeper in the offensive team
# 5. visualization in clusters.



row_labels <- League[,1]

#removing the squad column
League = select(League, c(2:9))
League

#scaling the data 
League = scale(League)

#Spilting the dataset 80/20

set.seed(123)
size <- floor(0.8 * nrow(League))

train_ind <- sample(seq_len(nrow(League)), size=size)
train_labels <- League[train_ind, 1]
test_labels = row_labels[-train_ind]

#K-nearest neighbor for the offensive team
data_train <- League[train_ind,1:7]
data_test <- League[-train_ind, 1:7]



