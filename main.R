# Loading the necessary libraries
library(dplyr)
library(class)
library(ggplot2)
library(readr)
library(tidyselect)
library(psych)
library(cluster)
library(factoextra)

#loading the datasets
table <- read.csv("england_premier_league_table_22.csv", sep = ";")
goal_keeping <- read.csv("england_premier_league_squad_goalkeeping_22.csv", sep = ";")
defensive_actions <- read.csv("england_premier_league_squad_defensive_actions_22.csv", sep = ";")

# data Cleaning. 
table_ <- select(table, c(1,2,4,5,6))
goal_keeping_ <- select(goal_keeping, c(1,8))
defensive_actions_ <- select(defensive_actions, c(1,15,17))

#merging the datasets.
squad_data <- merge(table_, goal_keeping_, by="Squad")
squad_data <- merge(squad_data, defensive_actions_, by= "Squad")
squad_data
#Analyzing the dataset.
# 1. Finding the most offensive and defensive team
# z. The teams with most wins and loses
# 3. What does qualified teams have in common
# 4. The effectiveness of the goalkeeper in the offensive team
# 5. visualization in clusters.




# 1. Finding the most offensive and defensive team
# 1. Finding the most offensive and defensive team
# 1. Finding the most offensive and defensive team
# 1. Finding the most offensive and defensive team
team_stats <- data.frame(Rank=squad_data$Rk,
                         Team = squad_data$Squad, 
                         Att3rdPres = squad_data$Att3rdPres, 
                         Def3rdPres = squad_data$Def3rdPres)

# Sort the data frame by Att3rdPres and Def3rdPres values
offensive_team <- team_stats[order(-team_stats$Att3rdPres),]
defensive_team <- team_stats[order(-team_stats$Def3rdPres),]

# The top offensive and defensive teams
cat("Top most offensive teams:\n")
print(head(offensive_team, 1))
cat("\nTop most defensive teams:\n")
print(head(defensive_team, 1))




# z. The team with most wins and loses
# z. The team with most wins and loses
# z. The team with most wins and loses
# z. The team with most wins and loses

sorted_data <- squad_data[order(squad_data$W, decreasing = TRUE),]
sorted_data_losses <- squad_data[order(squad_data$L, decreasing = TRUE),]

# Extract top team with most wins and losses
top_wins <- head(sorted_data, 1)
top_losses <- head(sorted_data_losses, 1)

# Print the results
cat("Team with the most wins:\n")
top_wins

cat("Teams with the most losses:\n")
top_losses





# 3. What does qualified teams have in common
# 3. What does qualified teams have in common
# 3. What does qualified teams have in common
# 3. What does qualified teams have in common
qualified_teams <- squad_data[squad_data$W + squad_data$D + squad_data$L == 38, ]
qualified_stats <- apply(qualified_teams[,3:8], 2, function(x) c(mean = mean(x), sd = sd(x)))
qualified_stats

cat("From this, we can see that on average, the qualified teams won 14.6 games, drew 8.8 games, and lost 14.6 games. Their goalkeepers had an average save rate of 69.23%, and their average attacking and defensive pressures were 1880.6 and 1322.9, respectively. We can also see the standard deviation of each metric, which gives us an idea of how much variation there is among the qualified teams in each category.")


# 4. The effectiveness of the goalkeeper in the offensive team
# 4. The effectiveness of the goalkeeper in the offensive team
# 4. The effectiveness of the goalkeeper in the offensive team
# 4. The effectiveness of the goalkeeper in the offensive team



correlation_ <- cor(squad_data$SaveRt, squad_data$Att3rdPres)
plot(squad_data$SaveRt, squad_data$Att3rdPres, xlab = "SaveRt", ylab = "Att3rdPres")
pairs.panels(squad_data)


# 5. visualization in clusters.
# 5. visualization in clusters.
# 5. visualization in clusters.
# 5. visualization in clusters.

# Scale the numerical columns
scaled_data <- scale(squad_data[, c("W", "D", "L", "SaveRt", "Att3rdPres", "Def3rdPres")])

# Perform k-means clustering
set.seed(123)
k <- 19 # Number of clusters
kmeans_model <- kmeans(scaled_data, k)

# Add cluster labels to data
squad_data$cluster <- as.factor(kmeans_model$cluster)

# Visualize the clusters using ggplot2

ggplot(squad_data, aes(x = Def3rdPres, y = Att3rdPres, color = cluster)) + 
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Clustering of Teams based on Performance",
       x = "Defensive Pressure", y = "Offensive Pressure")



# Visualizing using hierarchical clustering
squad_data
premier.labels <- squad_data$Squad

#scaling the data
data_ <- squad_data[2:9]
str(data_)
data_std <- scaled_data

# Using the distance function
?dist. # this shows the different kind of distance. but we will use euclidean

data.dist = dist(data_std); data.dist

hier_clust <- hclust(data.dist,
                     method = "complete"); hier_clust


# Dendrogram
plot(hier_clust)
# a net way
rect.hclust(hier_clust, k=3,
            border = 2:5) 
# create clusters

premier.cluster <-cutree(hier_clust, k=3);premier.cluster


# Visualize the cluster
rownames(data_std) <- paste(squad_data$Species, 1:dim(squad_data)[1], sep = "_")



fviz_cluster(list(data=data_std, cluster=premier.cluster))

#creating a table

table(premier.cluster, squad_data$Squad)

summary(table(premier.cluster, squad_data$Squad))



# Main Objectives:
# To examine the different tactical approaches used by teams in the English Premier League
# To investigate any data trends or patterns that might be pertinent to tactical strategies.
#	To identify teams that are comparable and may use similar tactical approaches
# To determine the league's most effective (Rank) tactical approach in regard to key performance metrics



cor(squad_data[,c("W", "D", "L", "SaveRt", "Def3rdPres", "Att3rdPres")])

ggplot(squad_data, aes(x = Att3rdPres, y = Def3rdPres)) +
  geom_point(aes(colour = SaveRt)) +
  labs(title = "Attacking vs Defensive Pressure", x = "Attacking Pressure", y = "Defensive Pressure")

squad_data %>% 
  group_by(Squad) %>% 
  summarise(mean_W = mean(W), mean_D = mean(D), mean_L = mean(L), mean_SaveRt = mean(SaveRt))


# To investigate any data trends or patterns that might be pertinent to tactical strategies.
# Plotting the distribution of SaveRt, Def3rdPres, and Att3rdPres 
# using histograms or density plots to see if there are any common patterns or outliers.

par(mfrow = c(1, 3))
hist(squad_data$SaveRt, main = "Save Rate")
hist(squad_data$Def3rdPres, main = "Defense Pressure")
hist(squad_data$Att3rdPres, main = "Attacking Pressure")






#metrics used to analyze tactics includes: team formations, passing patterns, shots, goals, 
#Pressing: This refers to the tactic of trying to win the ball back as quickly as possible after losing possession. Metrics such as successful pressures, pressure success rate, and recoveries in the opponent's half can be used to analyze the effectiveness of a team's pressing tactics.

#Possession: This refers to the amount of time a team spends in control of the ball during a match. Metrics such as possession percentage, number of passes, pass completion rate, and time in possession can be used to analyze a team's possession-based tactics.

#Counterattacks: This refers to the tactic of quickly transitioning from defense to attack when the opponent is out of position. Metrics such as successful dribbles, through balls, and counterattack goals can be used to analyze the effectiveness of a team's counterattacking tactics.
#Defensive solidity: This refers to the ability of a team to defend as a unit and prevent the opponent from creating scoring opportunities. Metrics such as tackles, interceptions, clearances, and blocks can be used to analyze a team's defensive tactics.

Set pieces: This refers to the tactic of scoring from free kicks, corners, or penalties. Metrics such as set piece goals, set piece attempts, and set piece success rate can be used to analyze the effectiveness of a team's set piece tactics.

Overall, analyzing tactics in a football match requires considering a range of metrics beyond just goals and shots, as they can provide a more detailed understanding of a team's performance and tactics.
