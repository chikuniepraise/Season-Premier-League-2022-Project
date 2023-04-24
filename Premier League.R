# Loading the necessary libraries
library(dplyr)
library(class)
library(ggplot2)
library(readr)
library(tidyselect)
library(psych)
library(stringr)
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



# Main Objectives:
# To examine the different tactical approaches used by teams in the English Premier League

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


#	To identify teams that are comparable and may use similar tactical approaches



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


# To investigate any data trends or patterns that might be pertinent to tactical strategies.

# 5. visualization in clusters.
# 5. visualization in clusters.
# 5. visualization in clusters.
# 5. visualization in clusters.

# Scale the numerical columns
scaled_data <- scale(squad_data[, c("W", "D", "L", "SaveRt", "Att3rdPres", "Def3rdPres")])

# Perform k-means clustering
set.seed(123)
k <- 5 # Number of clusters
kmeans_model <- kmeans(scaled_data, k)

# Add cluster labels to data
squad_data$cluster <- as.factor(kmeans_model$cluster)

# Visualize the clusters using ggplot2

ggplot(squad_data, aes(x = Def3rdPres, y = Att3rdPres, color = cluster)) + 
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Clustering of Teams based on Performance",
       x = "Defensive Pressure", y = "Offensive Pressure")+
  geom_text(aes(label = Squad), size = 3, hjust = 0, vjust = 0)



# Visualizing using hierarchical clustering
squad_data
premier.labels <- squad_data$Squad

#scaling the data
data_ <- squad_data[2:9]
str(data_)
data_std <- scaled_data

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



fviz_cluster(list(data=data_std, cluster=premier.cluster))+
  geom_point()+
  geom_text(aes(label = premier.labels), size = 3, nudge_x = 0.1, nudge_y = 0.1)

#creating a table

table(premier.cluster, squad_data$Squad)

summary(table(premier.cluster, squad_data$Squad))



# To determine the league's most effective (Rank) tactical approach in regard to key performance metrics


cor(squad_data[,c("W", "D", "L", "SaveRt", "Def3rdPres", "Att3rdPres")])

ggplot(squad_data, aes(x = Att3rdPres, y = Def3rdPres)) +
  geom_point(aes(colour = SaveRt)) +
  geom_text(aes(label = Squad), size = 2, hjust = 0, vjust = 0) +
labs(title = "Attacking vs Defensive Pressure", x = "Attacking Pressure", y = "Defensive Pressure")

squad_data %>% 
  group_by(Squad) %>% 
  summarise(mean_W = mean(W), mean_D = mean(D), mean_L = mean(L), mean_SaveRt = mean(SaveRt))
# To investigate any data trends or patterns that might be pertinent to tactical strategies.
# Plotting the distribution of SaveRt, Def3rdPres, and Att3rdPres 
# using histograms or density plots to see if there are any common patterns or outliers.

squad_data_ <- mutate(squad_data, squad_label = 1:20)

par(mfrow = c(1, 3))
hist(squad_data_$SaveRt, col = squad_data_$squad_label, main = "Save Rate")
hist(squad_data$Def3rdPres, col = squad_data_$squad_label, main = "Defense Pressure")
hist(squad_data$Att3rdPres, col = squad_data_$squad_label, main = "Attacking Pressure")

legend("left", legend = levels(as.factor(squad_data$Squad)), col = 1:length(levels(as.factor(squad_data$Squad))), pch = 15, bty = "n", x.intersp = 1, y.intersp = 1, inset = 0.7)
                         # attacking attributes in soccer can include metrics such as goals scored, 
#shots on target, shot accuracy, key passes, dribbles completed, and chances created

#Goals scored = Gf in table, TotShTg : Shots on Target in squadshooting
#G/SoT : Goals per shot on target, PK : penalty kick made


# Defensive attributes, can include metrics such as tackles, 
# interceptions, clearances, blocks, duels won, and aerials won. 


# data Cleaning. 
squad_shooting <- read.csv("~/R Forcast/Projects/england_premier_league_squad_shooting_22.csv", sep = ";")
View(squad_shooting)
att <- select(table, c(1,2,4,5,6,7))
atta <- select(squad_shooting, c(1,6,11,14))
defensive <- select(defensive_actions, c(1,5,6,7,15,17))

#merging the datasets.
Offensive <- merge(att, atta, by="Squad")
View(Offensive)
View(defensive)

# Scale the numerical columns
scaled_ <- scale(Offensive[, c("W", "D", "L", "GF", "TotShTg", "G.SoT", "PK")])

# Perform k-means clustering
set.seed(123)
k <- 5 # Number of clusters
kmeans_model <- kmeans(scaled_, k)

# Add cluster labels to data
Offensive$cluster <- as.factor(kmeans_model$cluster)

# Visualize the Goals scored and shots on target clusters using ggplot2 

ggplot(Offensive, aes(x = GF, y = TotShTg, color = cluster)) + 
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Clustering of Teams based on Goal Scored and Shots on Target.",
       x = "Goal Scored", y = "Shots on Target")+
  geom_text(aes(label = Squad), size = 3, hjust = 0, vjust = 0)


# Goals shot on target in relation to goals scored.

ggplot(Offensive, aes(x = GF, y = G.SoT, color = cluster)) + 
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Clustering of Teams based on Goal Scored and Shots Accuracy.",
       x = "Goal Scored", y = "Goals per Shots on Target")+
  geom_text(aes(label = Squad), size = 3, hjust = 0, vjust = 0)

# Goals Shot on target in relation  to wins
ggplot(Offensive, aes(x = W, y = G.SoT, color = cluster)) + 
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Clustering of Teams based on Wins and Shots on Target.",
       x = "Goal Scored", y = "Wins")+
  geom_text(aes(label = Squad), size = 3, hjust = 0, vjust = 0)



# Penalty kick in relationship to goals scored


ggplot(Offensive, aes(x = PK, y = GF, color = cluster)) + 
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Clustering of Teams based on Penalty kicks and  Goal Scored.",
       x = "Penalty Kicks", y = "Goals scored")+
  geom_text(aes(label = Squad), size = 3, hjust = 0, vjust = 0)


#Defense tactics
defensive

# Scale the numerical columns
scale_ <- scale(defensive[, c("Def3rdTck", "Mid3rdTck", "Att3rdTck", "Def3rdPres", "Att3rdPres")])

# Perform k-means clustering
set.seed(123)
k <- 5 # Number of clusters
kmeans_model <- kmeans(scale_, k)

# Add cluster labels to data
defensive$cluster <- as.factor(kmeans_model$cluster)

# Visualize the Tackles in Defensive and attacking 3rd. 

ggplot(defensive, aes(x = Def3rdTck, y = Att3rdTck, color = cluster)) + 
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Clusters in Tackles in Defensive 3rd and Attacking 3rd.",
       x = "Tackles in Defensive 3rd", y = "Tackles in Attacking 3rd")+
  geom_text(aes(label = Squad), size = 3, hjust = 0, vjust = 0)


# Visualize the Tackles in Attacking Pressure and attacking 3rd. 

ggplot(defensive, aes(x = Att3rdPres, y = Att3rdTck, color = cluster)) + 
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Clusters in Pressures in Attacking  and Tackles in Attacking .",
       x = "Pressures in Defensive 3rd", y = "Tackles in Attacking 3rd")+
  geom_text(aes(label = Squad), size = 3, hjust = 0, vjust = 0)



# Visualize the Defensive Pressure and Mid Tackles. 

ggplot(defensive, aes(x = Def3rdPres, y = Mid3rdTck, color = cluster)) + 
  geom_point(size = 3) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "Clusters in Defensive Pressure and Mid Tackles.",
       x = "Pressures in Defensive 3rd", y = "Tackles in Attacking 3rd") +
  geom_text(aes(label = Squad), size = 3, hjust = 0, vjust = 0)



