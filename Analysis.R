setwd('C:/Users/Elvan/Desktop/PyData')
library(dplyr)
library(ggplot2)
library(corrgram)
library(caret)

frankfurt <- read.csv('frankfurt_final.csv')
shanghai <- read.csv('shanghai_final.csv')
manila <- read.csv('manila_final.csv')

# Visualization
# Density plot to see farm distribution between regions and positions
g <- ggplot(data = frankfurt, aes(x = GPM, color = Region))
g + geom_density() + facet_grid(Position ~.)

# Density plot to see exp distribution between regions and positions
g <- ggplot(data = frankfurt, aes(x = XPM, color = Region))
g + geom_density() + facet_grid(Position ~.)

# Doesn't really tell us much because of graphical scaling, let's apply actual scaling
# We want to see if there are any differences based on region, so we scale based on the Positions
XGPM_scale <- function(x){
  x$GPM <- as.vector(scale(x$GPM))
  x$XPM <- as.vector(scale(x$XPM))
  return(x)
}

fix_dataframes <- function(x){
  x_scaled <- split(x, as.factor(x$Position))
  x_scaled <- lapply(x_scaled, XGPM_scale)
  x_scaled <- rbind(x_scaled[[1]], x_scaled[[2]], 
                    x_scaled[[3]], x_scaled[[4]], x_scaled[[5]])
  x_scaled <- arrange(x_scaled, Team, Position)
  return(x_scaled)
}

scaled_frankfurt <- fix_dataframes(frankfurt)
# Density plot to see farm distribution between regions and positions
g <- ggplot(data = scaled_frankfurt, aes(x = GPM, color = Region))
g + geom_density() + facet_grid(Position ~.)

# Density plot to see exp distribution between regions and positions
g <- ggplot(data = scaled_frankfurt, aes(x = XPM, color = Region))
g + geom_density() + facet_grid(Position ~.)

# Subset the data so that we retain only the teams that participated in the main event
main_event_frankfurt <- scaled_frankfurt[frankfurt$Tournament_Result > 0,]
# Density plot to see farm distribution between regions and positions
g <- ggplot(data = main_event_frankfurt, aes(x = GPM, color = Region))
g + geom_density()  + facet_grid(Position ~.)

# Density plot to see exp distribution between regions and positions
g <- ggplot(data = main_event_frankfurt, aes(x = XPM, color = Region))
g + geom_density() + facet_grid(Position ~.)

# Let's see how GPM differs between teams that participated in the main event and the teams that hadnt
scaled_frankfurt2 <- scaled_frankfurt
scaled_frankfurt2$Participation <- scaled_frankfurt$Tournament_Result>0
g <- ggplot(data = scaled_frankfurt2, aes(x = GPM, color = Participation))
g + geom_density() + facet_grid(Region ~.)


# Let's do the same thing for shanghai and manila and then combine them together for a more complete view
scaled_shanghai <- fix_dataframes(shanghai)
scaled_manila <- fix_dataframes(manila)

scaled_tournaments <- rbind(scaled_frankfurt[,1:18], scaled_shanghai[,1:18], scaled_manila)
# Density plot to see farm distribution between regions and positions
g <- ggplot(data = scaled_tournaments, aes(x = GPM, color = Region))
g + geom_density()  + facet_grid(Position ~.)

# Density plot to see exp distribution between regions and positions
g <- ggplot(data = scaled_tournaments, aes(x = XPM, color = Region))
g + geom_density() + facet_grid(Position ~.)

# Let's see how GPM differs between teams that participated in the main event and the teams that hadnt
scaled_tournaments2 <- scaled_tournaments
scaled_tournaments2$Participation <- scaled_tournaments$Tournament_Result>0
g <- ggplot(data = scaled_tournaments2, aes(x = GPM, color = Participation))
g + geom_density() + facet_grid(Region ~.)

# Scatterplot
# To observe the dual relatipnship between variables
g <- ggplot(data = scaled_tournaments2, aes(x = XPM, y= GPM, color = Participation))
g + geom_point() + facet_grid(Position ~.)

# What if we hadn't scaled?
tournaments <- rbind(frankfurt[,1:18], shanghai[,1:18], manila)
tournaments2 <- tournaments
tournaments2$Participation <- tournaments$Tournament_Result>0
g <- ggplot(data = tournaments2, aes(x = XPM, y= GPM, color = Participation))
g + geom_point() + facet_grid(Position ~.)

# Correlation
numeric_vars <- which(sapply(tournaments, is.numeric))
pc <- cor(tournaments[,numeric_vars])
corrgram(tournaments[,numeric_vars], order = FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Dota2 Valve Tournament Results in 2015-2016")
# Regression
# We don't want the Player and Team information because those are unique values and they will cause overfitting.
tournaments$Tournament_Result <- as.factor(tournaments$Tournament_Result)
train_indexes <- createDataPartition(tournaments$Tournament_Result, p = .75, list = F)
train_data <- tournaments[train_indexes, -c(1,2)]
test_data <- tournaments[-train_indexes, -c(1,2)]
fitControl <- trainControl(method = 'repeatedcv',
                           number = 3,
                           repeats = 3)

model1 <- train(Tournament_Result~. ,
                data = train_data,
                method = 'gbm',
                trControl = fitControl,
                verbose = T)
model1
preds1 <- predict(model1, test_data[,-14])
actual_results <- test_data$Tournament_Result
results_comparison <- data.frame(actual_results, preds1)
results_comparison$actual_results <- as.integer(results_comparison$actual_results)-1
results_comparison$preds1 <- as.integer(results_comparison$preds1)-1
qplot(results_comparison$preds1, results_comparison$actual_results)

player_team_names_test <- tournaments[as.integer(row.names(test_data)), 1:2]
results_comparison <- data.frame(results_comparison,player_team_names_test)


# Invite prediction
train_invite <- rbind(scaled_frankfurt, scaled_shanghai)
test_invite <- scaled_manila
train_invite$Outcome <- as.factor(train_invite$Outcome)
model2 <- train(Outcome~.,
                data = train_invite[,-c(1:2)],
                method = "gbm",
                trControl = fitControl,
                verbose = T)
model2
preds2 <- predict(model2, test_invite[,-c(1:2)])
actual_results_inv <- factor(test_invite$Inv_Status, levels = levels(preds2))
test_invite$TrueOutcome <- as.integer(actual_results_inv)
test_invite$PredictedOutcome <- as.integer(preds2)
team_invite_true <- aggregate(test_invite[, 19], list(test$Team), mean)
team_invite_predicted <- aggregate(test_invite[, 20], list(test$Team), mean)
team_invite_predicted$x <- as.integer(round(team_invite_predicted$x))
team_invite_true <- team_invite_true$x
team_invite_predicted <- team_invite_predicted$x
team_invite_predicted <- factor(team_invite_predicted, levels = c("1","2","3","4"))
confusionMatrix(team_invite_predicted,
                factor(team_invite_true, levels = levels(team_invite_predicted)))
