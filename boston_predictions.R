setwd('C:/Users/Elvan/Desktop/PyDataAnkara-master')
library(dplyr)
library(caret)

frankfurt <- read.csv('frankfurt_final.csv')
shanghai <- read.csv('shanghai_final.csv')
manila <- read.csv('manila_final.csv', sep = ";")
ti <- read.csv('ti_final.csv', sep = ";", dec = ",")

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

# Let's do the same thing for shanghai and manila and then combine them together for a more complete view
scaled_frankfurt <- fix_dataframes(frankfurt)
scaled_shanghai <- fix_dataframes(shanghai)
scaled_manila <- fix_dataframes(manila)
scaled_ti <- fix_dataframes(ti)

scaled_tournaments <- rbind(scaled_frankfurt[,1:18], scaled_shanghai[,1:18], scaled_manila[,1:18])
scaled_tournaments2 <- scaled_tournaments
scaled_tournaments2$Participation <- scaled_tournaments$Tournament_Result>0

tournaments <- rbind(frankfurt[,1:18], shanghai[,1:18], manila[,18])
tournaments2 <- tournaments
tournaments2$Participation <- tournaments$Tournament_Result>0

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:4)*50,
                        shrinkage = c(0.01, 0.1, 0.2),
                        n.minobsinnode = 5)

fitControl <- trainControl(method = 'repeatedcv',
                           number = 5,
                           repeats = 5)

# Invite prediction
train_invite <- rbind(scaled_frankfurt, scaled_shanghai, scaled_manila)
test_invite <- scaled_ti[,-19]
#train_invite$Outcome <- as.factor(train_invite$Outcome)

set.seed(3)
model2 <- train(Outcome~.,
                data = train_invite[,-c(1:2,14)],
                method = "gbm",
                trControl = fitControl,
                verbose = T,
                tuneGrid = gbmGrid)


model2



colnames(test_invite)[1:18] <- colnames(train_invite)[1:18]
preds2 <- predict(model2, test_invite[,-c(1:2,14)])
test_invite$PredictedOutcome <- as.numeric(preds2)
team_invite_predicted <- aggregate(test_invite$PredictedOutcome, list(ti$New_Team), sum)
team_invite_predicted <- team_invite_predicted[order(-team_invite_predicted$x),]
