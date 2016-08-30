library(dplyr)
frankfurt <- frankfurt_df_fixed[complete.cases(frankfurt_df_fixed),]
shanghai <- shanghai_df_fixed[complete.cases(shanghai_df_fixed),]
manila <- manila_df_fixed[complete.cases(manila_df_fixed),]

# Drop unused levels
frankfurt<-droplevels(frankfurt)
shanghai<-droplevels(shanghai)
manila<-droplevels(manila)

# Generate features

frankfurt$Player <- as.character(frankfurt$Player)
shanghai$Player <- as.character(shanghai$Player)
manila$Player <- as.character(manila$Player)

shanghai$Changed_Teams <- 1
manila$Changed_Teams <- 1

# Find out if players changed teams between frankfurt and shanghai majors
# This assumes previously unobserved players just formed a new team
for(i in 1:nrow(shanghai)){
  pos <- which(frankfurt$Player == shanghai$Player[i])
  if(length(pos)==1){
    temp <- (as.character(frankfurt$Team[pos])== as.character(shanghai$Team[i]))
    if(temp){
      shanghai$Changed_Teams[i] <- 0
    }  
  }
}

# Find out if players changed teams between shanghai and manila majors
# This assumes previously unobserved players just formed a new team
for(i in 1:nrow(manila)){
  pos <- which(shanghai$Player == manila$Player[i])
  if(length(pos)==1){
    temp <- (as.character(shanghai$Team[pos])== as.character(manila$Team[i]))
    if(temp){
      manila$Changed_Teams[i] <- 0
    }  
  }
}

frankfurt$Total_Games <- frankfurt$Wins + frankfurt$Losses
shanghai$Total_Games <- shanghai$Wins + shanghai$Losses
manila$Total_Games <- manila$Wins + manila$Losses

colnames(frankfurt)[16] <- "Tournament_Result"
colnames(shanghai)[16] <- "Tournament_Result"
colnames(manila)[16] <- "Tournament_Result"

frankfurt$Outcome <- -1

for(i in 1:nrow(frankfurt)){
  temp <- which(shanghai$Player == frankfurt$Player[i])
  if(length(temp) == 1){
    frankfurt$Outcome[i] <- shanghai$Inv_Status[temp]
  }
}

shanghai$Outcome <- -1

for(i in 1:nrow(shanghai)){
  temp <- which(manila$Player == shanghai$Player[i])
  if(length(temp) == 1){
    shanghai$Outcome[i] <- manila$Inv_Status[temp]
  }
}



frankfurt$Outcome <- as.factor(frankfurt$Outcome)
shanghai$Outcome <- as.factor(shanghai$Outcome)
frankfurt <- arrange(frankfurt, Team, desc(GPM))
position <- c(rep_len(1:5, 30), 1, rep_len(1:5,210))
frankfurt$Position <- position

shanghai <- arrange(shanghai, Team, desc(GPM))
shanghai$Position <- rep_len(1:5, nrow(shanghai))

manila <- arrange(manila, Team, desc(GPM))
manila$Position <- rep_len(1:5, nrow(manila))

write.csv(frankfurt, 'frankfurt_final.csv', row.names = F)
write.csv(shanghai, 'shanghai_final.csv', row.names = F)
write.csv(manila, 'manila_final.csv', row.names = F)

train <- rbind(frankfurt, shanghai)
test <- manila


