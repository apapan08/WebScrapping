# Script to Create a Racing Bar Plot 

# Import libraries
library(data.table)
library(foreach)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(r2d3)

data_score <- readRDS("output/Scoretable.rds")
source("./utils/HelpFunction.R")

# add flag variable for winner 

data_score[, HomePoints := ifelse(`Score Home` > `Score Away`,3,
                           ifelse(`Score Home` < `Score Away`,0,1))]

data_score[, AwayPoints := ifelse(`Score Home` > `Score Away`,0,
                                  ifelse(`Score Home` < `Score Away`,3,1))]


data_score_melted <- melt(data_score[,.(MatchId,`Home Team`,`Away Team`,HomePoints,AwayPoints)],
                          id.vars = c("MatchId","HomePoints","AwayPoints"))

# to find more efficient way 

data <- foreach(i = 1:nrow(data_score_melted))%do%
  {
    
    points_team <- ifelse(((data_score_melted[i,variable] == "Home Team")
                           & (data_score_melted[i,HomePoints] == 3)),3,
                          ifelse(((data_score_melted[i,variable] == "Home Team") & (data_score_melted[i,AwayPoints] == 3)),0,
                                 ifelse(((data_score_melted[i,variable] == "Away Team") & (data_score_melted[i,AwayPoints] == 3)),3,
                                 ifelse(((data_score_melted[i,variable] == "Away Team") & (data_score_melted[i,HomePoints == 3])),0,1))))
    dtreturn <- data.table(
      name = data_score_melted[i,value],
      MatchId = data_score_melted[i,MatchId],
      value = points_team
    )
    return(dtreturn)
  }


data <- rbindlist(data)
data <- data_score[,.(MatchId,`Match day`)][data , on = .(MatchId)]

data[,`Match day label` := paste0("     ",`Match day`," Αγωνιστική")]

data <- as.data.frame(data)




gd3 <- barchartrace_r2d3(
  data = data, 
  name = "name", date = "Match day", value = "value", date_label = "Match day label", colour = "name", 
  cumulative = TRUE, 
  title = "Α ΟΜΙΛΟΣ ΕΠΙΛΕΚΤΗΣ ΣΤΟΚ", 
  subtitle = "", 
  caption = "Source: cfa.com.cy/", 
  mood = "happy", top_n = 7, duration = 2000, 
  css = "scripts/styles.css", script = "scripts/barchartrace.js", 
  width = 1000, height = 600,
  margin = c(80, 10, 5, 0), 
  label_fix = FALSE
)
gd3




