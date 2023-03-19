library(rvest)
library(dplyr)
library(data.table)
library(xml2)
library(foreach)
library(lubridate)
Sys.setlocale(category = "LC_ALL", locale = "Greek")
'%!in%' <- function(x,y)!('%in%'(x,y))
# 1. Extract the updated league table  

TableLink <- "https://cfa.com.cy/Gr/competitions/50549072"

page <- read_html(TableLink)

LeaugeTable <- as.data.table(page %>% html_nodes(".ptable")%>%
                               html_table(fill = T))

colnames(LeaugeTable) <- c("POSITION","TEAM","GAMES","W","D","L","GF","GA","PTS")


# 2. EXTRACT ALL MATCH RESULTS

ScoreLink <- "https://cfa.com.cy/Gr/fixtures/%2050549072"

page <- read_html(ScoreLink)

ScoreTable <- as.data.table(
  page %>% html_nodes("table")%>%html_table())


ScoreTable <- ScoreTable[,.(`Home Team`= X,Score = X.2,`Away Team` = X.4, Stadium = X.5)]

#Notes (decisions of kop) : 
# Thiella - mouttagiakka : 3 - 0 
# Pseudas AEN 0-3
# Orfeas - Geroskipou 3 - 0


# 2a Fix Tables

# Manually fix cancelled matches 

ScoreTable[,k:=1:.N]


ScoreTable <- ScoreTable[!((k < 197) & Score %in% c("15:00","14:30","15:30"))]

ScoreTable[,k := NULL]

number_of_matches <- 15

start_date <- lubridate::dmy("24/9/2022")

allpossible_dates <- seq(start_date, lubridate::dmy("06/05/2023"), by=7)


# Manually exclude dates with no games
to_exclude_dates <- c(lubridate::dmy("24/12/2022"),
                      lubridate::dmy("31/12/2022"),
                      lubridate::dmy("15/04/2023"))

#create dates dim
allpossible_dates = allpossible_dates[which(allpossible_dates %!in% to_exclude_dates)]


date_mapping<-data.table(
  Date = allpossible_dates,
  `Match day` = c(1:length(allpossible_dates))
)


#The data are extracted with order so . .

ScoreTable[,`Match day`:= unlist(purrr::map(1:(number_of_matches*2),~rep(.x,8)))]

#make score variables

extract_score1 <- function(x){
  value=stringr::str_split(string = x,n = 2,pattern = "-")[[1]][1]
  return(value)
}

extract_score2 <- function(x){
  value=stringr::str_split(string = x,n = 2,pattern = "-")[[1]][2]
  return(value)
}



ScoreTable[,`Score Home`:=unlist(lapply(`Score`,extract_score1))]
ScoreTable[,`Score Away`:=unlist(lapply(`Score`,extract_score2))]


#join date 
ScoreTable <- date_mapping[ScoreTable,on=.(`Match day`)]

ScoreTable[, (c("Score Away","Score Home")) := lapply(.SD,as.integer), .SDcols = c("Score Away","Score Home")]


# Add Result 


ScoreTable[, Result := ifelse(`Score Home` > `Score Away`,1,ifelse(`Score Away` > `Score Home`,2,"x"))]


# Calculate  current standings 

# Function to calculate league standings
calculate_standings <- function(data) {
  standings <- data.table(Team = unique(c(data$`Home Team`, data$`Away Team`)),
                          Points = 0, Wins = 0, Draws = 0, Losses = 0)
  
  for (i in 1:nrow(data)) {
    match <- data[i]
    team_a <- match$`Home Team`
    team_b <- match$`Away Team`
    result <- match$Result
    
    team_a_index <- which(standings$Team == team_a)
    team_b_index <- which(standings$Team == team_b)
    
    if (result == "1") {
      standings[team_a_index, c("Points", "Wins") := list(Points + 3, Wins + 1)]
      standings[team_b_index, c("Losses") := Losses + 1]
    } else if (result == "2") {
      standings[team_b_index, c("Points", "Wins") := list(Points + 3, Wins + 1)]
      standings[team_a_index, c("Losses") := Losses + 1]
    } else if (result == "x") {
      standings[team_a_index, c("Points", "Draws") := list(Points + 1, Draws + 1)]
      standings[team_b_index, c("Points", "Draws") := list(Points + 1, Draws + 1)]
    }
  }
  
  standings <- standings[order(-Points, -Wins, -Draws, Losses)]
  return(standings)
}

standings <- calculate_standings(ScoreTable[!is.na(Result)])

ScoreTable[, MatchDayId := 1:.N , by = .(`Match day`)]

ScoreTable[, MatchIDUnique := paste0(`Match day`,"_",MatchDayId)]

current_match_day <- 24

matches <- ScoreTable[`Match day` == current_match_day + 1,.(MatchIDUnique)]

# Recursive function to generate all possible outcomes
generate_outcomes <- function(matches, outcomes = list()) {
    current_match <- matches[1]
    remaining_matches <- matches[-1]
    new_outcomes <- list()
    
    for (result in c("1", "x", "2")) {
      current_outcome <- copy(current_match)
      current_outcome$Result <- result
      
      if (length(outcomes) == 0) {
        new_outcomes <- c(new_outcomes, list(current_outcome))
      } else {
        for (outcome in outcomes) {
          new_outcomes <- c(new_outcomes, list(rbind(outcome, current_outcome)))
        }
      }
    }
    
    return(generate_outcomes(remaining_matches, new_outcomes))
}


AllOutcomes <- generate_outcomes(matches = matches)

