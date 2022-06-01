library(rvest)
library(dplyr)
library(data.table)
library(xml2)
library(foreach)
library(lubridate)
Sys.setlocale(category = "LC_ALL", locale = "Greek")
'%!in%' <- function(x,y)!('%in%'(x,y))
# 1. Extract the updated league table  

TableLink <- "https://www.cfa.com.cy/Gr/competitions/33715608"

page <- read_html(TableLink)

LeaugeTable <- as.data.table(page %>% html_nodes(".ptable")%>%
                               html_table(fill = T))

colnames(LeaugeTable) <- c("POSITION","TEAM","GAMES","W","D","L","GF","GA","PTS")


# 2. EXTRACT ALL MATCH RESULTS

ScoreLink <- "https://www.cfa.com.cy/Gr/fixtures/33715608"

page <- read_html(ScoreLink)

ScoreTable <- as.data.table(
  page %>% html_nodes("table")%>%html_table())


ScoreTable <- ScoreTable[,.(`Home Team`= X,Score = X.2,`Away Team` = X.4, Stadium = X.5)]


# 2a Fix Tables



#apea vs aspis was cancelled and played on the next day so we delete it
ScoreTable <-  ScoreTable[!(Score=="14:30")]


number_of_matches<-11

start_date <- lubridate::dmy("25/9/2021")

allpossible_dates<-seq(start_date, lubridate::dmy("02/04/2022"), by=7)


# Manually exclude dates with no games
to_exclude_dates <- c(lubridate::dmy("13/11/2021"),
                      lubridate::dmy("09/10/2021"),
                      lubridate::dmy("25/12/2021"),
                      lubridate::dmy("01/01/2022"),
                      lubridate::dmy("05/03/2022"),
                      lubridate::dmy("26/03/2022"))

#create dates dim
allpossible_dates = allpossible_dates[which(allpossible_dates %!in% to_exclude_dates)]


date_mapping<-data.table(
  Date = allpossible_dates,
  `Match day` = c(1:length(allpossible_dates))
)


#The data are extracted with order so . .

ScoreTable[,`Match day`:= unlist(purrr::map(1:(number_of_matches*2),~rep(.x,6)))]

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



# 3. Extract info players

page <- read_html(TableLink)


# extract link for each team 
AllLinks <- page%>%html_nodes(".std-team-text")

MappingTeamTable <- data.table(
  Team = LeaugeTable[,TEAM],
  Link = sapply(AllLinks, function(x) xml_attrs(x)[["href"]])
)

# extract all players
PlayerTable <- foreach(i=1:nrow(MappingTeamTable)) %do% {
  PageTeamLink = paste0("https://www.cfa.com.cy",MappingTeamTable[i,Link])
  PageTeamHtml <- read_html(PageTeamLink)
  TeamTable <- as.data.table(PageTeamHtml %>% html_nodes("table.col-md-12")%>%
                               html_table(header = T))
  TeamTable[,TEAM := MappingTeamTable[i,Team]]
  colnames(TeamTable) = c("NAME","APPS","SUB","STARTING","GOAL","OWN GOAL","YELLOW","RED","MINUTES","TEAM")
  return(TeamTable)
}

PlayerTable <- rbindlist(PlayerTable)




#function to extract Age for each player
CheckIfHref <- function(x) {
  if ("href" %in% names(xml_attrs(x))){
    TRUE
  } else {
    FALSE
  }
}

#make it faster with do par 
PlayerAgeInfo <- foreach(i=1:nrow(MappingTeamTable)) %do% {
  PageTeamLink = paste0("https://www.cfa.com.cy",MappingTeamTable[i,Link])
  PlayerInfoHtml <- read_html(PageTeamLink) %>% html_nodes("a")%>%html_attr("href")
  
  
  AllPlayersLink <- unique(PlayerInfoHtml[grepl("player",PlayerInfoHtml)])
  PlayersEachTeam <- foreach(j=1:length(AllPlayersLink))%do% {
    playerlink = AllPlayersLink[j]
    temp_info <- read_html(paste0("https://www.cfa.com.cy",playerlink)) %>%
      html_nodes("td.rightColumn")%>%html_text()
    temp_name <-read_html(paste0("https://www.cfa.com.cy",playerlink)) %>%
      html_nodes(".fpHeader1 span")%>%html_text()
    dt_temp <- data.table(
      Team = temp_info[1],
      Name = temp_name,
      DateBirth = temp_info[2]
    )
    return(dt_temp)
  }
  PlayersEachTeam <- rbindlist(PlayersEachTeam)
  return(PlayersEachTeam)
}


PlayerAgeInfo <- rbindlist(PlayerAgeInfo)

#For some reason only one player has its position in the place where the age should have been 

PlayerAgeInfo <- PlayerAgeInfo[Name == "SHEVERINIO SHENILLIO  BLANKER ",DateBirth :="29/05/1999"]

PlayerAgeInfo[,Age := floor(interval(lubridate::dmy(DateBirth), lubridate::ymd("2022-03-30")) / years(1))]


# 4. Extract info matches
page <- read_html(ScoreLink)
AllLinksInfo <- unique(
  page %>% html_nodes("a")%>%html_attr("href")
)

AllGamesLink <- unique(AllLinksInfo[grepl("game_details",AllLinksInfo)])

#Manually remove APEA-ASPIS match

AllGamesLink <- AllGamesLink[!(grepl("33742188",AllGamesLink))]

MatchIdMapping = data.table(
  matchid = 1:length(AllGamesLink),
  link = AllGamesLink
)


AllmatchGoals <- foreach(i =  1:length(AllGamesLink))%do% {
  testing_link <- AllGamesLink[i]
  GameInfoPage <- read_html(paste0("https://www.cfa.com.cy",testing_link))
  #check if any goals
  AnyGoals <- any(sapply(GameInfoPage %>%html_nodes("img"), function(x) grepl("ball.gif",x)))
  if (AnyGoals){
    InfoGameTable <- as.data.table(GameInfoPage %>% html_nodes("table:nth-of-type(3)")%>%html_table())
    InfoGameTable[,section_id := ifelse(((X1==""|is.na(X1)))&(X2==""|is.na(X2))&(X3==""|is.na(X3)),1,0)]
    InfoGameTable[,section_id := cumsum(section_id)]
    InfoGameTable = InfoGameTable[(section_id<=1)&((X1!="")|(X2!=""))]
    MinutesScored = data.table(
      MatchId = MatchIdMapping[i,matchid],
      HomeGoals = list(
        sapply(InfoGameTable[X1 != "",X2],function(x) as.numeric(stringi::stri_replace_all_fixed(x,"'","")),USE.NAMES = FALSE)),
      AwayGoals = list(
        sapply(InfoGameTable[X3 != "",X2],function(x) as.numeric(stringi::stri_replace_all_fixed(x,"'","")),USE.NAMES = FALSE)
      )
    )
    return(MinutesScored)
    
  }else{
    MinutesScored = data.table(
      MatchId = MatchIdMapping[i,matchid],
      HomeGoals = numeric(),
      AwayGoals = numeric()
    )
    return(MinutesScored)
  }
}
AllmatchGoals <- rbindlist(AllmatchGoals)
# MATCHID
ScoreTable[,MatchId := 1:.N]

ScoreTable <- AllmatchGoals[ScoreTable,on = .(MatchId)]

# We output four files 


fwrite(ScoreTable,"/output/Scoretable.csv")
fwrite(LeaugeTable,"/output/LeaugeTable.csv")
fwrite(PlayerAgeInfo,"/output/PlayerAgeInfo.csv")

