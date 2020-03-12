# Feb 16, 2020

# changes: instead of relying on a static excel table, we now scrape real time player data updated daily from basketball-reference.com

# libraries needed
devtools::install_github("thomasp85/patchwork")
libraries <- c("dplyr", "data.table", "openxlsx", "readxl", "ggplot2", "patchwork")
sapply(libraries, require, character.only = T)

# sourcing the scraping script:
source("/Users/tj/OneDrive/Computer_Stuff/GitHub/NBA_Sim/NBA_DataScrape.R")

# the data:
players_all <- fread("/Users/tj/Documents/Data/NBA/PlayerStats_2020.03.10.csv")
players <- players_all %>% select(Player, Tm, `3PA`, `3P%`, `2PA`, `2P%`, FTA, `FT%`, TOV, ORB, DRB, DWS, G, MP) %>% data.table()

# getting each players probability of playing within each team
players$DWS_zscore <- (players$DWS - mean(players$DWS))/sd(players$DWS)
players$DWS_scaled <- (players$DWS_zscore - min(players$DWS_zscore)) / (max(players$DWS_zscore) - min(players$DWS_zscore))
players$DWS_scaled <- players$DWS_scaled - mean(players$DWS_scaled)
players$DWS_zscore <- NULL



#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# possession function
## read this as team A is on offense, team B is on defense

# need to initialize this outside of the function to reset, then loop the possession function
playByPlay <- data.frame("Team" = character(), "Player" = character(), "Event" = character(), "Points" = double(), stringsAsFactors = F) 

# team_A = team w/ the ball
# team_B = team defending
# printPlayByPlay = bool, if you run the function once, it's nice to set this to TRUE
# def_multiplier = some number, 0 means defense has no impact, 1 means a regular impact, 2 means 2x, 3 = 3x, etc.

possession <- function(team_A, team_B, printPlayByPlay = TRUE, def_multiplier = 1){
  
  team_A1 = players[Tm == team_A]
  team_B1 = players[Tm == team_B]
  
  # using total season minutes played (MP) as play probability
  team_A5 = team_A1[Player %in% sample(team_A1$Player, size = 5, replace = FALSE, prob = team_A1$MP)]
  team_B5 = team_B1[Player %in% sample(team_B1$Player, size = 5, replace = FALSE, prob = team_B1$MP)]
  
  
  # action
  possible_actions <- c(paste0(1:5, "_2P%"), paste0(1:5, "_3P%"), paste0(1:5, "_FT%"), paste0(1:5, "_TOV"))
  prob_weights <- c(team_A5$`2PA`, team_A5$`3PA`, team_A5$FTA, team_A5$TOV)
  
  rebound_tbl <- data.table(Player = c(team_A5$Player, team_B5$Player), Tm = c(team_A5$Tm, team_B5$Tm), Rebounds = c(team_A5$ORB, team_B5$DRB))
  
  # away team's defensive impact
  defImpact <- mean(team_B5$DWS_scaled)*-1*def_multiplier
  
  # what happens? 
  # Initialize these variables to begin, and they will eventually reset
  # event <- "miss"
  rebound <- "homeRebound"
  
  # the actual play
  while(rebound == "homeRebound"){
    
    theAction <- sample(possible_actions, size = 1, prob = prob_weights)
    theAction2 <- c(substr(theAction, 1, 1), substr(theAction, 3, nchar(theAction))) 
    prb <- (team_A5[as.numeric(theAction2[1]),] %>% select(theAction2[2]))*(1+defImpact) #def impact happens here, only impacts shooting, is already inherent in rebound counts
    
    if(theAction2[2] == "TOV"){
      rebound <- "awayRebound" # to end the while loop
      assign("playByPlay", rbind(playByPlay, data.frame("Team" = team_A, "Player" = team_A5[as.numeric(theAction2[1]),]$Player, "Event" = "Turnover", "Points" = 0, stringsAsFactors = F)), envir = parent.frame()) # save event
      
    }else if(theAction2[2] == "2P%"){
      
      pt <- sample(c(0, 2), size = 1, prob = c(1-prb, prb))
      assign("playByPlay", rbind(playByPlay, data.frame("Team" = team_A, "Player" = team_A5[as.numeric(theAction2[1]),]$Player, "Event" = "2pt", "Points" = pt, stringsAsFactors = F)), envir = parent.frame()) # save event
      
      if(pt == 0){
        
        rebound_ind <- sample(1:10, size = 1, prob = rebound_tbl$Rebounds)
        if(rebound_ind <= 5){rebound <- "homeRebound"}else{rebound <- "awayRebound"}
        assign("playByPlay", rbind(playByPlay, data.frame("Team" = rebound_tbl[rebound_ind,]$Tm, "Player" = rebound_tbl[rebound_ind,]$Player, "Event" = "Rebound", "Points" = 0, stringsAsFactors = F)), envir = parent.frame()) 
        
      }else{
        rebound <- "awayRebound" # to end while loop
      }
      
    }else if(theAction2[2] == "3P%"){
      
      pt <- sample(c(0, 3), size = 1, prob = c(1-prb, prb))
      assign("playByPlay", rbind(playByPlay, data.frame("Team" = team_A, "Player" = team_A5[as.numeric(theAction2[1]),]$Player, "Event" = "3pt", "Points" = pt, stringsAsFactors = F)), envir = parent.frame()) # save event
      if(pt == 0){
        
        rebound_ind <- sample(1:10, size = 1, prob = rebound_tbl$Rebounds)
        if(rebound_ind <= 5){rebound <- "homeRebound"}else{rebound <- "awayRebound"}
        assign("playByPlay", rbind(playByPlay, data.frame("Team" = rebound_tbl[rebound_ind,]$Tm, "Player" = rebound_tbl[rebound_ind,]$Player, "Event" = "Rebound", "Points" = 0, stringsAsFactors = F)), envir = parent.frame()) 
        
      }else{
        rebound <- "awayRebound"
      }
      
    }else if(theAction2[2] == "FT%"){
      
      pt <- sample(c(0, 1), size = 1, prob = c(1-prb, prb))
      assign("playByPlay", rbind(playByPlay, data.frame("Team" = team_A, "Player" = team_A5[as.numeric(theAction2[1]),]$Player, "Event" = "FT", "Points" = pt, stringsAsFactors = F)), envir = parent.frame()) # save event
      
      pt <- sample(c(0, 1), size = 1, prob = c(1-prb, prb))
      assign("playByPlay", rbind(playByPlay, data.frame("Team" = team_A, "Player" = team_A5[as.numeric(theAction2[1]),]$Player, "Event" = "FT", "Points" = pt, stringsAsFactors = F)), envir = parent.frame()) # save event
      if(pt == 0){
        
        rebound_ind <- sample(1:10, size = 1, prob = rebound_tbl$Rebounds)
        if(rebound_ind <= 5){rebound <- "homeRebound"}else{rebound <- "awayRebound"}
        assign("playByPlay", rbind(playByPlay, data.frame("Team" = rebound_tbl[rebound_ind,]$Tm, "Player" = rebound_tbl[rebound_ind,]$Player, "Event" = "Rebound", "Points" = 0, stringsAsFactors = F)), envir = parent.frame()) 
        
      }else{
        rebound <- "awayRebound" # tto end while loop
      }
    }
  }# end while
  
  if(printPlayByPlay == TRUE){print(playByPlay)}
  
}

#----------------------------------------------------------------------------------------------------
# testing it out
playByPlay <- data.frame("Team" = character(), "Player" = character(), "Event" = character(), "Points" = double(), stringsAsFactors = F) 
possession(team_A = "UTA", team_B = "GSW", printPlayByPlay = T, def_multiplier = 1)
possession(team_A = "GSW", team_B = "UTA", printPlayByPlay = T, def_multiplier = 1)

# looping it for 1 game
playByPlay <- data.frame("Team" = character(), "Player" = character(), "Event" = character(), "Points" = double(), stringsAsFactors = F) 
for(i in 1:100){
  possession(team_A = "UTA", team_B = "GSW", printPlayByPlay = F, def_multiplier = 1)
  possession(team_A = "GSW", team_B = "UTA", printPlayByPlay = F, def_multiplier = 1)
}
playByPlay
View(playByPlay)


#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Monte Carlo Simulation
home_team <- "LAC"
away_team <- "LAL"
nPossessions <- 100
nGames <- 10


home_team_scores <- NULL
away_team_scores <- NULL
for(j in 1:nGames){
  playByPlay <- data.frame("Team" = character(), "Player" = character(), "Event" = character(), "Points" = double(), stringsAsFactors = F) 
  for(i in 1:nPossessions){
    possession(team_A = home_team, team_B = away_team, printPlayByPlay = F, def_multiplier = 1)
    possession(team_A = away_team, team_B = home_team, printPlayByPlay = F, def_multiplier = 1)
  }
  home_team_scores <- c(home_team_scores, sum(playByPlay[playByPlay$Team == home_team,]$Points))
  away_team_scores <- c(away_team_scores, sum(playByPlay[playByPlay$Team == away_team,]$Points))
  
  print(paste0(j, "/", nGames))
}
data.frame(home = home_team_scores, away = away_team_scores)

#----------------------------------------------------------------------------------------------------
# other teams
home_team <- "UTA"
away_team <- "BOS"
nPossessions <- 100
nGames <- 10


home_team_scores <- NULL
away_team_scores <- NULL
for(j in 1:nGames){
  playByPlay <- data.frame("Team" = character(), "Player" = character(), "Event" = character(), "Points" = double(), stringsAsFactors = F) 
  for(i in 1:nPossessions){
    possession(team_A = home_team, team_B = away_team, printPlayByPlay = F, def_multiplier = 1)
    possession(team_A = away_team, team_B = home_team, printPlayByPlay = F, def_multiplier = 1)
  }
  home_team_scores <- c(home_team_scores, sum(playByPlay[playByPlay$Team == home_team,]$Points))
  away_team_scores <- c(away_team_scores, sum(playByPlay[playByPlay$Team == away_team,]$Points))
  
  print(paste0(j, "/", nGames))
}
dat <- data.frame(home = home_team_scores, away = away_team_scores)
mean(dat$home - dat$away) #UTA beats boston by .4 pts

#----------------------------------------------------------------------------------------------------
# other teams
home_team <- "MIL"
away_team <- "DET"
nPossessions <- 100
nGames <- 100

x <- Sys.time()
home_team_scores <- NULL
away_team_scores <- NULL
for(j in 1:nGames){
  playByPlay <- data.frame("Team" = character(), "Player" = character(), "Event" = character(), "Points" = double(), stringsAsFactors = F) 
  for(i in 1:nPossessions){
    possession(team_A = home_team, team_B = away_team, printPlayByPlay = F, def_multiplier = 1)
    possession(team_A = away_team, team_B = home_team, printPlayByPlay = F, def_multiplier = 1)
  }
  home_team_scores <- c(home_team_scores, sum(playByPlay[playByPlay$Team == home_team,]$Points))
  away_team_scores <- c(away_team_scores, sum(playByPlay[playByPlay$Team == away_team,]$Points))
  
  print(paste0(j, "/", nGames))
}
Sys.time() - x
dat <- data.frame(home = home_team_scores, away = away_team_scores)
mean(dat$home - dat$away) 






#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Monte Carlo Function
MC_games <- function(home_team, away_team, nPossessions = 100, nGames = 10, def_multiplier = 1){
  home_team_scores <- NULL
  away_team_scores <- NULL
  for(j in 1:nGames){
    playByPlay <- data.frame("Team" = character(), "Player" = character(), "Event" = character(), "Points" = double(), stringsAsFactors = F) 
    for(i in 1:nPossessions){
      possession(team_A = home_team, team_B = away_team, printPlayByPlay = F, def_multiplier = 1)
      possession(team_A = away_team, team_B = home_team, printPlayByPlay = F, def_multiplier = 1)
    }
    home_team_scores <- c(home_team_scores, sum(playByPlay[playByPlay$Team == home_team,]$Points))
    away_team_scores <- c(away_team_scores, sum(playByPlay[playByPlay$Team == away_team,]$Points))
    
    print(paste0(j, "/", nGames))
  }
  data.frame(home = home_team_scores, away = away_team_scores)
}

games1 <- MC_games(home_team = "LAC", away_team = "GSW", nPossessions = 100, nGames = 10, def_multiplier = 0)
games1


#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Monte Carlo Simulation 2.0
# saving the results a bit differently so we can see player stats over the games
home_team <- "LAC"
away_team <- "LAL"
nPossessions <- 100
nGames <- 10


home_team_scores <- NULL
away_team_scores <- NULL
playByPlay_overall <- data.frame("Team" = character(), "Player" = character(), "Event" = character(), "Points" = double(), stringsAsFactors = F, game = double()) 

for(j in 1:nGames){
  playByPlay <- data.frame("Team" = character(), "Player" = character(), "Event" = character(), "Points" = double(), stringsAsFactors = F) 
  for(i in 1:nPossessions){
    possession(team_A = home_team, team_B = away_team, printPlayByPlay = F, def_multiplier = 1)
    possession(team_A = away_team, team_B = home_team, printPlayByPlay = F, def_multiplier = 1)
  }
  playByPlay_overall <- rbind(playByPlay_overall, cbind(playByPlay, data.frame(game = j)))
  print(paste0(j, "/", nGames))
}
playByPlay_overall2 <- data.table(playByPlay_overall)
x <- playByPlay_overall2[,.(sum(Points)), by = c("Team", "game")] %>% dcast.data.table(game ~ Team)

x$spread <- x$LAL - x$LAC
x2 <- x[,win := ifelse(spread > 0, 1, 0)]


scores <- playByPlay_overall2[,.(sum(Points)), by = c("Team", "Player")]
scores$PPG <- scores$V1/10
p1 <- scores[Team == "LAL",] %>% ggplot(aes(x = Player, y = PPG)) + geom_col() + coord_flip() + theme_bw() + theme(axis.title = element_blank(), panel.border = element_blank())
p2 <- scores[Team == "LAC",] %>% ggplot(aes(x = Player, y = PPG)) + geom_col() + coord_flip() + theme_bw() + theme(axis.title = element_blank(), panel.border = element_blank())
p1 | p2



