# Scraping data to use in MC simulations

# https://www.basketball-reference.com/leagues/NBA_2020_totals.html
# you can change the year to get historical stats

# libraries
libraries <- c("XML", "rvest", "dplyr", "data.table")
sapply(libraries, require, character.only = T)




# Scraping the regular player data
url <- "https://www.basketball-reference.com/leagues/NBA_2020_totals.html"
css <- "td" # other CSS options: "#div_totals_stats", ".right , .left , .center"
stats <- rvest::html_text(rvest::html_nodes(xml2::read_html(url), css))

# each df should have these 29 fields:
colHeaders <- c("Player", "Pos", "Age", "Tm", "G", "GS", "MP", "FG", "FGA", "FG%", "3P", "3PA", "3P%", "2P", "2PA", "2P%", "eFG%", "FT", "FTA", "FT%", "ORB", "DRB", "TRB",	"AST", "STL", "BLK", "TOV", "PF", "PTS")

# Every 29 elements in the large "stats" vector is a new row, so looping through to convert to data.frame format
df <- cbind(stats[seq(1, 600*29, by = 29)], stats[seq(2, 600*29, by = 29)])
for(i in 3:29){
  df <- cbind(df, stats[seq(i, 600*29, by = 29)])
}
df <- data.frame(df)
names(df) <- colHeaders

# cleaning up the data.frame
df <- df[!is.na(df$Player),] # deleting extra rows
# changing column types
df[colHeaders[1:29]] <- sapply(df[colHeaders[1:29]], as.character)
df[df == ""] <- "0" # replacing missing values w/ 0's
df[colHeaders[c(3, 5:29)]] <- sapply(df[colHeaders[c(3, 5:29)]], as.numeric)
totals <- df



# now scraping the advanced table
# Scraping the data
url <- "https://www.basketball-reference.com/leagues/NBA_2020_advanced.html"
css <- "td" # other CSS options: "#div_totals_stats", ".right , .left , .center"
stats <- rvest::html_text(rvest::html_nodes(xml2::read_html(url), css))

# there are 2 columns with blank data in the dataset
colHeaders <- c("Player", "Pos", "Age", "Tm", "G", 'MP', "PER", "TS%", "3PAr", "FTr", "ORB%", "DRB%", "TRB%", "AST%", "STL%", "BLK%", "TOV%", "USG%", "blank1", "OWS", "DWS", "WS", "WS/48", "blank2", "OBPM", "DBPM", "BPM", "VORP")

# new player every 28 records
df <- cbind(stats[seq(1, 600*28, by = 28)], stats[seq(2, 600*28, by = 28)])
for(i in 3:28){
  df <- cbind(df, stats[seq(i, 600*28, by = 28)])
}
df <- data.frame(df)
names(df) <- colHeaders

# data clean
df <- df[!is.na(df$Player),] # deleting extra rows
df[colHeaders[1:28]] <- sapply(df[colHeaders[c(1:28)]], as.character)
df[df == ""] <- "0" # replacing missing values w/ 0's
df[colHeaders[c(3, 5:28)]] <- sapply(df[colHeaders[c(3, 5:28)]], as.numeric)
advanced <- df

#----------------------------------------------------------------------------------------------------
# joining the two tables
players_all <- left_join(totals, select(advanced, Player, Tm, PER, `TS%`, DWS), by = c("Player", "Tm"))

write.csv(players_all, "/Users/tj/Documents/Data/NBA/PlayerStats_2020.03.10.csv", row.names = FALSE)

