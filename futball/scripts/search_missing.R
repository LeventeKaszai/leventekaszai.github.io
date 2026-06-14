library(DBI); library(RSQLite); library(dplyr); library(stringr); library(stringi)
normalize <- function(x) stri_trans_general(tolower(x), "Latin-ASCII")

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")
tm <- dbGetQuery(con, "SELECT DISTINCT player_name, player_url, team_name, nationality FROM tm_rosters")
dbDisconnect(con)

tm$last_norm <- normalize(word(tm$player_name, -1))
tm$full_norm <- normalize(tm$player_name)

search_terms <- c("Pinter","Kartik","Mesanovic","Mesanovic","Matanovic","Ubochioma","Slogar","Konyves","Pishchur","Novothny","Yordanov")
for (s in search_terms) {
  s_n <- normalize(s)
  cand <- tm[grepl(s_n, tm$full_norm, fixed=TRUE), ]
  cat(s, "->", if (nrow(cand) > 0) paste(cand$player_name, collapse=" | ") else "NINCS", "\n")
}
