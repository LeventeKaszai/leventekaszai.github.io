library(worldfootballR)
library(dplyr)
library(DBI)
library(RSQLite)

# NB1 2026/27 keretek (nyári átigazolások után frissült TM-adatok).
# Csak az NB1 sorokat cseréli le a tm_rosters táblában — NB2 érintetlen marad.

cat("=== NB1 2026/27 ===\n")
nb1_raw <- tm_player_market_values(
  start_year = 2026,
  league_url = "https://www.transfermarkt.com/nemzeti-bajnoksag/startseite/wettbewerb/UNG1/saison_id/2026"
)

nb1_raw <- mutate(nb1_raw, liga = "NB1", comp_name = "Nemzeti Bajnokság I",
                   country = "Hungary", season_start_year = 2026L)

cat("NB1 játékosok:", nrow(nb1_raw), "| csapatok:", n_distinct(nb1_raw$squad), "\n")
print(as.data.frame(nb1_raw |> count(squad) |> arrange(squad)))

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")

dbExecute(con, "DELETE FROM tm_rosters WHERE liga = 'NB1'")
dbWriteTable(con, "tm_rosters", nb1_raw, append = TRUE)

cat("\n=== Kész ===\n")
cat("tm_rosters NB1 sorok lecserélve 2026/27-es adatokra:", nrow(nb1_raw), "\n")
print(dbGetQuery(con, "SELECT liga, season_start_year, COUNT(*) n, COUNT(DISTINCT squad) teams FROM tm_rosters GROUP BY liga, season_start_year"))

dbDisconnect(con)
