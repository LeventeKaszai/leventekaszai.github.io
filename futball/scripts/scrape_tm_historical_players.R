library(worldfootballR)
library(dplyr)
library(DBI)
library(RSQLite)

# NB1 historikus jatekos-szintu keret-adat (piaci ertek, kor, pozicio, stb.),
# a scrape_tm.R-ben mar bevalt tm_player_market_values() hivassal, csak
# tobb korabbi szezonra ciklusban. Ugyanaz a szezon-tartomany, amit a
# scrape_tm_historical.R mar csapat-szinten lefed.
#
# FONTOS: a tm_rosters tablat NEM iraljuk felul (mint a scrape_tm.R teszi) --
# szezononkent DELETE + append, hogy a jelenlegi (2025-26, NB1+NB2) sorok
# erintetlenek maradjanak.

SEASONS <- c(2022, 2023, 2024, 2025)

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")

for (start_year in SEASONS) {
  cat("=== NB1", start_year, "/", start_year + 1 - 2000, "===\n")

  league_url <- paste0(
    "https://www.transfermarkt.com/nemzeti-bajnoksag/startseite/wettbewerb/UNG1/saison_id/",
    start_year
  )

  raw <- tryCatch(
    tm_player_market_values(start_year = start_year, league_url = league_url),
    error = function(e) { cat("  Hiba:", e$message, "\n"); NULL }
  )

  if (!is.null(raw) && nrow(raw) > 0) {
    raw <- mutate(raw, liga = "NB1", comp_name = "Nemzeti Bajnokság I",
                  country = "Hungary", season_start_year = start_year)

    cat("  ->", nrow(raw), "jatekos |", n_distinct(raw$squad), "csapat\n")

    dbExecute(con, "DELETE FROM tm_rosters WHERE liga = 'NB1' AND season_start_year = ?",
              params = list(start_year))
    dbWriteTable(con, "tm_rosters", raw, append = TRUE)
  } else {
    cat("  -> Nincs adat, kihagyva (a meglevo sorok erintetlenek maradnak)\n")
  }

  wait <- sample(30:50, 1)
  cat("  Varakozas", wait, "mp...\n\n")
  Sys.sleep(wait)
}

dbDisconnect(con)
cat("=== Kesz ===\n")
