library(worldfootballR)
library(dplyr)
library(DBI)
library(RSQLite)

# NB1 2025/26 csapat URL-ek (saison_id=2025 = 2025/26 szezon)
# Kiesett: Kecskeméti TE, Fehérvár FC
# Feljutott: Kisvárda FC, Kazincbarcikai SC
team_urls_2025 <- c(
  "https://www.transfermarkt.com/ferencvarosi-tc/startseite/verein/35/saison_id/2025",
  "https://www.transfermarkt.com/debreceni-vsc/startseite/verein/11287/saison_id/2025",
  "https://www.transfermarkt.com/diosgyori-vtk/startseite/verein/10929/saison_id/2025",
  "https://www.transfermarkt.com/gyor-eto-fc/startseite/verein/14867/saison_id/2025",
  "https://www.transfermarkt.com/kazincbarcikai-sc/startseite/verein/24031/saison_id/2025",
  "https://www.transfermarkt.com/kisvarda-fc/startseite/verein/30613/saison_id/2025",
  "https://www.transfermarkt.com/mtk-budapest-fc/startseite/verein/11289/saison_id/2025",
  "https://www.transfermarkt.com/nyiregyhaza-spartacus/startseite/verein/105742/saison_id/2025",
  "https://www.transfermarkt.com/paksi-fc/startseite/verein/12480/saison_id/2025",
  "https://www.transfermarkt.com/puskas-akademia-fc/startseite/verein/75406/saison_id/2025",
  "https://www.transfermarkt.com/ujpest-fc/startseite/verein/11288/saison_id/2025",
  "https://www.transfermarkt.com/zalaegerszegi-te-fc/startseite/verein/11286/saison_id/2025"
)

# Csapatonként scrape retry logikával
scrape_team <- function(url, retries = 3, wait = 10) {
  team_name <- sub(".*/startseite/verein/\\d+/saison_id/\\d+", "", url)
  for (i in seq_len(retries)) {
    cat("  Próba", i, ":", basename(dirname(dirname(dirname(dirname(url))))), "\n")
    result <- tryCatch(
      tm_squad_stats(team_url = url),
      error = function(e) { cat("  Hiba:", e$message, "\n"); NULL }
    )
    if (!is.null(result) && nrow(result) > 0) return(result)
    if (i < retries) { cat("  Újrapróbálás", wait, "mp után...\n"); Sys.sleep(wait) }
  }
  cat("  SIKERTELEN:", url, "\n")
  return(NULL)
}

cat("NB1 2025/26 keretek lekérése...\n")
all_rosters <- list()
for (url in team_urls_2025) {
  cat("\n[", which(team_urls_2025 == url), "/", length(team_urls_2025), "]", url, "\n")
  res <- scrape_team(url, retries = 3, wait = 15)
  if (!is.null(res)) all_rosters[[url]] <- res
  wait_sec <- sample(90:150, 1)
  cat("  Várakozás", wait_sec, "mp...\n")
  Sys.sleep(wait_sec)
}

if (length(all_rosters) == 0) {
  cat("\nNem sikerült csatlakozni a TM-hez. Próbáld meg később.\n")
  quit(status = 1)
}

rosters <- bind_rows(all_rosters)
cat("\nSikeresen lekért csapatok:", length(all_rosters), "/", length(team_urls_2025), "\n")
cat("Összes játékos:", nrow(rosters), "\n")

# Mentés DB-be — csak az új csapatokat írjuk, a meglévőket nem töröljük
con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")

existing_teams <- if (dbExistsTable(con, "tm_rosters")) {
  dbGetQuery(con, "SELECT DISTINCT team_name FROM tm_rosters")$team_name
} else character(0)

new_rosters <- rosters |> filter(!team_name %in% existing_teams)

if (nrow(new_rosters) > 0) {
  dbWriteTable(con, "tm_rosters", new_rosters, append = TRUE)
  cat("Új csapatok hozzáadva:", nrow(new_rosters), "játékos\n")
} else {
  cat("Nincs új csapat — minden adat már bent van.\n")
}

dbDisconnect(con)
cat("Kész! tm_rosters tábla frissítve 2025/26-os adatokkal.\n")
