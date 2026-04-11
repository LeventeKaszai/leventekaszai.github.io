library(DBI); library(RSQLite); library(dplyr); library(stringr); library(stringi)

normalize <- function(x) stri_trans_general(tolower(x), "Latin-ASCII")

con    <- dbConnect(RSQLite::SQLite(), "futball/futball.db")
tm     <- dbGetQuery(con, "SELECT DISTINCT player_name, player_url, team_name, nationality FROM tm_rosters")
twelve <- dbGetQuery(con, "SELECT DISTINCT player_id, label FROM player_stats ORDER BY label")
dbDisconnect(con)

# TM nevekből Twelve-stílusú label (normalizálva)
tm <- tm |> mutate(
  first    = word(player_name, 1),
  last     = word(player_name, -1),
  tm_label = paste0(str_sub(first, 1, 1), ". ", last),
  tm_label_norm = normalize(tm_label),
  last_norm     = normalize(last)
)

twelve <- twelve |> mutate(
  label_norm = normalize(label),
  last_norm  = normalize(word(label, -1))
)

# 1. kör: pontos match (normalizálva)
join1 <- twelve |>
  left_join(tm |> select(tm_label_norm, player_name, player_url, team_name, nationality),
            by = c("label_norm" = "tm_label_norm"))

matched   <- join1 |> filter(!is.na(player_name))
unmatched <- join1 |> filter(is.na(player_name))

cat("1. kör (normalizált) match:", nrow(matched), "/", nrow(twelve), "\n\n")

# 2. kör: csak vezetéknév alapján (normalizálva)
cat("Unmatchelt - legközelebbi TM nevek:\n")
for (i in seq_len(nrow(unmatched))) {
  lbl  <- unmatched$label[i]
  ln   <- unmatched$last_norm[i]
  cand <- tm |> filter(str_detect(last_norm, fixed(ln)))
  if (nrow(cand) > 0) {
    cat(" ", lbl, "->", paste(cand$player_name, collapse = " | "), "\n")
  } else {
    cat(" ", lbl, "-> NINCS TALALAT\n")
  }
}
