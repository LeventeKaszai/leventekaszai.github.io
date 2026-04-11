library(DBI); library(RSQLite); library(dplyr); library(stringr); library(stringi)

normalize <- function(x) stri_trans_general(tolower(x), "Latin-ASCII")

con    <- dbConnect(RSQLite::SQLite(), "futball/futball.db")
tm     <- dbGetQuery(con, "SELECT DISTINCT player_name, player_url, team_name, nationality FROM tm_rosters")
twelve <- dbGetQuery(con, "SELECT DISTINCT player_id, label FROM player_stats ORDER BY label")

# Build normalized labels for TM
tm <- tm |> mutate(
  first         = word(player_name, 1),
  last          = word(player_name, -1),
  tm_label      = paste0(str_sub(first, 1, 1), ". ", last),
  tm_label_norm = normalize(tm_label),
  last_norm     = normalize(last)
)

twelve <- twelve |> mutate(
  label_norm = normalize(label),
  last_norm  = normalize(word(label, -1))
)

# Round 1: exact normalized match
auto_match_raw <- twelve |>
  left_join(
    tm |> select(tm_label_norm, player_name, player_url, team_name, nationality),
    by = c("label_norm" = "tm_label_norm")
  ) |>
  filter(!is.na(player_name)) |>
  select(player_id, label, player_name, player_url, team_name, nationality) |>
  mutate(match_type = "auto")

# Deduplicate: same player_id + same player_name (transfer case) → keep first
# Different player_name with same player_id → flag as ambiguous (remove from auto)
auto_match <- auto_match_raw |>
  group_by(player_id) |>
  mutate(n_names = n_distinct(player_name)) |>
  ungroup() |>
  filter(n_names == 1) |>           # keep only unambiguous matches
  distinct(player_id, .keep_all = TRUE) |>
  select(-n_names)

# Ambiguous auto-matches (different people with same label format) → put in unmatched
ambiguous_ids <- auto_match_raw |>
  group_by(player_id) |>
  filter(n_distinct(player_name) > 1) |>
  pull(player_id) |> unique()

# IDs still needing a match
unmatched1_ids <- unique(c(
  setdiff(twelve$player_id, auto_match$player_id),
  ambiguous_ids
))

still_need <- twelve |> filter(player_id %in% unmatched1_ids)

# Round 2a: full normalized name match (for "Daniel Lima", "Iuri Medeiros" etc.)
tm_fullname <- tm |> mutate(full_norm = normalize(player_name)) |>
  select(full_norm, player_name, player_url, team_name, nationality)

r2a <- still_need |>
  left_join(tm_fullname, by = c("label_norm" = "full_norm")) |>
  filter(!is.na(player_name)) |>
  group_by(player_id) |> filter(n_distinct(player_name) == 1) |>
  slice(1) |> ungroup() |>
  select(player_id, label, player_name, player_url, team_name, nationality) |>
  mutate(match_type = "auto_r2a")

still_need2 <- still_need |> filter(!player_id %in% r2a$player_id)

# Round 2b: last-name substring (for "Y. Croizet" → "Yohan Croizet-Kollár", "Alegria" → "Maxsuell Alegria")
# Deduplicate TM by player_url so same player on two teams counts as one candidate
tm_last <- tm |>
  distinct(player_url, .keep_all = TRUE) |>
  select(last_norm, player_name, player_url, team_name, nationality)

r2b_list <- lapply(seq_len(nrow(still_need2)), function(i) {
  ln  <- still_need2$last_norm[i]
  pid <- still_need2$player_id[i]
  lbl <- still_need2$label[i]
  cand <- tm_last[grepl(ln, tm_last$last_norm, fixed = TRUE), ]
  if (nrow(cand) == 1) {
    data.frame(player_id = pid, label = lbl, player_name = cand$player_name,
               player_url = cand$player_url, team_name = cand$team_name,
               nationality = cand$nationality, match_type = "auto_r2b",
               stringsAsFactors = FALSE)
  } else NULL
})
r2b <- do.call(rbind, Filter(Negate(is.null), r2b_list))

# Combine all auto rounds
if (!is.null(r2b) && nrow(r2b) > 0) {
  auto_all <- bind_rows(auto_match, r2a, r2b)
} else {
  auto_all <- bind_rows(auto_match, r2a)
}

# Unmatched after round 1 (including ambiguous)
unmatched_ids <- setdiff(twelve$player_id, auto_all$player_id)

# Manual overrides (fill these in manual_mapping.csv)
manual_raw <- read.csv("futball/manual_mapping.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
manual <- manual_raw |>
  filter(!is.na(player_name) & player_name != "") |>
  mutate(match_type = "manual",
         nationality = NA_character_) |>
  select(player_id, label, player_name, player_url, team_name, nationality, match_type)

# Combine — only bind manual if non-empty
if (nrow(manual) > 0) {
  player_map <- bind_rows(auto_all, manual) |>
    distinct(player_id, .keep_all = TRUE)
} else {
  player_map <- auto_all
}

# Add remaining unmatched as NA rows
still_unmatched <- twelve |>
  filter(player_id %in% setdiff(unmatched_ids, manual$player_id)) |>
  mutate(player_name = NA_character_, player_url = NA_character_,
         team_name = NA_character_, nationality = NA_character_,
         match_type = "unmatched") |>
  select(player_id, label, player_name, player_url, team_name, nationality, match_type)

player_map <- bind_rows(player_map, still_unmatched) |>
  arrange(label)

dbWriteTable(con, "player_map", player_map, overwrite = TRUE)
dbDisconnect(con)

cat("player_map mentve:", nrow(player_map), "sor\n")
cat("Auto match:  ", sum(player_map$match_type == "auto"), "\n")
cat("Manual:      ", sum(player_map$match_type == "manual"), "\n")
cat("Unmatched:   ", sum(player_map$match_type == "unmatched"), "\n")
