library(DBI); library(RSQLite); library(dplyr); library(readr); library(stringr)

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")
sv_raw   <- dbGetQuery(con, "SELECT * FROM nb2_squad_values_historical_raw")
tab_raw  <- dbGetQuery(con, "SELECT * FROM nb2_tables_historical_raw")
pts2526  <- dbGetQuery(con, "SELECT team AS squad, value AS pts_total FROM team_stats WHERE liga='NB2' AND szezon='2025-26' AND kpi='Points'")
dbDisconnect(con)

# team_stats 'Points' mar meccsenkenti atlagkent van tarolva (ld. NB1-doku) -
# NB2-re ugyanaz a Wyscout-eredetu forras, ugyanugy mar per-match atlag.
pts2526$szezon <- "2025-26"

map <- read_csv("futball/data/team_name_mapping.csv", show_col_types = FALSE) |>
  distinct(tm_name, .keep_all = TRUE)

canonicalize <- function(df, col) {
  df |>
    left_join(map |> select(tm_name, canonical_name), by = setNames("tm_name", col)) |>
    mutate(team_canonical = coalesce(canonical_name, .data[[col]])) |>
    select(-canonical_name)
}

sv  <- canonicalize(sv_raw,  "squad") |> mutate(squad = str_trim(squad))
tab <- canonicalize(tab_raw, "squad") |> mutate(squad = str_trim(squad))

cat("=== Keretertek-oldal kanonizalas ===\n")
print(sv |> distinct(szezon, squad, team_canonical) |> arrange(szezon, team_canonical))

cat("\n=== Tabella-oldal kanonizalas ===\n")
print(tab |> distinct(szezon, squad, team_canonical) |> arrange(szezon, team_canonical))

# ── nb2_squad_values_historical (kanonikus, NB1-sema-egyenerteku) ───────────
squad_values_hist <- sv |>
  transmute(season_start_year, szezon, squad, squad_size, avg_age, foreigners,
            avg_market_value_euro, total_market_value_euro, team_canonical)

# ── pontszam-celvaltozo: TM tabella (22/23-24/25) + team_stats (25/26) ──────
tab_pts <- tab |>
  transmute(szezon, team = team_canonical, played, pts, pts_per_match = pts / played)

ts_pts <- pts2526 |>
  canonicalize("squad") |>
  transmute(szezon, team = team_canonical, pts_per_match = pts_total)

pts_all <- bind_rows(tab_pts |> select(szezon, team, pts_per_match), ts_pts)

cat("\n=== Pontszam-celvaltozo szezonon kent ===\n")
print(pts_all |> count(szezon))

# ── promoted flag: nem szerepelt az elozo szezon NB2 keretertek-listajan ───
# (lehet NB1-bol relegalt VAGY NB3-bol feljuto csapat is - mindket irany
# "uj NB2-ben", ugyanugy binaris flag, mint az NB1-modellben)
season_order <- c("2022-23", "2023-24", "2024-25", "2025-26", "2026-27")
teams_by_season <- split(squad_values_hist$team_canonical, squad_values_hist$szezon)

promoted_flag <- function(szezon, team) {
  idx <- match(szezon, season_order)
  if (idx == 1) return(0L)
  prev_teams <- teams_by_season[[season_order[idx - 1]]]
  if (is.null(prev_teams)) return(NA_integer_)
  as.integer(!team %in% prev_teams)
}

model_dataset <- squad_values_hist |>
  filter(szezon != "2026-27") |>  # nincs meg vegeredmenye
  distinct(szezon, team_canonical, .keep_all = TRUE) |>
  inner_join(pts_all, by = c("szezon", "team_canonical" = "team")) |>
  rowwise() |>
  mutate(promoted = promoted_flag(szezon, team_canonical)) |>
  ungroup() |>
  transmute(
    szezon, team = team_canonical, total_market_value_euro, avg_age,
    squad_size, foreigners, pts_per_match, promoted
  )

cat("\n=== nb2_model_dataset ===\n")
print(model_dataset |> count(szezon))
cat("N sorok osszesen:", nrow(model_dataset), "\n")

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")
dbWriteTable(con, "nb2_squad_values_historical", squad_values_hist, overwrite = TRUE)
dbWriteTable(con, "nb2_model_dataset", model_dataset, overwrite = TRUE)
dbDisconnect(con)

cat("\nnb2_squad_values_historical es nb2_model_dataset mentve.\n")
