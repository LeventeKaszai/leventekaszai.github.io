# Football bulk migráció: futball.db (SQLite) -> Supabase Postgres
# (identity / football / football_lab sémák)
#
# Futtatás:
#   Rscript futball/migration/migrate_football.R            # dry-run (nem ír)
#   Rscript futball/migration/migrate_football.R --live      # éles UPSERT írás
#
# Az eredeti futball.db-t csak olvassuk, nem módosítjuk (elv #7).
# A build_db*.R szkriptek változatlanok maradnak (elv #8).

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(RPostgres)
  library(dplyr); library(tidyr); library(stringr); library(digest); library(purrr)
})

DRY_RUN <- !("--live" %in% commandArgs(trailingOnly = TRUE))
cat(if (DRY_RUN) "=== DRY RUN (nincs éles írás) ===\n\n" else "=== ÉLES ÍRÁS ===\n\n")

readRenviron(".Renviron")
sq <- dbConnect(RSQLite::SQLite(), "futball/futball.db")
pg <- dbConnect(Postgres(),
  host = Sys.getenv("SUPABASE_HOST"), port = as.integer(Sys.getenv("SUPABASE_PORT")),
  dbname = Sys.getenv("SUPABASE_DB"), user = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PASSWORD")
)

first_non_na <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA else x[1] }

# Determinisztikus UUID (v5-szerű, SHA1-alapú), hogy az azonosság-felbontás
# rerun esetén is ugyanazt a player_uid-ot adja -- ez kell az idempotens
# UPSERT-hez (elv #6), mert egy véletlen UUIDgenerate() minden futtatáskor
# új sort hozna létre ugyanahhoz a játékoshoz.
deterministic_uuid <- function(key) {
  h <- substr(digest(paste0("football-player:", key), algo = "sha1", serialize = FALSE), 1, 32)
  tolower(paste0(substr(h,1,8), "-", substr(h,9,12), "-5", substr(h,14,16), "-a", substr(h,18,20), "-", substr(h,21,32)))
}

# ═══════════════════════════════════════════════════════════════════════
# 1. IDENTITY RESOLUTION
# ═══════════════════════════════════════════════════════════════════════

player_map <- dbGetQuery(sq, "SELECT * FROM player_map")

# Az auto_r2b kör (csak vezetéknév-részlet egyezés) empirikusan megbízhatatlannak
# bizonyult: 2/2 ellenőrzött esetben (Duarte-, López-pár) más valódi személyhez
# rendelt TM-profilt -- valószínűleg azért, mert a round 1 már elvitte a helyes
# candidate-ot egy másik Twelve-játékoshoz, és a maradék substring-match tévesen
# a foglalt profilra mutatott. Ezért minden auto_r2b matchet visszaminősítünk
# unmatched-re; kézi review a manual_mapping.csv-n keresztül később.
player_map <- player_map |>
  mutate(
    player_name = if_else(match_type == "auto_r2b", NA_character_, player_name),
    player_url  = if_else(match_type == "auto_r2b", NA_character_, player_url),
    team_name   = if_else(match_type == "auto_r2b", NA_character_, team_name),
    nationality = if_else(match_type == "auto_r2b", NA_character_, nationality),
    match_type  = if_else(match_type == "auto_r2b", "unmatched", match_type)
  )

tm_summary <- dbGetQuery(sq, "
  SELECT player_url, player_dob, player_nationality, squad, player_position, season_start_year
  FROM tm_rosters WHERE player_url IS NOT NULL AND player_url != ''
") |>
  mutate(birth_date = as.Date(player_dob, origin = "1970-01-01")) |>
  arrange(player_url, desc(season_start_year)) |>
  group_by(player_url) |>
  summarise(
    birth_date       = first_non_na(birth_date),
    nationality_tm   = first_non_na(player_nationality),
    current_team_tm  = first_non_na(squad),
    position_tm      = first_non_na(player_position),
    .groups = "drop"
  )

twelve_position <- dbGetQuery(sq, "SELECT player_id, position, szezon FROM player_stats") |>
  arrange(player_id, desc(szezon)) |>
  distinct(player_id, .keep_all = TRUE) |>
  select(player_id, position_twelve = position)

identity_map <- player_map |>
  left_join(tm_summary, by = "player_url") |>
  mutate(
    player_uid     = map_chr(player_id, ~ deterministic_uuid(.x)),
    canonical_name = coalesce(player_name, label),
    primary_sport  = "football"
  )

player_identity <- identity_map |>
  distinct(player_uid, canonical_name, birth_date, primary_sport)

source_map_twelve <- identity_map |>
  transmute(sport = "football", source_system = "twelve",
            source_player_id = as.character(player_id),
            player_uid, match_confidence = match_type)

source_map_tm <- identity_map |>
  filter(!is.na(player_url) & player_url != "") |>
  transmute(sport = "football", source_system = "transfermarkt",
            source_player_id = player_url,
            player_uid, match_confidence = match_type) |>
  distinct(source_player_id, .keep_all = TRUE)

player_identity_source_map <- bind_rows(source_map_twelve, source_map_tm)

# ═══════════════════════════════════════════════════════════════════════
# 2. FOOTBALL (public)
# ═══════════════════════════════════════════════════════════════════════

player_profiles <- identity_map |>
  left_join(twelve_position, by = "player_id") |>
  transmute(
    player_uid,
    display_name      = canonical_name,
    position          = coalesce(position_twelve, position_tm),
    current_team      = coalesce(team_name, current_team_tm),
    nationality       = coalesce(nationality, nationality_tm),
    birth_date,
    transfermarkt_url = player_url
  ) |>
  distinct(player_uid, .keep_all = TRUE)

player_uid_by_twelve_id <- identity_map |> distinct(player_id, player_uid)

# FONTOS: a position mezőt is meg kell tartani -- egy játékosnak több pozícióján
# is lehet Twelve-adata ugyanabban a szezonban (pl. CB-ként ÉS FB-ként mérve),
# és a rank/rank_of pozíció-relatív rangsor. Korábban a position nélküli
# distinct(player_uid, season, league, quality, kpi) csendben eldobta az egyik
# pozíció sorait ütközés esetén -- ez a teljes adat kb. 16%-át érintette.
player_season_stats <- dbGetQuery(sq, "
  SELECT player_id, szezon, liga, position, quality, kpi, value_per90, rank, rank_of FROM player_stats_long
") |>
  inner_join(player_uid_by_twelve_id, by = "player_id") |>
  filter(!is.na(quality) & !is.na(kpi) & !is.na(position)) |>
  transmute(player_uid, season = szezon, league = liga, position, quality, kpi, value_per90, rank, rank_of) |>
  distinct(player_uid, season, league, position, quality, kpi, .keep_all = TRUE)

team_season_stats <- dbGetQuery(sq, "SELECT * FROM team_stats") |>
  filter(!is.na(quality) & !is.na(kpi)) |>
  transmute(team_name = team, season = szezon, league = liga, quality, kpi, value, rank, rank_of) |>
  distinct(team_name, season, league, quality, kpi, .keep_all = TRUE)

matches_out <- dbGetQuery(sq, "SELECT * FROM matches") |>
  transmute(match_id, match_date = as.Date(date), season = szezon, league = liga,
            home_team, away_team, home_goals, away_goals)

# ═══════════════════════════════════════════════════════════════════════
# 3. FOOTBALL_LAB (private)
# ═══════════════════════════════════════════════════════════════════════

tm_rosters_out <- dbGetQuery(sq, "SELECT * FROM tm_rosters") |>
  mutate(player_dob = as.Date(player_dob, origin = "1970-01-01"))

coach_changes_out <- dbGetQuery(sq, "SELECT * FROM coach_changes") |>
  mutate(match_before_date = as.Date(match_before_date), match_after_date = as.Date(match_after_date))

# Egy spell "censored" (nem megerősített távozás), ha a vég-dátuma nem
# szerepel coach_changes-ben out_coach/match_before_date párként -- ilyenkor
# a dátum csak az adatgyűjtés jelenlegi határa, nem tényleges edzőváltás.
confirmed_ends <- coach_changes_out |>
  transmute(team, coach = out_coach, spell_end_date = match_before_date, confirmed = TRUE)

coach_spells_out <- dbGetQuery(sq, "SELECT * FROM coach_spells") |>
  mutate(spell_start_date = as.Date(spell_start_date), spell_end_date = as.Date(spell_end_date)) |>
  left_join(confirmed_ends, by = c("team", "coach", "spell_end_date")) |>
  mutate(is_censored_end = is.na(confirmed)) |>
  select(-confirmed)

coach_adjusted_ratings_out <- dbGetQuery(sq, "SELECT * FROM coach_adjusted_ratings") |>
  rename(xt_adj_diff = `xT_adj_diff`, xt_adj_se = `xT_adj_se`,
         npxg_adj_diff = `npxG_adj_diff`, npxg_adj_se = `npxG_adj_se`,
         adj_xt = `adj_xT`, adj_oppxt = `adj_oppxT`, adj_npxg = `adj_npxG`, adj_oppnpxg = `adj_oppnpxG`)

team_finances_out <- dbGetQuery(sq, "SELECT * FROM team_finances") |>
  rename(season = szezon_std, season_raw = szezon) |>
  filter(!is.na(season) & season != "")

team_finances_season_out <- dbGetQuery(sq, "SELECT * FROM team_finances_season") |>
  rename(season = szezon)

nb1_model_dataset_out <- dbGetQuery(sq, "SELECT * FROM nb1_model_dataset") |> mutate(promoted = as.logical(promoted))
nb2_model_dataset_out <- dbGetQuery(sq, "SELECT * FROM nb2_model_dataset") |> mutate(promoted = as.logical(promoted))

nb1_squad_values_historical_out     <- dbGetQuery(sq, "SELECT * FROM nb1_squad_values_historical")
nb2_squad_values_historical_out     <- dbGetQuery(sq, "SELECT * FROM nb2_squad_values_historical")
nb2_squad_values_historical_raw_out <- dbGetQuery(sq, "SELECT * FROM nb2_squad_values_historical_raw")

nb1_final_tables_historical_out <- dbGetQuery(sq, "SELECT * FROM nb1_final_tables_historical") |>
  rename_with(tolower)

nb2_tables_historical_raw_out <- dbGetQuery(sq, "SELECT * FROM nb2_tables_historical_raw")

nb1_2627_goal_estimates_out      <- dbGetQuery(sq, "SELECT * FROM nb1_2627_goal_estimates") |> mutate(generalva = as.Date(generalva))
nb1_2627_poisson_params_out      <- dbGetQuery(sq, "SELECT * FROM nb1_2627_poisson_params") |> mutate(generalva = as.Date(generalva))
nb1_2627_preseason_forecast_out  <- dbGetQuery(sq, "SELECT * FROM nb1_2627_preseason_forecast") |> mutate(generalva = as.Date(generalva))
nb1_2627_round1_ah_odds_out      <- dbGetQuery(sq, "SELECT * FROM nb1_2627_round1_ah_odds") |> mutate(generalva = as.Date(generalva))
nb1_2627_round1_poisson_odds_out <- dbGetQuery(sq, "SELECT * FROM nb1_2627_round1_poisson_odds") |> mutate(generalva = as.Date(generalva))
nb2_2627_preseason_forecast_out  <- dbGetQuery(sq, "SELECT * FROM nb2_2627_preseason_forecast") |> mutate(generalva = as.Date(generalva))

value_model_coefs_out <- dbGetQuery(sq, "SELECT * FROM value_model_coefs")

value_model_residuals_out <- dbGetQuery(sq, "SELECT * FROM value_model_residuals") |>
  left_join(player_uid_by_twelve_id, by = "player_id") |>
  select(-player_id)

match_team_stats_long_out <- dbGetQuery(sq, "SELECT * FROM match_team_stats_long")

# ═══════════════════════════════════════════════════════════════════════
# 4. VALIDÁCIÓ
# ═══════════════════════════════════════════════════════════════════════

sqlite_counts <- dbGetQuery(sq, "SELECT name FROM sqlite_master WHERE type='table'")$name |>
  set_names() |> map_int(~ dbGetQuery(sq, sprintf("SELECT COUNT(*) n FROM `%s`", .x))$n)

targets <- list(
  `identity.player_identity` = player_identity,
  `identity.player_identity_source_map` = player_identity_source_map,
  `football.matches` = matches_out,
  `football.player_profiles` = player_profiles,
  `football.player_season_stats` = player_season_stats,
  `football.team_season_stats` = team_season_stats,
  `football_lab.tm_rosters` = tm_rosters_out,
  `football_lab.coach_spells` = coach_spells_out,
  `football_lab.coach_changes` = coach_changes_out,
  `football_lab.coach_adjusted_ratings` = coach_adjusted_ratings_out,
  `football_lab.team_finances` = team_finances_out,
  `football_lab.team_finances_season` = team_finances_season_out,
  `football_lab.nb1_model_dataset` = nb1_model_dataset_out,
  `football_lab.nb2_model_dataset` = nb2_model_dataset_out,
  `football_lab.nb1_squad_values_historical` = nb1_squad_values_historical_out,
  `football_lab.nb2_squad_values_historical` = nb2_squad_values_historical_out,
  `football_lab.nb2_squad_values_historical_raw` = nb2_squad_values_historical_raw_out,
  `football_lab.nb1_final_tables_historical` = nb1_final_tables_historical_out,
  `football_lab.nb2_tables_historical_raw` = nb2_tables_historical_raw_out,
  `football_lab.nb1_2627_goal_estimates` = nb1_2627_goal_estimates_out,
  `football_lab.nb1_2627_poisson_params` = nb1_2627_poisson_params_out,
  `football_lab.nb1_2627_preseason_forecast` = nb1_2627_preseason_forecast_out,
  `football_lab.nb1_2627_round1_ah_odds` = nb1_2627_round1_ah_odds_out,
  `football_lab.nb1_2627_round1_poisson_odds` = nb1_2627_round1_poisson_odds_out,
  `football_lab.nb2_2627_preseason_forecast` = nb2_2627_preseason_forecast_out,
  `football_lab.value_model_coefs` = value_model_coefs_out,
  `football_lab.value_model_residuals` = value_model_residuals_out,
  `football_lab.match_team_stats_long` = match_team_stats_long_out
)

cat("--- Sor-szám validáció (cél tábla vs forrás) ---\n")
for (nm in names(targets)) {
  cat(sprintf("%-45s %6d sor\n", nm, nrow(targets[[nm]])))
}

cat("\n--- Identity felbontás összegzés ---\n")
cat("player_map sorok:            ", nrow(player_map), "\n")
cat("Ebből egyedi player_uid:     ", n_distinct(player_identity$player_uid), "\n")
print(count(identity_map, match_type, sort = TRUE))

cat("\n--- Ütköző vezetéknevek player_profiles-ban (manuális ellenőrzésre) ---\n")
print(player_profiles |> count(display_name, sort = TRUE) |> filter(n > 1))

cat("\n--- coach_spells cenzúrázott vég-dátumok ---\n")
print(coach_spells_out |> count(is_censored_end))

cat("\n--- player_dob konverzió spot-check (5 minta) ---\n")
print(tm_rosters_out |> filter(!is.na(player_dob)) |> select(player_name, player_dob, player_age) |> head(5))

if (DRY_RUN) {
  cat("\n=== DRY RUN VÉGE -- nem történt írás. Futtasd --live kapcsolóval az éles UPSERT-hez. ===\n")
  dbDisconnect(sq); dbDisconnect(pg)
  quit(save = "no", status = 0)
}

# ═══════════════════════════════════════════════════════════════════════
# 5. ÉLES ÍRÁS
# ═══════════════════════════════════════════════════════════════════════
# Elv #6 szerint két minta:
#  - identity + football (public) táblák: UPSERT (ON CONFLICT DO UPDATE),
#    definiált konfliktus-kulccsal -- ezek sor-szinten inkrementálisan bővülnek.
#  - football_lab táblák: a forrás build_db*.R szkriptek maguk is teljes
#    táblát írnak felül (dbWriteTable(..., overwrite=TRUE) mintát követve),
#    így itt is truncate+reload az idempotens megoldás, nem soronkénti UPSERT.

upsert <- function(con, schema, table, df, conflict_cols) {
  if (nrow(df) == 0) return(invisible())
  tmp <- paste0("_stage_", table)
  dbWriteTable(con, tmp, df, temporary = TRUE, overwrite = TRUE)
  cols <- names(df)
  update_cols <- setdiff(cols, conflict_cols)
  set_clause <- paste(sprintf('"%s" = EXCLUDED."%s"', update_cols, update_cols), collapse = ", ")
  # a stage tábla text-ként hordozza a UUID oszlopokat -- explicit cast a SELECT-ben
  select_expr <- ifelse(cols == "player_uid", '"player_uid"::uuid', sprintf('"%s"', cols))
  sql <- sprintf(
    'INSERT INTO %s.%s (%s) SELECT %s FROM %s ON CONFLICT (%s) DO UPDATE SET %s',
    schema, table,
    paste(sprintf('"%s"', cols), collapse = ", "),
    paste(select_expr, collapse = ", "),
    tmp,
    paste(sprintf('"%s"', conflict_cols), collapse = ", "),
    set_clause
  )
  dbExecute(con, sql)
}

truncate_reload <- function(con, schema, table, df) {
  dbExecute(con, sprintf("TRUNCATE TABLE %s.%s", schema, table))
  if (nrow(df) > 0) dbAppendTable(con, Id(schema = schema, table = table), df)
}

dbBegin(pg)
tryCatch({
  upsert(pg, "identity", "player_identity", player_identity, "player_uid")
  upsert(pg, "identity", "player_identity_source_map", player_identity_source_map, c("sport","source_system","source_player_id"))
  upsert(pg, "football", "matches", matches_out, "match_id")
  upsert(pg, "football", "player_profiles", player_profiles, "player_uid")
  upsert(pg, "football", "player_season_stats", player_season_stats, c("player_uid","season","league","position","quality","kpi"))
  upsert(pg, "football", "team_season_stats", team_season_stats, c("team_name","season","league","quality","kpi"))

  truncate_reload(pg, "football_lab", "tm_rosters", tm_rosters_out)
  truncate_reload(pg, "football_lab", "coach_spells", coach_spells_out)
  truncate_reload(pg, "football_lab", "coach_changes", coach_changes_out)
  truncate_reload(pg, "football_lab", "coach_adjusted_ratings", coach_adjusted_ratings_out)
  truncate_reload(pg, "football_lab", "team_finances", team_finances_out)
  truncate_reload(pg, "football_lab", "team_finances_season", team_finances_season_out)
  truncate_reload(pg, "football_lab", "nb1_model_dataset", nb1_model_dataset_out)
  truncate_reload(pg, "football_lab", "nb2_model_dataset", nb2_model_dataset_out)
  truncate_reload(pg, "football_lab", "nb1_squad_values_historical", nb1_squad_values_historical_out)
  truncate_reload(pg, "football_lab", "nb2_squad_values_historical", nb2_squad_values_historical_out)
  truncate_reload(pg, "football_lab", "nb2_squad_values_historical_raw", nb2_squad_values_historical_raw_out)
  truncate_reload(pg, "football_lab", "nb1_final_tables_historical", nb1_final_tables_historical_out)
  truncate_reload(pg, "football_lab", "nb2_tables_historical_raw", nb2_tables_historical_raw_out)
  truncate_reload(pg, "football_lab", "nb1_2627_goal_estimates", nb1_2627_goal_estimates_out)
  truncate_reload(pg, "football_lab", "nb1_2627_poisson_params", nb1_2627_poisson_params_out)
  truncate_reload(pg, "football_lab", "nb1_2627_preseason_forecast", nb1_2627_preseason_forecast_out)
  truncate_reload(pg, "football_lab", "nb1_2627_round1_ah_odds", nb1_2627_round1_ah_odds_out)
  truncate_reload(pg, "football_lab", "nb1_2627_round1_poisson_odds", nb1_2627_round1_poisson_odds_out)
  truncate_reload(pg, "football_lab", "nb2_2627_preseason_forecast", nb2_2627_preseason_forecast_out)
  truncate_reload(pg, "football_lab", "value_model_coefs", value_model_coefs_out)
  truncate_reload(pg, "football_lab", "value_model_residuals", value_model_residuals_out)
  truncate_reload(pg, "football_lab", "match_team_stats_long", match_team_stats_long_out)

  dbCommit(pg)
  cat("\n=== ÉLES ÍRÁS KÉSZ, COMMIT OK ===\n")
}, error = function(e) {
  dbRollback(pg)
  stop("Migráció sikertelen, ROLLBACK megtörtént: ", conditionMessage(e))
})

dbDisconnect(sq); dbDisconnect(pg)
