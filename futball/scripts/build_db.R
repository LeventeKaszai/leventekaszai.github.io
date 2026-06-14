library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DBI)
library(RSQLite)

# ── 1. JÁTÉKOS ADATOK ─────────────────────────────────────────────────────────

# Parser: régi distribution formátum (pl. cb_activedefence_distribution2425.json)
# és új formátum (pl. st_aerialthreat2425.json — _distribution nélkül)
parse_distribution <- function(filepath) {
  f     <- fromJSON(filepath, simplifyVector = FALSE)
  fname <- basename(filepath)

  position    <- toupper(str_extract(fname, "^[^_]+"))
  quality     <- gsub("(_distribution)?\\d{4}\\.json$", "",
                      gsub("^[^_]+_", "", fname))
  season_code <- str_extract(fname, "\\d{4}(?=\\.json$)")
  szezon      <- switch(season_code, "2425" = "2024-25", "2526" = "2025-26", season_code)

  kpis <- map_chr(f$data$kpis, "name")
  n    <- length(f$data$target_identifiers)

  map_dfr(seq_len(n), function(i) {
    hovers <- f$data$hover_strings[[i]]
    tibble(
      player_id   = f$data$target_identifiers[[i]],
      label       = f$data$target_labels[[i]],
      position    = position,
      szezon      = szezon,
      quality     = quality,
      kpi         = kpis,
      value_per90 = map_dbl(hovers, ~{
        str_split(.x, "\n")[[1]][3] |> str_extract("-?[\\d.]+") |> as.numeric()
      }),
      rank    = str_extract(hovers, "(?<=Rank: )\\d+") |> as.integer(),
      rank_of = str_extract(hovers, "(?<=/)\\d+")      |> as.integer()
    )
  })
}

# Parser: nb1_ formátum (pl. nb1_st_finishing.json → 2025-26 végleges)
parse_nb1_file <- function(filepath) {
  f     <- fromJSON(filepath, simplifyVector = FALSE)
  fname <- basename(filepath)

  name_part <- gsub("^nb1_(.+)\\.json$", "\\1", fname)

  # Pozíció prefix nélküli fájlok kihagyva (pl. nb1_intelligentdefence.json — CB duplikátum)
  if (!grepl("^(cb|fb|m|st|w)_", name_part)) {
    message("Kihagyva (nincs pozíció prefix): ", fname)
    return(NULL)
  }

  position <- toupper(str_extract(name_part, "^[^_]+"))
  quality  <- gsub("^[^_]+_(.+)$", "\\1", name_part)
  szezon   <- "2025-26"

  kpis <- map_chr(f$data$kpis, "name")
  n    <- length(f$data$target_identifiers)

  map_dfr(seq_len(n), function(i) {
    hovers <- f$data$hover_strings[[i]]
    tibble(
      player_id   = f$data$target_identifiers[[i]],
      label       = f$data$target_labels[[i]],
      position    = position,
      szezon      = szezon,
      quality     = quality,
      kpi         = kpis,
      value_per90 = map_dbl(hovers, ~{
        str_split(.x, "\n")[[1]][3] |> str_extract("-?[\\d.]+") |> as.numeric()
      }),
      rank    = str_extract(hovers, "(?<=Rank: )\\d+") |> as.integer(),
      rank_of = str_extract(hovers, "(?<=/)\\d+")      |> as.integer()
    )
  })
}

# 2024-25: distribution2425 és új st_*2425 formátumú fájlok
files_2425 <- list.files("futball/raw_json/players/2024-25", pattern = "2425\\.json$", full.names = TRUE)
data_2425  <- map_dfr(files_2425, parse_distribution)

# 2025-26: nb1_ végleges fájlok (distribution2526 fájlok kihagyva — felváltja az nb1_)
files_nb1  <- list.files("futball/raw_json/players/2025-26", pattern = "^nb1_.+\\.json$", full.names = TRUE)
data_2526  <- map_dfr(files_nb1, parse_nb1_file)

all_data <- bind_rows(data_2425, data_2526)

# ── Wide formátumok ───────────────────────────────────────────────────────────

players_wide <- all_data |>
  pivot_wider(
    id_cols     = c(player_id, label, position, szezon),
    names_from  = c(quality, kpi),
    values_from = c(value_per90, rank),
    names_glue  = "{.value}_{quality}_{kpi}"
  )

# player_stats: pozíció + szezon szerinti wide tábla (backward compat)
player_stats <- all_data |>
  pivot_wider(
    id_cols     = c(player_id, label, position, szezon),
    names_from  = c(quality, kpi),
    values_from = c(value_per90, rank),
    names_glue  = "{.value}_{quality}_{kpi}"
  )

# ── 2. CSAPATSTATISZTIKÁK ─────────────────────────────────────────────────────

# NB1 parser: egy fájl = egy KPI, ranking-bar formátum
parse_nb1_team_file <- function(filepath) {
  f     <- fromJSON(filepath, simplifyVector = FALSE)
  fname <- basename(filepath)

  kpi    <- gsub("^nb12526(.+)\\.json$", "\\1", fname)
  szezon <- "2025-26"
  liga   <- "NB1"

  teams  <- f$data$target_labels
  values <- f$data$plot_values
  hovers <- f$data$hover_strings

  tibble(
    team    = unlist(teams),
    liga    = liga,
    szezon  = szezon,
    quality = NA_character_,
    kpi     = kpi,
    value   = as.numeric(unlist(values)),
    rank    = str_extract(unlist(hovers), "(?<=Rank: )\\d+") |> as.integer(),
    rank_of = str_extract(unlist(hovers), "(?<=/)(\\d+)") |> as.integer()
  )
}

# NB2 parser: egy fájl = több KPI, distribution formátum
parse_nb2_team_file <- function(filepath) {
  f     <- fromJSON(filepath, simplifyVector = FALSE)
  fname <- basename(filepath)

  quality <- gsub("^nb2_(.+)\\.json$", "\\1", fname)
  szezon  <- "2025-26"
  liga    <- "NB2"

  kpi_names <- map_chr(f$data$kpis, "name")
  teams     <- f$data$target_labels
  hovers    <- f$data$hover_strings
  n         <- length(teams)
  k         <- length(kpi_names)

  map_dfr(seq_len(n), function(i) {
    h <- hovers[[i]]
    map_dfr(seq_len(k), function(j) {
      hover  <- h[[j]]
      lines  <- str_split(hover, "\n")[[1]]
      has_rank <- str_detect(hover, "Rank:")
      val_line <- if (has_rank) lines[3] else lines[2]
      tibble(
        team    = teams[[i]],
        liga    = liga,
        szezon  = szezon,
        quality = quality,
        kpi     = kpi_names[j],
        value   = str_extract(val_line, "-?[\\d.]+") |> as.numeric(),
        rank    = if (has_rank) str_extract(hover, "(?<=Rank: )\\d+") |> as.integer() else NA_integer_,
        rank_of = if (has_rank) str_extract(hover, "(?<=/)(\\d+)") |> as.integer() else NA_integer_
      )
    })
  })
}

# NB1 team parser: multi-KPI distribution formátum (pl. nb1_attack.json)
parse_nb1_team_dist_file <- function(filepath) {
  f     <- fromJSON(filepath, simplifyVector = FALSE)
  fname <- basename(filepath)

  quality <- gsub("^nb1_(.+)\\.json$", "\\1", fname)
  szezon  <- "2025-26"
  liga    <- "NB1"

  if (!identical(f$target_type, "team")) {
    message("Kihagyva (nem csapat fájl): ", fname)
    return(NULL)
  }

  kpi_names <- map_chr(f$data$kpis, "name")
  teams     <- f$data$target_labels
  hovers    <- f$data$hover_strings
  n         <- length(teams)
  k         <- length(kpi_names)

  map_dfr(seq_len(n), function(i) {
    h <- hovers[[i]]
    map_dfr(seq_len(k), function(j) {
      hover  <- h[[j]]
      lines  <- str_split(hover, "\n")[[1]]
      has_rank <- str_detect(hover, "Rank:")
      val_line <- if (has_rank) lines[3] else lines[2]
      tibble(
        team    = teams[[i]],
        liga    = liga,
        szezon  = szezon,
        quality = quality,
        kpi     = kpi_names[j],
        value   = str_extract(val_line, "-?[\\d.]+") |> as.numeric(),
        rank    = if (has_rank) str_extract(hover, "(?<=Rank: )\\d+") |> as.integer() else NA_integer_,
        rank_of = if (has_rank) str_extract(hover, "(?<=/)(\\d+)") |> as.integer() else NA_integer_
      )
    })
  })
}

files_nb1_team_dist <- list.files("futball/raw_json/teams/nb1", pattern = "^nb1_(attack|attackingtransition|chancecreation|defence|defensivetransition|intelligentdefence|oppositionchancecreation|overallperformance|styleofplay.+)\\.json$", full.names = TRUE)
files_nb2_team      <- list.files("futball/raw_json/teams/nb2", pattern = "^nb2_.+\\.json$", full.names = TRUE)

team_stats <- bind_rows(
  map_dfr(files_nb1_team_dist, parse_nb1_team_dist_file),
  map_dfr(files_nb2_team,      parse_nb2_team_file)
)

# ── DB mentés ─────────────────────────────────────────────────────────────────

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")

dbWriteTable(con, "player_stats_long", all_data,     overwrite = TRUE)
dbWriteTable(con, "player_stats_wide", players_wide, overwrite = TRUE)
dbWriteTable(con, "player_stats",      player_stats, overwrite = TRUE)
dbWriteTable(con, "team_stats",        team_stats,   overwrite = TRUE)

dbDisconnect(con)

cat("Kész!\n\n")
cat("=== Játékos adatok ===\n")
cat("Összes sor:", nrow(all_data), "\n")
cat("Játékosok (unique):", n_distinct(all_data$player_id), "\n")
cat("Szezonok × pozíciók:\n")
print(all_data |> count(szezon, position))
cat("\n=== Csapat adatok ===\n")
print(team_stats |> count(liga, szezon, name = "sorok") |>
  left_join(team_stats |> group_by(liga) |>
    summarise(csapatok = n_distinct(team), kpik = n_distinct(kpi)), by = "liga"))

