library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DBI)
library(RSQLite)

# ── CSAPATSTATISZTIKÁK (szezon-szintű) ────────────────────────────────────────

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

  # Szezon felismerése fájlnévből: _2223 → 2022-23, _2324 → 2023-24, _2425 → 2024-25, _2526 → 2025-26, _2627 → 2026-27, egyéb → 2025-26
  season_code <- str_extract(fname, "_(\\d{4})\\.json$", group = 1)
  szezon <- switch(season_code %||% "",
    "2223" = "2022-23",
    "2324" = "2023-24",
    "2425" = "2024-25",
    "2526" = "2025-26",
    "2627" = "2026-27",
    "2025-26"
  )

  # Quality: nb1_ prefix és szezon-suffix (_2425/_2526) eltávolítva
  name_part <- gsub("^nb1_", "", gsub("\\.json$", "", fname))
  quality   <- gsub("_\\d{4}$", "", name_part)
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

files_nb1_team_dist <- list.files("futball/raw_json/teams/nb1",
  pattern = "^nb1_(attack|attackingtransition|chancecreation|defence|defensivetransition|intelligentdefence|oppositionchancecreation|overallperformance|styleofplay)",
  full.names = TRUE)
files_nb2_team      <- list.files("futball/raw_json/teams/nb2", pattern = "^nb2_.+\\.json$", full.names = TRUE)

team_stats <- bind_rows(
  map_dfr(files_nb1_team_dist, parse_nb1_team_dist_file),
  map_dfr(files_nb2_team,      parse_nb2_team_file)
)

# ── DB mentés ─────────────────────────────────────────────────────────────────
# Csak a team_stats táblát írja felül.

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")

dbWriteTable(con, "team_stats", team_stats, overwrite = TRUE)

dbDisconnect(con)

cat("Kész! (csapat adatok)\n\n")
print(team_stats |> count(liga, szezon, name = "sorok") |>
  left_join(team_stats |> group_by(liga) |>
    summarise(csapatok = n_distinct(team), kpik = n_distinct(kpi)), by = "liga"))
