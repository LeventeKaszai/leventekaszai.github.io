library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DBI)
library(RSQLite)

# ── JSON fájlok beolvasása és feldolgozása ─────────────────────────────────────

parse_distribution <- function(filepath) {
  f     <- fromJSON(filepath, simplifyVector = FALSE)
  fname <- basename(filepath)

  position    <- toupper(str_extract(fname, "^[^_]+"))
  quality     <- gsub("^[^_]+_(.+)_distribution\\d+\\.json$", "\\1", fname)
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

json_files <- list.files("futball/raw_json", pattern = "\\.json$", full.names = TRUE)

all_data <- map_dfr(json_files, parse_distribution)

# ── Wide formátum: egy sor = egy játékos × szezon × pozíció ──────────────────

players_wide <- all_data |>
  pivot_wider(
    id_cols     = c(player_id, label, position, szezon),
    names_from  = c(quality, kpi),
    values_from = c(value_per90, rank),
    names_glue  = "{.value}_{quality}_{kpi}"
  )

# ── DB mentés ─────────────────────────────────────────────────────────────────

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")

dbWriteTable(con, "player_stats_long", all_data,    overwrite = TRUE)
dbWriteTable(con, "player_stats_wide", players_wide, overwrite = TRUE)

dbDisconnect(con)

cat("Kész!\n")
cat("Játékosok (unique):", n_distinct(all_data$player_id), "\n")
cat("Pozíciók:", paste(sort(unique(all_data$position)), collapse = ", "), "\n")
cat("Szezonok:", paste(sort(unique(all_data$szezon)),   collapse = ", "), "\n")
cat("Quality típusok:", paste(sort(unique(all_data$quality)), collapse = ", "), "\n")
