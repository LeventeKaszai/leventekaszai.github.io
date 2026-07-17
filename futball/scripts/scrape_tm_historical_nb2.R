library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(DBI)
library(RSQLite)

# NB1 mintájára: NB2 historikus csapat-keretérték + tabella-adat.
# NB2 Transfermarkt kódja UN2 (nem UNG2 - az csak a regionális "East" bajnokság).
# A liga főoldala (startseite) NB2-re is tartalmazza az összesítő
# keretérték-táblázatot (ellenőrizve 2022/23-2026/27-re) - nem kell
# csapatlaponként scrape-elni, mint korábban feltételeztük.

TM_HEADERS <- c(
  "User-Agent"      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
  "Accept-Language" = "de-DE,de;q=0.9,en;q=0.8",
  "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
)

SEASONS <- c(2022, 2023, 2024, 2025, 2026)
SZEZON_LABELS <- c("2022" = "2022-23", "2023" = "2023-24", "2024" = "2024-25",
                    "2025" = "2025-26", "2026" = "2026-27")

parse_eur <- function(x) {
  num <- suppressWarnings(as.numeric(gsub("[^0-9.,]", "", gsub(",", ".", x))))
  case_when(
    grepl("m", x, ignore.case = TRUE) ~ num * 1e6,
    grepl("k", x, ignore.case = TRUE) ~ num * 1e3,
    TRUE ~ num
  )
}

# ── 1. SZEZONONKÉNTI CSAPAT-KERETÉRTÉK TÁBLA (egy oldal/szezon) ──────────────

scrape_season_squad_values <- function(start_year) {
  league_url <- paste0(
    "https://www.transfermarkt.com/nemzeti-bajnoksag-ii-/startseite/wettbewerb/UN2/saison_id/",
    start_year
  )
  resp <- GET(league_url, add_headers(.headers = TM_HEADERS), timeout(30))
  html <- content(resp, "text", encoding = "UTF-8")

  start_idx <- str_locate(html, "</tfoot>")[1, 2]
  after     <- substr(html, start_idx, nchar(html))
  end_idx   <- start_idx + str_locate(after, "</tbody>")[1, 1]
  chunk     <- substr(html, start_idx, end_idx)

  row_htmls <- str_split(chunk, '<tr class="(?:odd|even)">')[[1]][-1]

  map_dfr(row_htmls, function(r) {
    name <- str_match(r, 'title="([^"]+)"')[, 2]
    tds  <- str_match_all(r, "<td[^>]*>(.*?)</td>")[[1]][, 2]
    eur_vals <- str_extract_all(r, "€[\\d.,]+[mk]?")[[1]]

    tibble(
      season_start_year = start_year,
      szezon             = SZEZON_LABELS[as.character(start_year)],
      squad              = name,
      squad_size         = suppressWarnings(as.integer(str_extract(str_remove_all(tds[3], "<[^>]+>"), "\\d+"))),
      avg_age            = suppressWarnings(as.numeric(str_replace(tds[4], ",", "."))),
      foreigners         = suppressWarnings(as.integer(str_extract(tds[5], "\\d+"))),
      avg_market_value_euro   = if (length(eur_vals) >= 1) parse_eur(eur_vals[1]) else NA_real_,
      total_market_value_euro = if (length(eur_vals) >= 2) parse_eur(eur_vals[2]) else NA_real_
    )
  })
}

# ── 2. SZEZONONKÉNTI TABELLA (Pl/W/D/L/GF:GA/Pts) - a pontszám-célváltozóhoz ──
# Csak a 2022-23, 2023-24, 2024-25-re kell (2025-26-ra már megvan a Points
# a team_stats-ban, Wyscout-eredetű).

scrape_season_table <- function(start_year) {
  table_url <- paste0(
    "https://www.transfermarkt.com/nemzeti-bajnoksag-ii-/tabelle/wettbewerb/UN2?saison_id=",
    start_year
  )
  resp <- GET(table_url, add_headers(.headers = TM_HEADERS), timeout(30))
  html <- content(resp, "text", encoding = "UTF-8")

  idx   <- str_locate(html, 'class="items"')[1, 1]
  chunk <- substr(html, idx, idx + 20000)
  rows  <- str_match_all(chunk, "(?s)<tr>(.*?)</tr>")[[1]][, 2]

  map_dfr(rows, function(r) {
    if (!str_detect(r, "Platz1|zentriert")) return(NULL)
    cells <- str_split(str_remove_all(r, "&nbsp;"), "<t[dh][^>]*>")[[1]][-1]
    cells <- str_remove_all(cells, "</t[dh]>.*$")
    cells <- str_trim(str_remove_all(cells, "<[^>]+>"))
    cells <- cells[cells != ""]
    if (length(cells) < 8) return(NULL)
    # oszlopok: rang | csapat | Pl | W | D | L | Goals | +/- | Pts
    rang <- suppressWarnings(as.integer(cells[1]))
    if (is.na(rang)) return(NULL)
    n <- length(cells)
    tibble(
      season_start_year = start_year,
      szezon  = SZEZON_LABELS[as.character(start_year)],
      rang    = rang,
      squad   = cells[2],
      played  = suppressWarnings(as.integer(cells[n - 6])),
      pts     = suppressWarnings(as.integer(cells[n]))
    )
  })
}

# ── 3. FUTTATÁS ────────────────────────────────────────────────────────────

cat("=== NB2 historikus csapat-keretertekek: 22/23 - 26/27 ===\n\n")
squad_value_list <- list()
for (start_year in SEASONS) {
  cat("--- Szezon", SZEZON_LABELS[as.character(start_year)], "---\n")
  res <- tryCatch(scrape_season_squad_values(start_year), error = function(e) {
    cat("  Hiba:", e$message, "\n"); NULL
  })
  if (!is.null(res)) {
    squad_value_list[[as.character(start_year)]] <- res
    cat("  ->", nrow(res), "csapat\n")
  }
  wait <- sample(15:25, 1)
  cat("  Varakozas", wait, "mp...\n\n")
  Sys.sleep(wait)
}
squad_values_hist <- bind_rows(squad_value_list)
cat("Osszesen (keretertek):", nrow(squad_values_hist), "csapat-szezon sor\n\n")

cat("=== NB2 historikus tabellak: 22/23 - 24/25 ===\n\n")
table_list <- list()
for (start_year in c(2022, 2023, 2024)) {
  cat("--- Szezon", SZEZON_LABELS[as.character(start_year)], "---\n")
  res <- tryCatch(scrape_season_table(start_year), error = function(e) {
    cat("  Hiba:", e$message, "\n"); NULL
  })
  if (!is.null(res)) {
    table_list[[as.character(start_year)]] <- res
    cat("  ->", nrow(res), "csapat\n")
  }
  wait <- sample(10:18, 1)
  cat("  Varakozas", wait, "mp...\n\n")
  Sys.sleep(wait)
}
tables_hist <- bind_rows(table_list)
cat("Osszesen (tabella):", nrow(tables_hist), "csapat-szezon sor\n\n")

# ── 4. DB MENTÉS (nyers, TM-neveken - a kanonizálás külön lépésben) ─────────

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")
dbWriteTable(con, "nb2_squad_values_historical_raw", squad_values_hist, overwrite = TRUE)
dbWriteTable(con, "nb2_tables_historical_raw", tables_hist, overwrite = TRUE)
dbDisconnect(con)

cat("nb2_squad_values_historical_raw es nb2_tables_historical_raw mentve.\n")
cat("\n=== Kesz ===\n")
