library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(DBI)
library(RSQLite)

# NB2 vegleges tabella-tortenet: a nb2_tables_historical_raw csak
# played+pts-t tartalmazott (kanonizalatlan csapatnevekkel), es csak
# 2022-23 - 2024-25-re. Ez a script az nb1_final_tables_historical mintajat
# koveti: teljes W/D/L/GF/GA/GD bontas, kanonizalt csapatnevek,
# 2022-23 - 2025-26 (mind a negy mar lezart szezon).

TM_HEADERS <- c(
  "User-Agent"      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
  "Accept-Language" = "de-DE,de;q=0.9,en;q=0.8",
  "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
)

SEASONS <- c(2022, 2023, 2024, 2025)
SZEZON_LABELS <- c("2022" = "2022-23", "2023" = "2023-24", "2024" = "2024-25", "2025" = "2025-26")

mapping_raw <- read.csv("futball/data/team_name_mapping.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
TEAM_MAP <- setNames(mapping_raw$canonical_name, mapping_raw$tm_name)
canonicalize <- function(tm_name) {
  hit <- TEAM_MAP[tm_name]
  ifelse(is.na(hit), tm_name, unname(hit))
}

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
    if (length(cells) < 9) return(NULL)
    rang <- suppressWarnings(as.integer(cells[1]))
    if (is.na(rang)) return(NULL)
    n <- length(cells)
    # oszlopok a vegen: Pl | W | D | L | Goals(gf:ga) | +/- | Pts
    goals <- str_split(cells[n - 2], ":")[[1]]
    team_tm <- cells[2]
    tibble(
      szezon         = SZEZON_LABELS[as.character(start_year)],
      team_canonical = canonicalize(team_tm),
      team_tm        = team_tm,
      rank           = rang,
      pl             = suppressWarnings(as.integer(cells[n - 6])),
      w              = suppressWarnings(as.integer(cells[n - 5])),
      d              = suppressWarnings(as.integer(cells[n - 4])),
      l              = suppressWarnings(as.integer(cells[n - 3])),
      gf             = suppressWarnings(as.integer(goals[1])),
      ga             = suppressWarnings(as.integer(goals[2])),
      gd             = suppressWarnings(as.integer(cells[n - 1])),
      pts            = suppressWarnings(as.integer(cells[n]))
    )
  })
}

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")

for (start_year in SEASONS) {
  label <- SZEZON_LABELS[as.character(start_year)]
  cat("=== NB2", label, "tabella ===\n")
  res <- tryCatch(scrape_season_table(start_year), error = function(e) {
    cat("  Hiba:", e$message, "\n"); NULL
  })
  if (!is.null(res) && nrow(res) > 0) {
    cat("  ->", nrow(res), "csapat\n")
    print(as.data.frame(res |> select(team_canonical, rank, pl, w, d, l, gf, ga, pts)))
    if (dbExistsTable(con, "nb2_final_tables_historical")) {
      dbExecute(con, "DELETE FROM nb2_final_tables_historical WHERE szezon = ?", params = list(label))
      dbWriteTable(con, "nb2_final_tables_historical", res, append = TRUE)
    } else {
      dbWriteTable(con, "nb2_final_tables_historical", res, overwrite = TRUE)
    }
  } else {
    cat("  -> Nincs adat, kihagyva\n")
  }
  wait <- sample(15:25, 1)
  cat("  Varakozas", wait, "mp...\n\n")
  Sys.sleep(wait)
}

dbDisconnect(con)
cat("=== Kesz ===\n")
