library(httr); library(rvest); library(stringr); library(dplyr); library(purrr); library(DBI); library(RSQLite)

TM_HEADERS <- c(
  "User-Agent"      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
  "Accept-Language" = "de-DE,de;q=0.9,en;q=0.8",
  "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
)
SZEZON_LABELS <- c("2022" = "2022-23", "2023" = "2023-24", "2024" = "2024-25")

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
    cells <- str_split(str_remove_all(r, "&nbsp;"), "<t[dh][^>]*>")[[1]][-1]
    cells <- str_remove_all(cells, "</t[dh]>.*$")
    cells <- str_trim(str_remove_all(cells, "<[^>]+>"))
    cells <- cells[cells != ""]
    if (length(cells) < 8) return(NULL)
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
cat("Osszesen (tabella):", nrow(tables_hist), "csapat-szezon sor\n")
print(tables_hist)

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")
dbWriteTable(con, "nb2_tables_historical_raw", tables_hist, overwrite = TRUE)
dbDisconnect(con)
cat("Mentve.\n")
