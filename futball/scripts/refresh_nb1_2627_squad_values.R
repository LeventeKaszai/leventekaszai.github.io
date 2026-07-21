library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(DBI)
library(RSQLite)

# Az NB1 2026/27 sor frissítése a nb1_squad_values_historical táblában a
# nyári átigazolások utáni, frissen scrapelt TM-adatok alapján. Csak a
# 2026-27 szezon sorait cseréli le — a többi szezon (2022-23 .. 2025-26)
# érintetlen marad.

TM_HEADERS <- c(
  "User-Agent"      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
  "Accept-Language" = "de-DE,de;q=0.9,en;q=0.8",
  "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
)

parse_eur <- function(x) {
  num <- suppressWarnings(as.numeric(gsub("[^0-9.,]", "", gsub(",", ".", x))))
  case_when(
    grepl("m", x, ignore.case = TRUE) ~ num * 1e6,
    grepl("k", x, ignore.case = TRUE) ~ num * 1e3,
    TRUE ~ num
  )
}

scrape_season_squad_values <- function(start_year, szezon_label) {
  league_url <- paste0(
    "https://www.transfermarkt.com/nemzeti-bajnoksag/startseite/wettbewerb/UNG1/saison_id/",
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
      szezon             = szezon_label,
      squad              = name,
      squad_size         = suppressWarnings(as.integer(str_extract(str_remove_all(tds[3], "<[^>]+>"), "\\d+"))),
      avg_age            = suppressWarnings(as.numeric(str_replace(tds[4], ",", "."))),
      foreigners         = suppressWarnings(as.integer(str_extract(tds[5], "\\d+"))),
      avg_market_value_euro   = if (length(eur_vals) >= 1) parse_eur(eur_vals[1]) else NA_real_,
      total_market_value_euro = if (length(eur_vals) >= 2) parse_eur(eur_vals[2]) else NA_real_
    )
  })
}

cat("=== NB1 2026/27 csapat-keretertek frissites ===\n\n")
new_row <- scrape_season_squad_values(2026, "2026-27")
cat("->", nrow(new_row), "csapat\n")

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")

# team_canonical hozzárendelése a régi 2026-27 sorok alapján (squad -> team_canonical)
old_map <- dbGetQuery(con, "
  SELECT DISTINCT squad, team_canonical FROM nb1_squad_values_historical WHERE szezon = '2026-27'
")

new_row <- new_row |> left_join(old_map, by = "squad")

missing_map <- new_row |> filter(is.na(team_canonical))
if (nrow(missing_map) > 0) {
  cat("\nFIGYELEM: nincs team_canonical hozzárendelés ezekhez:\n")
  print(missing_map$squad)
  stop("Térképezd fel a hiányzó csapatokat, mielőtt tovább mész.")
}

print(as.data.frame(new_row |> select(squad, team_canonical, squad_size, avg_age, foreigners, avg_market_value_euro, total_market_value_euro)))

dbExecute(con, "DELETE FROM nb1_squad_values_historical WHERE szezon = '2026-27'")
dbWriteTable(con, "nb1_squad_values_historical", new_row, append = TRUE)

cat("\n=== Kész ===\n")
print(dbGetQuery(con, "SELECT szezon, COUNT(*) n FROM nb1_squad_values_historical GROUP BY szezon"))

dbDisconnect(con)
