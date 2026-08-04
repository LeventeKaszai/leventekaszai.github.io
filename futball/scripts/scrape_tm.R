library(worldfootballR)
library(httr)
library(rvest)
library(dplyr)
library(purrr)
library(DBI)
library(RSQLite)

TM_HEADERS <- c(
  "User-Agent"      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
  "Accept-Language" = "de-DE,de;q=0.9,en;q=0.8",
  "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
)

# ── NB1: worldfootballR (1 request az egész ligához) ─────────────────────────

cat("=== NB1 2025/26 ===\n")
nb1_raw <- tryCatch(
  tm_player_market_values(
    start_year = 2025,
    league_url = "https://www.transfermarkt.com/nemzeti-bajnoksag/startseite/wettbewerb/UNG1/saison_id/2025"
  ),
  error = function(e) { cat("Hiba:", e$message, "\n"); NULL }
)

if (!is.null(nb1_raw)) {
  nb1_raw <- mutate(nb1_raw, liga = "NB1", comp_name = "Nemzeti Bajnokság I",
                    country = "Hungary", season_start_year = 2025L)
  cat("NB1 játékosok:", nrow(nb1_raw), "| csapatok:", n_distinct(nb1_raw$squad), "\n")
}

cat("\nVárakozás 120 mp...\n")
Sys.sleep(120)

# ── NB2: csapatlaponkénti rvest scraping (marktwerte hiányos NB2-ben) ─────────

# NB2 2025/26 — hardcoded (dinamikus lekérés megbízhatatlan TM-en)
nb2_team_urls <- c(
  "https://www.transfermarkt.com/vasas-fc/startseite/verein/5378/saison_id/2025",
  "https://www.transfermarkt.com/budapest-honved-fc/startseite/verein/709/saison_id/2025",
  "https://www.transfermarkt.com/mol-fehervar-fc/startseite/verein/11107/saison_id/2025",
  "https://www.transfermarkt.com/kecskemeti-te/startseite/verein/12423/saison_id/2025",
  "https://www.transfermarkt.com/mezokovesd-zsory-fc/startseite/verein/24032/saison_id/2025",
  "https://www.transfermarkt.com/szeged-csanad-ga/startseite/verein/30660/saison_id/2025",
  "https://www.transfermarkt.com/soroksar-sc/startseite/verein/14585/saison_id/2025",
  "https://www.transfermarkt.com/szentlorinc-se/startseite/verein/30624/saison_id/2025",
  "https://www.transfermarkt.com/fc-ajka/startseite/verein/24012/saison_id/2025",
  "https://www.transfermarkt.com/aqvital-fc-csakvar/startseite/verein/33524/saison_id/2025",
  "https://www.transfermarkt.com/kozarmisleny-fc/startseite/verein/24023/saison_id/2025",
  "https://www.transfermarkt.com/budapesti-vsc/startseite/verein/22516/saison_id/2025",
  "https://www.transfermarkt.com/duna-aszfalt-tiszakecskei-lc/startseite/verein/40173/saison_id/2025",
  "https://www.transfermarkt.com/karcagi-se/startseite/verein/27399/saison_id/2025",
  "https://www.transfermarkt.com/budafoki-mte/startseite/verein/28410/saison_id/2025",
  "https://www.transfermarkt.com/bekescsaba-1912-elore-se/startseite/verein/6049/saison_id/2025"
)
cat("\nNB2 csapatok:", length(nb2_team_urls), "\n\n")

parse_squad_page <- function(page, url) {
  # Csapat neve az oldalfejlécből
  # A TM idokozben "--main"-rol "--oswald"-ra (vagy mas variansra) valtoztatta
  # az osztalynevet -- csak az alap "data-header__headline-wrapper" osztalyra
  # szurunk, ami mindket variansban szerepel (tobb class egyszerre).
  squad_name <- page |>
    html_node("h1.data-header__headline-wrapper") |>
    html_text(trim = TRUE)
  if (is.na(squad_name) || squad_name == "") {
    squad_name <- sub("^.+/([^/]+)/startseite.*$", "\\1", url) |>
      gsub("-", " ", x = _) |> tools::toTitleCase()
  }

  rows <- page |> html_nodes("table.items > tbody > tr")
  rows <- rows[grepl("odd|even", html_attr(rows, "class", default = ""))]
  if (length(rows) == 0) return(NULL)

  map_dfr(rows, function(row) {
    name <- row |> html_node("td.posrela td.hauptlink a") |> html_text(trim = TRUE)
    if (is.na(name) || name == "") return(NULL)

    pos_rows <- row |> html_nodes("td.posrela table.inline-table tr")
    position <- if (length(pos_rows) >= 2) html_text(pos_rows[[2]], trim = TRUE) else NA_character_

    nat <- row |> html_node("img.flaggenrahmen") |> html_attr("title")

    # Kor kinyerése "(24)" mintából vagy dátum nélkül
    age_tds <- row |> html_nodes("td.zentriert")
    age_text <- if (length(age_tds) >= 2) html_text(age_tds[[2]], trim = TRUE) else NA_character_
    age <- suppressWarnings(
      as.integer(gsub(".*\\((\\d+)\\).*", "\\1", age_text))
    )
    if (is.na(age)) age <- suppressWarnings(as.integer(trimws(age_text)))

    mv_text <- row |> html_node("td.rechts.hauptlink") |> html_text(trim = TRUE)
    mv_euro <- tryCatch({
      num <- as.numeric(gsub("[^0-9.]", "", gsub(",", ".", mv_text)))
      case_when(
        grepl("m", mv_text, ignore.case = TRUE) ~ num * 1e6,
        grepl("k", mv_text, ignore.case = TRUE) ~ num * 1e3,
        TRUE ~ num
      )
    }, error = function(e) NA_real_)

    tibble(
      comp_name                = "Nemzeti Bajnokság II",
      country                  = "Hungary",
      season_start_year        = 2025L,
      squad                    = squad_name,
      player_name              = name,
      player_position          = position,
      player_nationality       = nat,
      player_age               = age,
      player_market_value_euro = mv_euro,
      liga                     = "NB2"
    )
  })
}

cat("=== NB2 2025/26 (csapatlaponként) ===\n")
nb2_list <- list()

for (i in seq_along(nb2_team_urls)) {
  url  <- nb2_team_urls[i]
  slug <- sub("^.+/([^/]+)/startseite.*$", "\\1", url)
  cat("[", i, "/", length(nb2_team_urls), "]", slug, "\n")

  resp <- tryCatch(
    GET(url, add_headers(.headers = TM_HEADERS), timeout(30)),
    error = function(e) { cat("  GET hiba:", e$message, "\n"); NULL }
  )

  if (!is.null(resp) && !http_error(resp)) {
    page <- read_html(content(resp, "text", encoding = "UTF-8"))
    res  <- parse_squad_page(page, url)
    if (!is.null(res) && nrow(res) > 0) {
      nb2_list[[url]] <- res
      cat("  →", nrow(res), "játékos |", unique(res$squad), "\n")
    } else {
      cat("  → Üres vagy parse hiba\n")
    }
  } else {
    cat("  → HTTP hiba:", if (!is.null(resp)) status_code(resp) else "NULL", "\n")
  }

  wait <- sample(20:40, 1)
  cat("  Várakozás", wait, "mp...\n")
  Sys.sleep(wait)
}

nb2_raw <- if (length(nb2_list) > 0) bind_rows(nb2_list) else NULL

if (!is.null(nb2_raw)) {
  cat("\nNB2 játékosok:", nrow(nb2_raw), "| csapatok:", n_distinct(nb2_raw$squad), "\n")
}

# ── DB MENTÉS ─────────────────────────────────────────────────────────────────

if (is.null(nb1_raw) && is.null(nb2_raw)) {
  cat("\nNincs mentendő adat.\n")
  quit(status = 1)
}

rosters <- bind_rows(nb1_raw, nb2_raw)

con <- dbConnect(RSQLite::SQLite(), "futball/futball.db")
dbWriteTable(con, "tm_rosters", rosters, overwrite = TRUE)
dbDisconnect(con)

cat("\n=== Kész ===\n")
cat("Összes játékos:", nrow(rosters), "\n")
print(rosters |> count(liga, squad) |> arrange(liga, squad), n = 50)
