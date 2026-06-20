library(htmltools)

# ── DB stat loader ──────────────────────────────────────────────────────────────

load_player_db_stats <- function(nev, db_path = "futball/futball.db") {
  if (!file.exists(db_path)) return(NULL)

  library(DBI)
  library(RSQLite)

  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))

  # Player map: player_id keresés — több stratégiával
  resolve_player_id <- function(con, nev) {
    # 1) Exact match
    pm <- dbGetQuery(con,
      "SELECT player_id, player_name FROM player_map WHERE player_name = ? LIMIT 1",
      params = list(nev))
    if (nrow(pm) > 0) return(pm)

    # 2) Fordított szórend (pl. "VITÁLYOS Viktor" → "Viktor Vitályos")
    words   <- strsplit(trimws(nev), "\\s+")[[1]]
    rev_nev <- paste(rev(words), collapse = " ")
    rev_tc  <- paste(
      sapply(strsplit(rev_nev, " ")[[1]], function(w)
        paste0(toupper(substr(w, 1, 1)), tolower(substr(w, 2, nchar(w))))),
      collapse = " ")
    pm <- dbGetQuery(con,
      "SELECT player_id, player_name FROM player_map WHERE player_name = ? LIMIT 1",
      params = list(rev_tc))
    if (nrow(pm) > 0) return(pm)

    # 3) Normalizált label egyezés (ékezet nélkül, kisbetű)
    norm <- function(x) {
      x <- tolower(x)
      chartr("\u00e1\u00e9\u00ed\u00f3\u00f6\u0151\u00fa\u00fc\u0171",
             "aeiooouuu", x)
    }
    norm_nev     <- norm(nev)
    norm_rev     <- norm(rev_tc)
    all_pm <- dbGetQuery(con, "SELECT player_id, player_name, label FROM player_map")
    all_pm$label_norm <- norm(all_pm$label)
    hit <- all_pm[all_pm$label_norm == norm_nev | all_pm$label_norm == norm_rev, ]
    if (nrow(hit) > 0) return(hit[1, c("player_id", "player_name")])

    data.frame()
  }

  pm <- resolve_player_id(con, nev)
  if (nrow(pm) == 0) return(NULL)
  pid      <- pm$player_id[1]
  tm_name  <- pm$player_name[1]   # TM-kompatibilis név a további lekérdezésekhez

  # TransferMarkt adatok
  tm <- dbGetQuery(con,
    "SELECT squad, player_dob, player_age, player_nationality,
            player_height_mtrs, player_foot, player_market_value_euro,
            contract_expiry, player_url, player_position
     FROM tm_rosters WHERE player_name = ? LIMIT 1",
    params = list(tm_name)
  )

  klub        <- if (nrow(tm) > 0 && !is.na(tm$squad[1]))           tm$squad[1]         else NULL
  kor         <- if (nrow(tm) > 0 && !is.na(tm$player_age[1]))      as.integer(tm$player_age[1]) else NULL
  allampolgarsag <- if (nrow(tm) > 0 && !is.na(tm$player_nationality[1])) tm$player_nationality[1] else NULL
  mag         <- if (nrow(tm) > 0 && !is.na(tm$player_height_mtrs[1])) tm$player_height_mtrs[1] else NULL
  lab         <- if (nrow(tm) > 0 && !is.na(tm$player_foot[1]))     tm$player_foot[1]   else NULL
  tm_ertek    <- if (nrow(tm) > 0 && !is.na(tm$player_market_value_euro[1]))
                   tm$player_market_value_euro[1] else NULL
  szerzodes_lejarat <- if (nrow(tm) > 0 && !is.na(tm$contract_expiry[1])) tm$contract_expiry[1] else NULL

  # Statisztikák (long formátum) — mindkét pozíció
  stats <- dbGetQuery(con,
    "SELECT position, quality, kpi AS KPI, value_per90, rank, rank_of
     FROM player_stats_long WHERE player_id = ?
     ORDER BY position, quality, kpi",
    params = list(pid)
  )

  # Értékelési modell
  model <- dbGetQuery(con,
    "SELECT position, value_predicted_eur AS pred_value, residual FROM value_model_residuals WHERE player_id = ?",
    params = list(pid)
  )

  list(
    klub       = klub,
    kor        = kor,
    allampolgarsag = allampolgarsag,
    mag        = mag,
    lab        = lab,
    tm_ertek   = tm_ertek,
    szerzodes_lejarat = szerzodes_lejarat,
    stats      = stats,
    model      = model
  )
}

# ── Fix KPI lista ───────────────────────────────────────────────────────────────

FUTBALL_DEFAULT_KPIS <- c(
  "Defensive actions",
  "Defending 1v1 %",
  "Defensive line height (m)",
  "Ball recoveries",
  "Aerials won %",
  "Touches",
  "Ball progression (xT)",
  "Playmaking passes"
)

# ── Fix statisztika kártyák ─────────────────────────────────────────────────────

pick_key_metrics <- function(stats, position, kpi_list = FUTBALL_DEFAULT_KPIS) {

  if (nrow(stats) == 0) return(list())

  pos_stats   <- stats[stats$position == position, ]
  other_stats <- stats[stats$position != position, ]

  rank_pct <- function(r, r_of) 1 - (r - 1) / (r_of - 1)

  fmt_kpi_label <- function(kpi) {
    gsub("\\^2", "\u00b2", kpi)
  }

  find_row <- function(requested, df) {
    hit <- df[df$KPI == requested, ]
    if (nrow(hit) == 0) hit <- df[startsWith(df$KPI, requested), ]
    if (nrow(hit) == 0) return(NULL)
    hit[1, ]
  }

  metrics <- list()
  for (req in kpi_list) {
    row <- find_row(req, pos_stats)
    if (is.null(row)) row <- find_row(req, other_stats)
    if (is.null(row)) next

    pct      <- rank_pct(row$rank, row$rank_of)
    pct_disp <- round(pct * 100)
    ctx <- if (row$rank == 1) {
      "Liga legjobb"
    } else if (pct_disp >= 90) {
      paste0("Top ", 100 - pct_disp, "%")
    } else if (pct_disp >= 80) {
      "Fels\u0151 20%"
    } else if (pct_disp <= 10) {
      "Als\u00f3 10%"
    } else if (pct_disp <= 20) {
      "Als\u00f3 20%"
    } else {
      NULL
    }

    metrics[[length(metrics) + 1]] <- list(
      label   = fmt_kpi_label(row$KPI),
      value   = paste0(row$rank, "/", row$rank_of),
      context = ctx,
      pct     = pct
    )
  }

  metrics
}

# ── Render function ─────────────────────────────────────────────────────────────

render_player_profile <- function(profile, db_path = "futball/futball.db") {
  p <- profile

  db <- tryCatch(
    load_player_db_stats(p$nev, db_path = db_path),
    error = function(e) { message("DB hiba: ", e$message); NULL }
  )

  translate_foot <- function(f) {
    switch(tolower(as.character(f)),
      "left"  = "bal",
      "right" = "jobb",
      "both"  = "mindkett\u0151",
      f
    )
  }

  if (!is.null(db)) {
    if (!is.null(db$klub)  && is.null(p$klub))  p$klub  <- db$klub
    if (!is.null(db$kor))                        p$kor   <- db$kor
    if (!is.null(db$allampolgarsag))             p$allampolgarsag <- db$allampolgarsag
    if (!is.null(db$mag))                        p$mag   <- db$mag
    if (!is.null(db$lab))                        p$lab   <- translate_foot(db$lab)
    if (!is.null(db$tm_ertek))                   p$tm_ertek <- db$tm_ertek
    if (!is.null(db$szerzodes_lejarat))          p$szerzodes_lejarat <- db$szerzodes_lejarat
  }

  # Elsődleges pozíció (FB vagy CB stb.)
  primary_pos <- if (!is.null(p$pozicio)) {
    strsplit(p$pozicio, "[/ ,]+")[[1]][1]
  } else "FB"

  mutatok <- if (!is.null(db) && !is.null(db$stats) && nrow(db$stats) > 0) {
    pick_key_metrics(db$stats, primary_pos)
  } else {
    p$mutatok
  }

  # ── Helpers ──────────────────────────────────────────────────────────────────

  rating_cfg <- function(r) {
    switch(tolower(as.character(r)),
      "strong-buy"  = list(bg = "#1a7a4a", label = "Strong Buy"),
      "buy"         = list(bg = "#4caf50", label = "Buy"),
      "hold"        = list(bg = "#7A756D", label = "Hold"),
      "sell"        = list(bg = "#e57373", label = "Sell"),
      "strong-sell" = list(bg = "#C43A28", label = "Strong Sell"),
      list(bg = "#7A756D", label = as.character(r))
    )
  }

  rating_badge <- function(r) {
    cfg <- rating_cfg(r)
    tags$span(
      class = "pp-rating-badge",
      style = paste0("background:", cfg$bg, ";"),
      cfg$label
    )
  }

  outcome_style <- function(o) {
    switch(tolower(as.character(o)),
      "bejott"         = "color:#1a7a4a;font-weight:600;",
      "bej\u00f6tt"   = "color:#1a7a4a;font-weight:600;",
      "reszben bejott" = "color:#5A7D9F;font-weight:600;",
      "r\u00e9szben bej\u00f6tt" = "color:#5A7D9F;font-weight:600;",
      "nem jott be"    = "color:#C43A28;font-weight:600;",
      "nem j\u00f6tt be" = "color:#C43A28;font-weight:600;",
      ""
    )
  }

  multiline_text <- function(txt) {
    paras <- strsplit(as.character(txt), "\n\n")[[1]]
    lapply(paras, function(para) tags$p(para))
  }

  fmt_ertek <- function(v) {
    if (is.null(v) || is.na(v)) return(NULL)
    if (v >= 1e6) paste0(format(v / 1e6, digits = 2, nsmall = 0), "M \u20ac")
    else if (v >= 1e3) paste0(format(v / 1e3, digits = 2, nsmall = 0), "K \u20ac")
    else paste0(as.integer(v), " \u20ac")
  }

  is_valogatott <- isTRUE(p$valogatott) ||
    tolower(as.character(p$valogatott)) %in% c("igen", "true", "yes")

  # ── Build HTML ────────────────────────────────────────────────────────────────

  tagList(

    # ── Vissza navigáció
    tags$div(
      class = "pp-back-nav",
      tags$a(href = "../jatekosok.html", "\u2190 Vissza a list\u00e1hoz")
    ),

    # ── Header
    tags$div(
      class = "pp-header",
      tags$div(
        class = "pp-header-top",
        tags$h1(class = "pp-name", p$nev),
        rating_badge(p$besorolas)
      ),
      tags$div(
        class = "pp-meta-badges",
        tags$span(class = "pp-badge pp-badge-sport", "Labdar\u00fcg\u00e1s"),
        if (!is.null(p$klub))           tags$span(class = "pp-badge", p$klub),
        if (!is.null(p$allampolgarsag)) tags$span(class = "pp-badge", p$allampolgarsag),
        if (!is.null(p$kor))            tags$span(class = "pp-badge", paste0(p$kor, " \u00e9v")),
        if (!is.null(p$pozicio))        tags$span(class = "pp-badge", p$pozicio),
        if (!is.null(p$mag))            tags$span(class = "pp-badge", paste0(p$mag, " m")),
        if (!is.null(p$lab))            tags$span(class = "pp-badge", p$lab),
        if (is_valogatott)              tags$span(class = "pp-badge pp-badge-valogatott", "V\u00e1logatott"),
        if (!is.null(p$tm_ertek))       tags$span(class = "pp-badge", paste0("TM: ", fmt_ertek(p$tm_ertek)))
      )
    ),

    # ── Tézis
    tags$div(class = "pp-tezis", p$tezis),

    # ── Kulcs mutatók (NB1 rang)
    if (!is.null(mutatok) && length(mutatok) > 0)
      tags$div(
        class = "pp-section",
        tags$h3(paste0("NB1 rangsor \u2014 ", primary_pos, " (2025\u201326)")),
        tags$div(
          class = "pp-metrics-grid",
          lapply(mutatok, function(m) {
            pct <- if (!is.null(m$pct)) m$pct else 0.5
            badge_col <- if (pct >= 0.80) "#1a7a4a"
                         else if (pct >= 0.60) "#4caf50"
                         else if (pct <= 0.20) "#C43A28"
                         else if (pct <= 0.40) "#e57373"
                         else "#7A756D"
            tags$div(
              class = "pp-metric-card",
              style = paste0("border-top: 3px solid ", badge_col, ";"),
              tags$div(class = "pp-metric-value", as.character(m$value)),
              tags$div(class = "pp-metric-label", m$label),
              if (!is.null(m$context))
                tags$div(class = "pp-metric-context",
                  style = paste0("color:", badge_col, ";font-weight:600;"),
                  m$context)
            )
          })
        )
      ),

    # ── Elemzés
    tags$div(
      class = "pp-section",
      tags$h3("Elemz\u00e9s"),
      tags$div(class = "pp-elemzes", multiline_text(p$elemzes))
    ),

    # ── Forgatókönyvek
    tags$div(
      class = "pp-section",
      tags$h3("Forgat\u00f3k\u00f6nyvek"),
      tags$div(
        class = "pp-scenarios",
        tags$div(
          class = "pp-scenario pp-scenario-bull",
          tags$div(class = "pp-scenario-header", "Bull case"),
          tags$p(p$bull_case$szoveg),
          tags$div(class = "pp-scenario-metric", p$bull_case$meroeszam)
        ),
        tags$div(
          class = "pp-scenario pp-scenario-base",
          tags$div(class = "pp-scenario-header", "Base case"),
          tags$p(p$base_case$szoveg),
          tags$div(class = "pp-scenario-metric", p$base_case$meroeszam)
        ),
        tags$div(
          class = "pp-scenario pp-scenario-bear",
          tags$div(class = "pp-scenario-header", "Bear case"),
          tags$p(p$bear_case$szoveg),
          tags$div(class = "pp-scenario-metric", p$bear_case$meroeszam)
        )
      )
    ),

    # ── Értékelés-történet
    if (!is.null(p$rating_history) && length(p$rating_history) > 0) {
      tags$div(
        class = "pp-section",
        tags$h3("\u00c9rt\u00e9kel\u00e9s-t\u00f6rt\u00e9net"),
        tags$div(
          class = "pp-rating-history",
          lapply(rev(p$rating_history), function(h) {
            tags$div(
              class = "pp-history-item",
              tags$div(
                class = "pp-history-left",
                tags$span(class = "pp-history-date", as.character(h$datum)),
                rating_badge(h$besorolas)
              ),
              tags$div(
                class = "pp-history-right",
                tags$div(class = "pp-history-indok", h$indok),
                if (!is.null(h$kimenet))
                  tags$div(
                    class = "pp-history-kimenet",
                    style = outcome_style(h$kimenet),
                    as.character(h$kimenet)
                  )
              )
            )
          })
        )
      )
    },

    # ── Dátum footer
    if (!is.null(p$publikalt) || !is.null(p$frissitve)) {
      tags$div(
        class = "pp-footer-info",
        if (!is.null(p$publikalt))
          tags$div(class = "pp-footer-item",
            tags$span(class = "pp-footer-label", "Publik\u00e1lva:"),
            tags$span(as.character(p$publikalt))
          ),
        if (!is.null(p$frissitve))
          tags$div(class = "pp-footer-item",
            tags$span(class = "pp-footer-label", "Utolj\u00e1ra szerkesztve:"),
            tags$span(as.character(p$frissitve))
          ),
        tags$div(class = "pp-footer-item",
          tags$span(class = "pp-footer-label", "Adatok forr\u00e1sa:"),
          tags$span("Twelve Football")
        )
      )
    }

  ) # end tagList
}
