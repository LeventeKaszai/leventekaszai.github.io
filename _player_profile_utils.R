library(htmltools)

# ── DB stat loader ─────────────────────────────────────────────────────────────

load_player_db_stats <- function(player_name, pozicio_fallback = NULL,
                                 db_path = "bball_10_tables.db") {
  if (!file.exists(db_path)) return(NULL)

  library(DBI)
  library(RSQLite)

  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))

  # Player info: klub, pozicio, kor
  info <- dbGetQuery(con,
    "SELECT Poszt, Csapat AS klub, [Szül. év.] AS szul_ev, [Mag.] AS mag, [Töm.] AS tom
     FROM player_info WHERE Játékos = ? LIMIT 1",
    params = list(player_name)
  )

  pozicio_db <- if (nrow(info) > 0 && !is.na(info$Poszt[1]))   info$Poszt[1]              else pozicio_fallback
  klub_db    <- if (nrow(info) > 0 && !is.na(info$klub[1]))   info$klub[1]               else NULL
  szul_ev_db <- if (nrow(info) > 0 && !is.na(info$szul_ev[1])) as.integer(info$szul_ev[1]) else NULL
  mag_db     <- if (nrow(info) > 0 && !is.na(info$mag[1]))    as.integer(info$mag[1])     else NULL
  tom_db     <- if (nrow(info) > 0 && !is.na(info$tom[1]))    as.integer(info$tom[1])     else NULL

  # Pozíció a metrika-logikához (DB > fallback)
  pozicio <- if (!is.null(pozicio_db)) pozicio_db else pozicio_fallback

  # All seasons, newest first
  scoring <- dbGetQuery(con,
    "SELECT Szezon, Csapat, G, Min,
            [Min PG]  AS min_pg,
            [PTS PG]  AS pts_pg,
            [TS%]     AS ts_pct,
            [3P%]     AS threeP_pct
     FROM player_scoring_stats
     WHERE Játékos = ?
     ORDER BY Szezon DESC",
    params = list(player_name)
  )
  if (nrow(scoring) == 0) return(NULL)
  cur <- scoring[1, ]

  # AST/REB for current season
  ast_reb <- dbGetQuery(con,
    "SELECT [AST PG]   AS ast_pg,
            [AST/ TOV] AS ast_tov,
            [TOV%]     AS tov_pct,
            [TRB%]     AS trb_pct,
            [ORB%]     AS orb_pct,
            [BPG PG]   AS blk_pg
     FROM player_ast_reb_stats
     WHERE Játékos = ? AND Szezon = ? AND Csapat = ?",
    params = list(player_name, cur$Szezon, cur$Csapat)
  )

  # Advanced for current season
  adv <- dbGetQuery(con,
    "SELECT [USG%] AS usg_pct
     FROM player_advanced_stats
     WHERE Játékos = ? AND Szezon = ? AND Csapat = ?",
    params = list(player_name, cur$Szezon, cur$Csapat)
  )


  # League sample for current season (min 200 perc) → percentilis
  league <- dbGetQuery(con,
    "SELECT s.[PTS PG]  AS pts_pg,
            s.[TS%]     AS ts_pct,
            s.[3P%]     AS threeP_pct,
            a.[AST PG]  AS ast_pg,
            a.[AST/ TOV] AS ast_tov,
            a.[TOV%]    AS tov_pct,
            a.[TRB%]    AS trb_pct,
            a.[ORB%]    AS orb_pct,
            a.[BPG PG]  AS blk_pg,
            adv.[USG%]  AS usg_pct
     FROM player_scoring_stats s
     JOIN player_ast_reb_stats   a   USING(Játékos, Szezon, Csapat)
     JOIN player_advanced_stats  adv USING(Játékos, Szezon, Csapat)
     WHERE s.Szezon = ? AND s.Min >= 200",
    params = list(cur$Szezon)
  )

  # Percentilis → szöveges kontextus
  pct_label <- function(val, vec, higher_better = TRUE) {
    if (is.null(val) || is.na(val) || length(vec) == 0) return(NULL)
    vec <- vec[!is.na(vec)]
    if (length(vec) == 0) return(NULL)
    pct <- if (higher_better) mean(vec <= val) else mean(vec >= val)
    if (pct >= 0.90) "Liga top 10%"
    else if (pct >= 0.80) "Liga top 20%"
    else if (pct >= 0.75) "Liga felső negyed"
    else NULL
  }

  # ── jatekpercek ──────────────────────────────────────────────────────────────

  jatekpercek <- list(
    osszes_felnottt  = as.integer(sum(scoring$Min)),
    bajnoki_aktualis = as.integer(cur$Min),
    merkozes         = as.integer(cur$G),
    min_pg           = round(cur$min_pg, 1)
  )

  # ── pozíció-alapú mutatok ────────────────────────────────────────────────────

  pos_code <- toupper(as.character(pozicio))
  is_pg    <- pos_code %in% c("PG", "G")
  is_c     <- pos_code %in% c("C", "FC")
  is_pf    <- pos_code == "PF"

  mutatok <- list()

  if (is_pg) {

    if (nrow(ast_reb) > 0) {
      ar <- ast_reb[1, ]

      ast_val <- round(ar$ast_pg, 1)
      mutatok[[1]] <- list(label = "Assziszt/mérkőzés",
        value   = as.character(ast_val),
        context = pct_label(ast_val, league$ast_pg))

      atov_val <- round(ar$ast_tov, 2)
      mutatok[[2]] <- list(label = "Assziszt/góldobás arány",
        value   = as.character(atov_val),
        context = pct_label(atov_val, league$ast_tov))

      tov_val <- round(ar$tov_pct, 1)
      mutatok[[3]] <- list(label = "Turnover arány",
        value   = paste0(tov_val, "%"),
        context = pct_label(tov_val, league$tov_pct, higher_better = FALSE))
    }

    if (!is.na(cur$threeP_pct) && cur$threeP_pct > 0) {
      tp_val <- round(cur$threeP_pct * 100, 1)
      mutatok[[length(mutatok) + 1]] <- list(label = "Három pontos %",
        value   = paste0(tp_val, "%"),
        context = pct_label(tp_val, league$threeP_pct * 100))
    }

  } else {

    pts_val <- round(cur$pts_pg, 1)
    mutatok[[1]] <- list(label = "Pont/mérkőzés",
      value   = as.character(pts_val),
      context = pct_label(pts_val, league$pts_pg))

    ts_val <- round(cur$ts_pct * 100, 1)
    mutatok[[2]] <- list(label = "True Shooting %",
      value   = paste0(ts_val, "%"),
      context = pct_label(ts_val, league$ts_pct * 100))

    if (nrow(ast_reb) > 0) {
      ar <- ast_reb[1, ]

      trb_val <- round(ar$trb_pct, 1)
      mutatok[[3]] <- list(label = "Lepattanó arány",
        value   = paste0(trb_val, "%"),
        context = pct_label(trb_val, league$trb_pct))

      if (is_c) {
        blk_val <- round(ar$blk_pg, 1)
        mutatok[[4]] <- list(label = "Blokk/mérkőzés",
          value   = as.character(blk_val),
          context = pct_label(blk_val, league$blk_pg))
      } else if (is_pf) {
        orb_val <- round(ar$orb_pct, 1)
        mutatok[[4]] <- list(label = "Tám. lepattanó arány",
          value   = paste0(orb_val, "%"),
          context = pct_label(orb_val, league$orb_pct))
      } else if (nrow(adv) > 0) {
        usg_val <- round(adv$usg_pct[1], 1)
        mutatok[[4]] <- list(label = "Használati arány",
          value   = paste0(usg_val, "%"),
          context = pct_label(usg_val, league$usg_pct))
      }
    }

  }

  list(
    jatekpercek = jatekpercek,
    mutatok     = mutatok,
    klub        = klub_db,
    pozicio     = pozicio_db,
    szul_ev     = szul_ev_db,
    mag         = mag_db,
    tom         = tom_db
  )
}

# ── Render function ────────────────────────────────────────────────────────────

render_player_profile <- function(profile, db_path = "bball_10_tables.db") {
  p <- profile

  # DB-ből automatikusan töltjük be a statisztikákat
  db_stats <- tryCatch(
    load_player_db_stats(p$nev, pozicio_fallback = p$pozicio, db_path = db_path),
    error = function(e) { message("DB betöltési hiba: ", e$message); NULL }
  )

  if (!is.null(db_stats)) {
    p$jatekpercek <- db_stats$jatekpercek
    if (length(db_stats$mutatok) > 0) p$mutatok  <- db_stats$mutatok
    if (!is.null(db_stats$klub))    p$klub    <- db_stats$klub
    if (!is.null(db_stats$pozicio)) p$pozicio <- db_stats$pozicio
    if (!is.null(db_stats$szul_ev)) p$szul_ev <- db_stats$szul_ev
    if (!is.null(db_stats$mag))     p$mag     <- db_stats$mag
    if (!is.null(db_stats$tom))     p$tom     <- db_stats$tom
  }

  # ── Helpers ─────────────────────────────────────────────────────────────────

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

  fmt_num <- function(x) {
    format(as.integer(x), big.mark = "\u00a0")
  }

  # ── Derived values ───────────────────────────────────────────────────────────

  sport_label  <- if (tolower(p$sport) == "basketball") "Kos\u00e1rlabda" else "Labdar\u00fcg\u00e1s"
  is_football  <- tolower(p$sport) == "football"
  is_valogatott <- isTRUE(p$valogatott) ||
    tolower(as.character(p$valogatott)) %in% c("igen", "true", "yes")
  jp <- p$jatekpercek

  # ── Build HTML ────────────────────────────────────────────────────────────────

  tagList(

    # ── Back navigation
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
        tags$span(class = "pp-badge pp-badge-sport", sport_label),
        tags$span(class = "pp-badge", p$klub),
        if (!is.null(p$szul_ev) && !is.na(p$szul_ev))
          tags$span(class = "pp-badge", as.character(p$szul_ev)),
        tags$span(class = "pp-badge", p$pozicio),
        if (!is.null(p$mag) && !is.na(p$mag))
          tags$span(class = "pp-badge", paste0(p$mag, " cm")),
        if (!is.null(p$tom) && !is.na(p$tom))
          tags$span(class = "pp-badge", paste0(p$tom, " kg")),
        if (is_valogatott) tags$span(class = "pp-badge pp-badge-valogatott", "V\u00e1logatott")
      )
    ),

    # ── Tézis
    tags$div(class = "pp-tezis", p$tezis),

    # ── Játékpercek
    if (!is.null(jp)) tags$div(
      class = "pp-section",
      tags$h3("J\u00e1t\u00e9kpercek"),
      tags$div(
        class = "pp-stats-row",
        tags$div(class = "pp-stat-item",
          tags$div(class = "pp-stat-value", fmt_num(jp$osszes_felnottt)),
          tags$div(class = "pp-stat-label", "\u00d6sszes feln\u0151tt perc")
        ),
        tags$div(class = "pp-stat-item",
          tags$div(class = "pp-stat-value", fmt_num(jp$bajnoki_aktualis)),
          tags$div(class = "pp-stat-label", "Bajnoki perc (aktu\u00e1lis szezon)")
        ),
        tags$div(class = "pp-stat-item",
          tags$div(class = "pp-stat-value", as.character(jp$merkozes)),
          tags$div(class = "pp-stat-label", "M\u00e9rk\u0151z\u00e9s")
        ),
        if (!is.null(jp$min_pg))
          tags$div(class = "pp-stat-item",
            tags$div(class = "pp-stat-value", as.character(jp$min_pg)),
            tags$div(class = "pp-stat-label", "Perc/m\u00e9rk\u0151z\u00e9s")
          )
      )
    ),

    # ── Szerződés (csak foci)
    if (is_football && !is.null(p$szerzodes)) {
      sz <- p$szerzodes
      tags$div(
        class = "pp-section",
        tags$h3("Szerz\u0151d\u00e9s"),
        tags$div(
          class = "pp-stats-row",
          tags$div(class = "pp-stat-item",
            tags$div(class = "pp-stat-value", as.character(sz$lejarat)),
            tags$div(class = "pp-stat-label", "Lej\u00e1rat")
          ),
          tags$div(class = "pp-stat-item",
            tags$div(class = "pp-stat-value", as.character(sz$transfermarkt_ertek)),
            tags$div(class = "pp-stat-label",
                     paste0("Transfermarkt-\u00e9rt\u00e9k (", sz$transfermarkt_datum, ")"))
          )
        )
      )
    },

    # ── Posztspecifikus mutatók
    if (!is.null(p$mutatok) && length(p$mutatok) > 0)
      tags$div(
        class = "pp-section",
        tags$h3(class = "pp-type-label", p$type),
        tags$div(
          class = "pp-metrics-grid",
          lapply(p$mutatok, function(m) {
            tags$div(
              class = "pp-metric-card",
              tags$div(class = "pp-metric-value", as.character(m$value)),
              tags$div(class = "pp-metric-label", m$label),
              if (!is.null(m$context))
                tags$div(class = "pp-metric-context", m$context)
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
          )
      )
    }

  ) # end tagList
}
