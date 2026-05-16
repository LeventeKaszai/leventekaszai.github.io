library(DBI)
library(RSQLite)
library(dplyr)

con <- dbConnect(SQLite(), "bball_10_tables.db")

# ── Adatok ────────────────────────────────────────────────────────────────────
s <- dbGetQuery(con, "SELECT * FROM player_scoring_stats")
a <- dbGetQuery(con, "SELECT * FROM player_ast_reb_stats")
adv <- dbGetQuery(con, "SELECT * FROM player_advanced_stats")
oo  <- dbGetQuery(con, 'SELECT Játékos, Szezon, Csapat, "Net RTG" AS net_onoff, "Min%" AS min_pct FROM on_off_stats')
ts  <- dbGetQuery(con, 'SELECT Season, Csapat, "Gy%" AS gy_pct FROM team_stats')
dbDisconnect(con)

# Szezon TEXT → Season INT (pl. "2025-26" → 2526)
szezon_to_int <- function(x) {
  parts <- strsplit(x, "-")
  sapply(parts, function(p) as.integer(paste0(substr(p[1], 3, 4), p[2])))
}

# ── BPM + DRE számítás (analytics.qmd logikája) ──────────────────────────────
raw <- s %>%
  inner_join(a,   by = c("Játékos", "Szezon", "Csapat")) %>%
  inner_join(adv, by = c("Játékos", "Szezon", "Csapat")) %>%
  rename(
    Perc          = Min.x,
    min_pg        = `Min PG.x`,
    pts_100       = `PTS 100`,
    twop_att_100  = `2P att. 100`,
    threep_att_100= `3P att. 100`,
    fga_100       = `FG att. 100`,
    fta_100       = `FT att. 100`,
    orb_100       = `ORB 100`,
    drb_100       = `DRB 100`,
    ast_100       = `AST 100`,
    tov_100       = `TOV 100`,
    blk_100       = `BLK 100`,
    pf_100        = `PF 100`,
    stl_raw       = STL,
    tov_raw       = TOV,
    ast_pct       = `AST%`,
    tov_pct       = `TOV%`,
    orb_pct       = `ORB%`,
    drb_pct       = `DRB%`,
    stl_pct       = `STL%`,
    blk_pct       = `BLK%`,
    usg_pct       = `USG%`,
    ortg          = ORtg,
    net_rtg       = `Net Rtg`
  )

raw <- raw %>%
  group_by(Szezon) %>%
  mutate(
    lg_ortg  = weighted.mean(ortg[Perc >= 200], Perc[Perc >= 200], na.rm = TRUE),
    obpm_raw = (ortg - lg_ortg) * (usg_pct / 100) * 0.55 +
               ast_pct * 0.09 - tov_pct * 0.10 + orb_pct * 0.10,
    dbpm_raw = drb_pct * 0.045 + stl_pct * 0.70 + blk_pct * 0.35,
    lg_obpm  = mean(obpm_raw[Perc >= 200], na.rm = TRUE),
    lg_dbpm  = mean(dbpm_raw[Perc >= 200], na.rm = TRUE),
    BPM      = round(obpm_raw - lg_obpm + dbpm_raw - lg_dbpm, 1)
  ) %>%
  ungroup() %>%
  mutate(
    stl_100 = ifelse(tov_raw > 0, stl_raw * (tov_100 / tov_raw), NA_real_),
    DRE = round(
      -8.424 + 0.792 * pts_100 - 0.719 * twop_att_100 - 0.552 * threep_att_100
      - 0.159 * fta_100 + 0.135 * orb_100 + 0.400 * drb_100 + 0.544 * ast_100
      + 1.680 * stl_100 + 0.764 * blk_100 - 1.360 * tov_100 - 0.108 * pf_100,
      1)
  )

base <- raw %>%
  select(Játékos, Szezon, Csapat, Perc, BPM, DRE, net_rtg)

cat("\n════════════════════════════════════════════════════════\n")
cat("   DRE vs BPM — Validáció (NB1, 2022-23 → 2025-26)\n")
cat("════════════════════════════════════════════════════════\n")

# ── TESZT 1: Csapat Gy% korreláció ───────────────────────────────────────────
cat("\n── 1. TESZT: Csapat győzelmi arány korreláció ──\n")
cat("   (perc-súlyozott csapat átlag BPM/DRE vs Gy%)\n\n")

team_metrics <- base %>%
  filter(!is.na(BPM), !is.na(DRE), Perc >= 100) %>%
  group_by(Szezon, Csapat) %>%
  summarise(
    w_BPM = weighted.mean(BPM, Perc, na.rm = TRUE),
    w_DRE = weighted.mean(DRE, Perc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Season = szezon_to_int(Szezon)) %>%
  inner_join(ts, by = c("Season", "Csapat"))

r_bpm <- cor(team_metrics$w_BPM, team_metrics$gy_pct, use = "complete.obs")
r_dre <- cor(team_metrics$w_DRE, team_metrics$gy_pct, use = "complete.obs")

cat(sprintf("   BPM  → r = %.3f  (R² = %.3f)\n", r_bpm, r_bpm^2))
cat(sprintf("   DRE  → r = %.3f  (R² = %.3f)\n", r_dre, r_dre^2))
cat(sprintf("   Győztes: %s  (ΔR² = %.3f)\n",
    ifelse(r_dre^2 > r_bpm^2, "DRE ✓", "BPM ✓"), abs(r_dre^2 - r_bpm^2)))

# ── TESZT 2: On-off Net RTG korreláció ───────────────────────────────────────
cat("\n── 2. TESZT: On-off Net RTG korreláció ──\n")
cat("   (egyéni BPM/DRE vs mért on/off Net RTG, min 200 perc)\n\n")

oo_join <- base %>%
  filter(Perc >= 200, !is.na(BPM), !is.na(DRE)) %>%
  inner_join(
    oo %>% filter(!is.na(net_onoff)),
    by = c("Játékos", "Szezon", "Csapat")
  )

r2_bpm_oo <- cor(oo_join$BPM,       oo_join$net_onoff, use = "complete.obs")
r2_dre_oo <- cor(oo_join$DRE,       oo_join$net_onoff, use = "complete.obs")

cat(sprintf("   n = %d játékos-szezon páros\n", nrow(oo_join)))
cat(sprintf("   BPM  → r = %.3f  (R² = %.3f)\n", r2_bpm_oo, r2_bpm_oo^2))
cat(sprintf("   DRE  → r = %.3f  (R² = %.3f)\n", r2_dre_oo, r2_dre_oo^2))
cat(sprintf("   Győztes: %s  (ΔR² = %.3f)\n",
    ifelse(r2_dre_oo^2 > r2_bpm_oo^2, "DRE ✓", "BPM ✓"),
    abs(r2_dre_oo^2 - r2_bpm_oo^2)))

# ── TESZT 3: Év-év stabilitás ────────────────────────────────────────────────
cat("\n── 3. TESZT: Év-év stabilitás (prediktív erő) ──\n")
cat("   (szezon N metrika → szezon N+1 metrika, min 200 perc mindkét évben)\n\n")

szn_order <- c("2022-23" = 1, "2023-24" = 2, "2024-25" = 3, "2025-26" = 4)

yoy <- base %>%
  filter(Perc >= 200, !is.na(BPM), !is.na(DRE)) %>%
  mutate(szn_idx = szn_order[Szezon]) %>%
  inner_join(
    base %>%
      filter(Perc >= 200, !is.na(BPM), !is.na(DRE)) %>%
      mutate(szn_idx = szn_order[Szezon]) %>%
      select(Játékos, szn_idx, BPM_next = BPM, DRE_next = DRE),
    by = c("Játékos")
  ) %>%
  filter(szn_idx.y == szn_idx.x + 1)

r_yoy_bpm <- cor(yoy$BPM, yoy$BPM_next, use = "complete.obs")
r_yoy_dre <- cor(yoy$DRE, yoy$DRE_next, use = "complete.obs")

cat(sprintf("   n = %d visszatérő játékos\n", nrow(yoy)))
cat(sprintf("   BPM  → r = %.3f  (R² = %.3f)\n", r_yoy_bpm, r_yoy_bpm^2))
cat(sprintf("   DRE  → r = %.3f  (R² = %.3f)\n", r_yoy_dre, r_yoy_dre^2))
cat(sprintf("   Győztes: %s  (ΔR² = %.3f)\n",
    ifelse(r_yoy_dre^2 > r_yoy_bpm^2, "DRE ✓", "BPM ✓"),
    abs(r_yoy_dre^2 - r_yoy_bpm^2)))

# ── ÖSSZEFOGLALÓ ─────────────────────────────────────────────────────────────
cat("\n════════════════════════════════════════════════════════\n")
cat("   ÖSSZEFOGLALÓ\n")
cat("════════════════════════════════════════════════════════\n")
scores <- c(
  ifelse(r_dre^2    > r_bpm^2,    "DRE", "BPM"),
  ifelse(r2_dre_oo^2 > r2_bpm_oo^2, "DRE", "BPM"),
  ifelse(r_yoy_dre^2 > r_yoy_bpm^2, "DRE", "BPM")
)
tests <- c("Csapat Gy%", "On-off Net RTG", "Év-év stabilitás")
for (i in 1:3) cat(sprintf("   %-20s → %s\n", tests[i], scores[i]))
cat(sprintf("\n   Végeredmény: DRE %d–%d BPM\n",
    sum(scores == "DRE"), sum(scores == "BPM")))
cat("════════════════════════════════════════════════════════\n\n")
