library(DBI); library(RSQLite); library(dplyr)

con <- dbConnect(SQLite(), "bball_10_tables.db")
s_raw   <- dbGetQuery(con, "SELECT * FROM player_scoring_stats")
a_raw   <- dbGetQuery(con, "SELECT * FROM player_ast_reb_stats")
adv_raw <- dbGetQuery(con, "SELECT * FROM player_advanced_stats")
pi_raw  <- dbGetQuery(con, 'SELECT Játékos, "Szül. év." AS szul_ev FROM player_info')
dbDisconnect(con)

s_sel <- s_raw %>% select(Játékos, Szezon, Csapat, Min,
  pts_100=`PTS 100`, twop_att_100=`2P att. 100`,
  threep_att_100=`3P att. 100`, fta_100=`FT att. 100`)
a_sel <- a_raw %>% select(Játékos, Szezon, Csapat,
  tov_raw=TOV, stl_raw=STL,
  ast_100=`AST 100`, tov_100=`TOV 100`, orb_100=`ORB 100`,
  drb_100=`DRB 100`, blk_100=`BLK 100`, pf_100=`PF 100`)

raw <- s_sel %>%
  inner_join(a_sel, by=c("Játékos","Szezon","Csapat")) %>%
  mutate(stl_100 = ifelse(tov_raw>0, stl_raw*(tov_100/tov_raw), NA_real_))

dre <- function(d) round(
  -8.424 + 0.792*d$pts_100 - 0.719*d$twop_att_100 - 0.552*d$threep_att_100
  - 0.159*d$fta_100 + 0.135*d$orb_100 + 0.400*d$drb_100 + 0.544*d$ast_100
  + 1.680*d$stl_100 + 0.764*d$blk_100 - 1.360*d$tov_100 - 0.108*d$pf_100, 2)

raw <- raw %>% mutate(DRE = dre(.))

szn_idx <- c("2022-23"=1,"2023-24"=2,"2024-25"=3,"2025-26"=4)

# ═══════════════════════════════════════════════════════════════════
# 1. OPTIMÁLIS DECAY β  (leave-one-season-out cross-val)
# ═══════════════════════════════════════════════════════════════════
cat("══════════════════════════════════════════════\n")
cat(" 1. OPTIMÁLIS DECAY β\n")
cat("══════════════════════════════════════════════\n\n")

# Minden egymást követő párra (N→N+1): súlyozott múlt-DRE vs következő szezon
# β ∈ [0.2, 0.95], lépés 0.05
pairs_df <- raw %>%
  filter(Min >= 200, !is.na(DRE)) %>%
  mutate(idx = szn_idx[Szezon]) %>%
  inner_join(
    raw %>% filter(Min >= 200, !is.na(DRE)) %>%
      mutate(idx_next = szn_idx[Szezon]) %>%
      select(Játékos, idx_next, DRE_next=DRE),
    by="Játékos"
  ) %>%
  filter(idx_next == idx + 1)

beta_seq <- seq(0.20, 0.95, by=0.025)
cv_results <- sapply(beta_seq, function(beta) {
  weighted_past <- pairs_df %>%
    group_by(Játékos, idx_next) %>%
    summarise(
      wDRE_past = weighted.mean(DRE, beta^(idx_next - idx) * Min, na.rm=TRUE),
      DRE_next  = first(DRE_next),
      .groups="drop"
    )
  cor(weighted_past$wDRE_past, weighted_past$DRE_next, use="complete.obs")^2
})

best_beta_idx <- which.max(cv_results)
best_beta     <- beta_seq[best_beta_idx]

cat(sprintf("  Optimális β = %.3f  (R² = %.4f)\n", best_beta, cv_results[best_beta_idx]))
cat(sprintf("  β=0.50 (eredeti) R² = %.4f\n\n",
    cv_results[which(abs(beta_seq - 0.50) < 0.001)]))

cat("  β      R²\n")
for (i in seq_along(beta_seq)) {
  marker <- if (i == best_beta_idx) " ← optimum" else ""
  cat(sprintf("  %.3f  %.4f%s\n", beta_seq[i], cv_results[i], marker))
}

# ═══════════════════════════════════════════════════════════════════
# 2. EMPIRIKUS BAYESIAN k-ÉRTÉKEK  (variancia-dekompozíció)
# ═══════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════\n")
cat(" 2. EMPIRIKUS BAYESIAN k-ÉRTÉKEK\n")
cat("══════════════════════════════════════════════\n\n")

# k = σ²_error / σ²_talent
# σ²_error  = átlagos intra-player szezonközi variancia
# σ²_talent = inter-player variancia - σ²_error/G_átlag
# Közelítés: ICC alapú

stat_cols <- c("pts_100","twop_att_100","threep_att_100","fta_100",
               "orb_100","drb_100","ast_100","stl_100","blk_100","tov_100","pf_100")

multi_szn <- raw %>%
  filter(Min >= 200, !is.na(DRE)) %>%
  group_by(Játékos) %>% filter(n() >= 2) %>% ungroup()

k_results <- sapply(stat_cols, function(col) {
  d <- multi_szn %>%
    select(Játékos, Szezon, Min, val=all_of(col)) %>%
    filter(!is.na(val))

  # ICC: intraclass correlation
  grand_mean  <- mean(d$val, na.rm=TRUE)

  # Between-player variance
  player_means <- d %>% group_by(Játékos) %>%
    summarise(m=mean(val,na.rm=TRUE), n=n(), .groups="drop")
  ss_between <- sum(player_means$n * (player_means$m - grand_mean)^2)
  df_between <- nrow(player_means) - 1

  # Within-player variance
  d2 <- d %>% left_join(player_means %>% select(Játékos,m), by="Játékos")
  ss_within  <- sum((d2$val - d2$m)^2, na.rm=TRUE)
  df_within  <- nrow(d) - nrow(player_means)

  ms_between <- ss_between / df_between
  ms_within  <- ss_within  / df_within

  n0 <- (nrow(d) - sum(player_means$n^2)/nrow(d)) / df_between
  icc <- pmax((ms_between - ms_within) / (ms_between + (n0-1)*ms_within), 0)

  # k = σ²_within / σ²_between_talent
  # Ha ICC = var_talent/(var_talent+var_error), akkor:
  # k = (1-ICC)/ICC * n0  (szezons száma referencia)
  k_emp <- ifelse(icc > 0, round((1-icc)/icc * n0), 9999)
  c(ICC=round(icc,3), k=pmin(k_emp, 2000))
})

k_df <- as.data.frame(t(k_results))
k_df$stat <- rownames(k_df)

cat(sprintf("  %-20s  ICC     k (emp.)  k (orig.)\n", "Stat"))
orig_k <- c(pts_100=600,twop_att_100=450,threep_att_100=550,fta_100=350,
            orb_100=750,drb_100=450,ast_100=450,stl_100=850,blk_100=950,
            tov_100=450,pf_100=350)
for (i in 1:nrow(k_df)) {
  s <- k_df$stat[i]
  cat(sprintf("  %-20s  %.3f   %5.0f     %5.0f\n",
      s, k_df$ICC[i], k_df$k[i], orig_k[s]))
}

# ═══════════════════════════════════════════════════════════════════
# 3. AGING CURVE  (életkori csoportonkénti ΔwDRE)
# ═══════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════\n")
cat(" 3. AGING CURVE\n")
cat("══════════════════════════════════════════════\n\n")

pi_dedup <- pi_raw %>% filter(!is.na(szul_ev)) %>%
  group_by(Játékos) %>% summarise(szul_ev=max(szul_ev), .groups="drop")

age_df <- raw %>%
  filter(Min >= 200, !is.na(DRE)) %>%
  left_join(pi_dedup, by="Játékos") %>%
  filter(!is.na(szul_ev)) %>%
  mutate(
    Kor = as.integer(substr(Szezon,1,4)) - szul_ev,
    idx = szn_idx[Szezon]
  ) %>%
  inner_join(
    raw %>% filter(Min>=200,!is.na(DRE)) %>%
      mutate(idx_next=szn_idx[Szezon]) %>%
      select(Játékos, idx_next, DRE_next=DRE),
    by="Játékos"
  ) %>%
  filter(idx_next == idx+1) %>%
  mutate(delta_DRE = DRE_next - DRE,
         kor_grp   = cut(Kor, breaks=c(17,21,24,27,30,33,45),
                         labels=c("18-21","22-24","25-27","28-30","31-33","34+")))

aging_summary <- age_df %>%
  filter(!is.na(kor_grp)) %>%
  group_by(kor_grp) %>%
  summarise(n=n(), mean_delta=round(mean(delta_DRE,na.rm=TRUE),3),
            sd_delta=round(sd(delta_DRE,na.rm=TRUE),3), .groups="drop")

cat(sprintf("  %-10s  n     ΔDRE/szn   SD\n","Korcsoport"))
for (i in 1:nrow(aging_summary)) {
  cat(sprintf("  %-10s  %-4d  %+.3f      %.3f\n",
      aging_summary$kor_grp[i], aging_summary$n[i],
      aging_summary$mean_delta[i], aging_summary$sd_delta[i]))
}

cat("\n══════════════════════════════════════════════\n")
cat(" ÖSSZEFOGLALÓ — javasolt paraméterek\n")
cat("══════════════════════════════════════════════\n\n")
cat(sprintf("  Optimális decay β  = %.3f\n", best_beta))
cat("  k-értékek: lásd fent (empirikus ICC alapján)\n")
cat("  Aging: lásd ΔDRE/szn táblázat\n\n")
