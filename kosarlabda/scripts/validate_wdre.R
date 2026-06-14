library(DBI); library(RSQLite); library(dplyr)

con <- dbConnect(SQLite(), "bball_10_tables.db")
s_raw   <- dbGetQuery(con, "SELECT * FROM player_scoring_stats")
a_raw   <- dbGetQuery(con, "SELECT * FROM player_ast_reb_stats")
adv_raw <- dbGetQuery(con, "SELECT * FROM player_advanced_stats")
pi_raw  <- dbGetQuery(con, 'SELECT Játékos, "Szül. év." AS szul_ev FROM player_info')
dbDisconnect(con)

pi_dedup <- pi_raw |>
  filter(!is.na(szul_ev)) |>
  group_by(Játékos) |>
  summarise(szul_ev = max(szul_ev), .groups = "drop")

s <- s_raw |> select(Játékos, Szezon, Csapat, Min,
  pts_100=`PTS 100`, twop_att_100=`2P att. 100`,
  threep_att_100=`3P att. 100`, fta_100=`FT att. 100`)
a <- a_raw |> select(Játékos, Szezon, Csapat,
  tov_raw=TOV, stl_raw=STL,
  ast_100=`AST 100`, tov_100=`TOV 100`, orb_100=`ORB 100`,
  drb_100=`DRB 100`, blk_100=`BLK 100`, pf_100=`PF 100`,
  ast_pct=`AST%`, tov_pct=`TOV%`, orb_pct=`ORB%`,
  drb_pct=`DRB%`, stl_pct=`STL%`, blk_pct=`BLK%`)
adv <- adv_raw |> select(Játékos, Szezon, Csapat,
  ortg=ORtg, usg_pct=`USG%`, net_rtg=`Net Rtg`)

raw <- s |>
  inner_join(a,   by = c("Játékos","Szezon","Csapat")) |>
  inner_join(adv, by = c("Játékos","Szezon","Csapat")) |>
  mutate(stl_100 = ifelse(tov_raw>0, stl_raw*(tov_100/tov_raw), NA_real_))

# ── DRE és BPM számítás ───────────────────────────────────────────────────────
raw <- raw |>
  mutate(DRE = round(
    -8.424 + 0.792*pts_100 - 0.719*twop_att_100 - 0.552*threep_att_100
    - 0.159*fta_100 + 0.135*orb_100 + 0.400*drb_100 + 0.544*ast_100
    + 1.680*stl_100 + 0.764*blk_100 - 1.360*tov_100 - 0.108*pf_100, 2)) |>
  group_by(Szezon) |>
  mutate(
    lg_ortg  = weighted.mean(ortg[Min>=200], Min[Min>=200], na.rm=TRUE),
    obpm_raw = (ortg-lg_ortg)*(usg_pct/100)*0.55 +
               ast_pct*0.09 - tov_pct*0.10 + orb_pct*0.10,
    dbpm_raw = drb_pct*0.045 + stl_pct*0.70 + blk_pct*0.35,
    lg_obpm  = mean(obpm_raw[Min>=200], na.rm=TRUE),
    lg_dbpm  = mean(dbpm_raw[Min>=200], na.rm=TRUE),
    BPM      = round(obpm_raw-lg_obpm + dbpm_raw-lg_dbpm, 2)
  ) |>
  ungroup() |>
  select(-lg_ortg,-obpm_raw,-dbpm_raw,-lg_obpm,-lg_dbpm)

# ── CV minimális percküszöb ───────────────────────────────────────────────────
MIN_PERC <- 200   # módosítható: 100 / 200 / 500

# ── Aging curve: kvadratikus illesztés játékos-szintű ΔDRE adatokon ───────────
szn_idx_age <- c("2022-23"=1,"2023-24"=2,"2024-25"=3,"2025-26"=4)

aging_pairs <- raw |>
  filter(Min >= MIN_PERC, !is.na(DRE)) |>
  left_join(pi_dedup, by="Játékos") |>
  filter(!is.na(szul_ev)) |>
  mutate(age = as.integer(substr(Szezon,1,4)) - szul_ev,
         idx = szn_idx_age[Szezon]) |>
  inner_join(
    raw |> filter(Min>=MIN_PERC,!is.na(DRE)) |>
      mutate(idx_next=szn_idx_age[Szezon]) |>
      select(Játékos, idx_next, DRE_next=DRE),
    by="Játékos"
  ) |>
  filter(idx_next == idx + 1) |>
  mutate(delta_DRE = DRE_next - DRE)

aging_fit <- lm(delta_DRE ~ age + I(age^2), data=aging_pairs,
                weights=aging_pairs$Min)

b0 <- coef(aging_fit)[1]
b1 <- coef(aging_fit)[2]
b2 <- coef(aging_fit)[3]
peak_age <- -b1 / (2 * b2)

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  Kvadratikus aging görbe illesztés\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat(sprintf("  ΔDRE = %.4f + %.4f·age + %.6f·age²\n", b0, b1, b2))
cat(sprintf("  Csúcskor: %.1f év\n", peak_age))
cat(sprintf("  n = %d egymást követő szezonpár\n\n", nrow(aging_pairs)))

# Összehasonlítás: kvadratikus vs bucket
cat(sprintf("  %-6s  %8s  %8s\n", "Kor", "Kvadr.", "Bucket"))
cat(rep("-",28),"\n",sep="")
bucket_vals <- c("18"=0.925,"19"=0.925,"20"=0.925,"21"=0.925,
                 "22"=0.330,"23"=0.330,"24"=0.330,
                 "25"=0.275,"26"=0.275,"27"=0.275,
                 "28"=-0.389,"29"=-0.389,"30"=-0.389,
                 "31"=-0.552,"32"=-0.552,"33"=-0.552,
                 "34"=-0.331,"35"=-0.331,"36"=-0.331,"37"=-0.331)
for (age in c(19,21,23,25,27,29,31,33,35)) {
  quad_val <- b0 + b1*age + b2*age^2
  bkt_val  <- bucket_vals[as.character(age)]
  cat(sprintf("  %-6d  %8.3f  %8.3f\n", age, quad_val, bkt_val))
}

get_aging_delta <- function(kor) b0 + b1*kor + b2*kor^2

# ── Bayesian shrinkage k-értékek (empirikus, optimize_weights.R) ──────────────
K <- list(pts=125, p2a=110, p3a=90, fta=210, orb=130,
          drb=160, ast=95, stl=730, blk=265, tov=285, pf=160)
K_DRE    <- 400
BETA     <- 0.5
ENS_MID  <- 350   # logisztikus átmenet közepe (percben)
ENS_STEEP <- 50   # meredekség

shrink <- function(x, lg, min, k) (min*x + k*lg) / (min + k)

# Ligaátlagok szezonon belül (200+ perc)
lg_means <- raw |> filter(Min>=200) |> group_by(Szezon) |>
  summarise(across(c(pts_100,twop_att_100,threep_att_100,fta_100,
                     orb_100,drb_100,ast_100,stl_100,blk_100,tov_100,pf_100),
                   \(x) mean(x,na.rm=TRUE), .names="lg_{.col}"), .groups="drop")

per_szn <- raw |> left_join(lg_means, by="Szezon") |>
  mutate(
    s_pts = shrink(pts_100, lg_pts_100, Min, K$pts),
    s_2pa = shrink(twop_att_100, lg_twop_att_100, Min, K$p2a),
    s_3pa = shrink(threep_att_100, lg_threep_att_100, Min, K$p3a),
    s_fta = shrink(fta_100, lg_fta_100, Min, K$fta),
    s_orb = shrink(orb_100, lg_orb_100, Min, K$orb),
    s_drb = shrink(drb_100, lg_drb_100, Min, K$drb),
    s_ast = shrink(ast_100, lg_ast_100, Min, K$ast),
    s_stl = shrink(stl_100, lg_stl_100, Min, K$stl),
    s_blk = shrink(blk_100, lg_blk_100, Min, K$blk),
    s_tov = shrink(tov_100, lg_tov_100, Min, K$tov),
    s_pf  = shrink(pf_100,  lg_pf_100,  Min, K$pf),
    DRE_shr = round(
      -8.424 + 0.792*s_pts - 0.719*s_2pa - 0.552*s_3pa
      - 0.159*s_fta + 0.135*s_orb + 0.400*s_drb + 0.544*s_ast
      + 1.680*s_stl + 0.764*s_blk - 1.360*s_tov - 0.108*s_pf, 2)
  )

szn_idx <- c("2022-23"=1,"2023-24"=2,"2024-25"=3,"2025-26"=4)
szn_list <- names(szn_idx)

# ── Time-series CV: predict szn N+1 from data up to szn N ────────────────────
# Target: DRE_raw in the next season (min 100 perc)
# Predictors computed using ONLY data up to szn N

results <- list()

for (target_szn in szn_list[-1]) {  # 2023-24, 2024-25, 2025-26
  t_idx  <- szn_idx[target_szn]
  past_szns <- szn_list[szn_idx[szn_list] < t_idx]

  # Target: játékosok akik a következő szezonban legalább MIN_PERC percet játszottak
  target <- per_szn |>
    filter(Szezon == target_szn, Min >= MIN_PERC) |>
    select(Játékos, DRE_target=DRE, BPM_target=BPM, Min_target=Min)

  # Baseline: single-season (csak az előző szezon)
  prev_szn <- szn_list[t_idx - 1]
  baseline <- per_szn |>
    filter(Szezon == prev_szn, Min >= MIN_PERC) |>
    select(Játékos, DRE_prev=DRE_shr, BPM_prev=BPM, Min_prev=Min)

  # wDRE: minden elérhető past szezon decay-súlyozva (eredeti: dupla shrinkage)
  wdre_past <- per_szn |>
    filter(Szezon %in% past_szns, Min >= MIN_PERC, !is.na(DRE_shr)) |>
    mutate(
      dt    = t_idx - szn_idx[Szezon],
      decay = BETA^dt
    ) |>
    group_by(Játékos) |>
    summarise(
      eff_min   = sum(Min * decay, na.rm=TRUE),
      wDRE_obs  = sum(DRE_shr * Min * decay, na.rm=TRUE) /
                  sum(Min * decay, na.rm=TRUE),
      wDRE      = wDRE_obs * eff_min / (eff_min + K_DRE),
      .groups   = "drop"
    )

  # Shrinkage nélküli változat: nyers DRE decay-súlyozva + aging
  prev_szn_year <- as.integer(substr(prev_szn, 1, 4))
  wdre_raw_past <- per_szn |>
    filter(Szezon %in% past_szns, Min >= MIN_PERC, !is.na(DRE)) |>
    mutate(dt = t_idx - szn_idx[Szezon], decay = BETA^dt) |>
    group_by(Játékos) |>
    summarise(
      wDRE_raw = sum(DRE * Min * decay, na.rm=TRUE) / sum(Min * decay, na.rm=TRUE),
      .groups  = "drop"
    ) |>
    left_join(pi_dedup, by="Játékos") |>
    mutate(
      kor          = prev_szn_year - szul_ev,
      aging_adj    = get_aging_delta(kor),
      wDRE_raw_age = wDRE_raw + aging_adj
    )

  # Csak aging: előző szezon raw DRE + aging delta (se shrinkage, se multi-season)
  prev_only_age <- raw |>
    filter(Szezon == prev_szn, Min >= MIN_PERC, !is.na(DRE)) |>
    left_join(pi_dedup, by="Játékos") |>
    mutate(
      kor          = prev_szn_year - szul_ev,
      aging_adj    = get_aging_delta(kor),
      DRE_prev_raw = DRE,
      DRE_prev_age = DRE + aging_adj
    ) |>
    select(Játékos, DRE_prev_raw, DRE_prev_age)

  joined <- target |>
    inner_join(baseline,    by="Játékos") |>
    inner_join(wdre_past,   by="Játékos") |>
    inner_join(wdre_raw_past |> select(Játékos, wDRE_raw, wDRE_raw_age), by="Játékos") |>
    inner_join(prev_only_age, by="Játékos") |>
    filter(!is.na(DRE_prev), !is.na(wDRE), !is.na(DRE_target)) |>
    mutate(
      ens_w    = 1 / (1 + exp(-(Min_prev - ENS_MID) / ENS_STEEP)),
      DRE_ens  = ens_w * DRE_prev + (1 - ens_w) * wDRE_raw_age
    )

  rmse   <- function(pred, act) sqrt(mean((pred - act)^2, na.rm=TRUE))
  topk   <- function(pred, act, k) {
    length(intersect(order(pred, decreasing=TRUE)[1:k],
                     order(act,  decreasing=TRUE)[1:k])) / k
  }

  k10 <- min(10, floor(nrow(joined) * 0.3))
  k20 <- min(20, floor(nrow(joined) * 0.5))

  results[[target_szn]] <- list(
    szn               = target_szn,
    n                 = nrow(joined),
    # RMSE
    rmse_dre          = rmse(joined$DRE_prev,      joined$DRE_target),
    rmse_bpm          = rmse(joined$BPM_prev,      joined$DRE_target),
    rmse_wdre         = rmse(joined$wDRE,          joined$DRE_target),
    rmse_wdre_raw     = rmse(joined$wDRE_raw,      joined$DRE_target),
    rmse_wdre_raw_age = rmse(joined$wDRE_raw_age,  joined$DRE_target),
    rmse_dre_raw      = rmse(joined$DRE_prev_raw,  joined$DRE_target),
    rmse_dre_age      = rmse(joined$DRE_prev_age,  joined$DRE_target),
    rmse_ens          = rmse(joined$DRE_ens,        joined$DRE_target),
    rmse_naive        = rmse(rep(0, nrow(joined)), joined$DRE_target),
    # Spearman rang-korreláció
    sp_dre            = cor(joined$DRE_prev,      joined$DRE_target, method="spearman"),
    sp_wdre_raw_age   = cor(joined$wDRE_raw_age,  joined$DRE_target, method="spearman"),
    sp_ens            = cor(joined$DRE_ens,        joined$DRE_target, method="spearman"),
    # Top-K pontosság
    k10 = k10, k20 = k20,
    topk10_dre          = topk(joined$DRE_prev,     joined$DRE_target, k10),
    topk10_wdre_raw_age = topk(joined$wDRE_raw_age, joined$DRE_target, k10),
    topk10_ens          = topk(joined$DRE_ens,       joined$DRE_target, k10),
    topk20_dre          = topk(joined$DRE_prev,     joined$DRE_target, k20),
    topk20_wdre_raw_age = topk(joined$wDRE_raw_age, joined$DRE_target, k20),
    topk20_ens          = topk(joined$DRE_ens,       joined$DRE_target, k20),
    # joined mentése nested K-hoz
    data = joined
  )
}

# ── 1. RMSE összehasonlítás ───────────────────────────────────────────────────
models <- c("DRE(shrk)","wDRE(raw+age)","Ensemble","Naiv(0)")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  RMSE összehasonlítás — min =", MIN_PERC, "perc\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat(sprintf("  %-12s  %4s  %10s  %13s  %10s  %8s\n",
    "Target szn","n","DRE(shrk)","wDRE(raw+age)","Ensemble","Naiv(0)"))
cat(rep("-",65),"\n",sep="")

tot <- c(n=0,dre=0,rawage=0,ens=0,naive=0)
for (r in results) {
  cat(sprintf("  %-12s  %4d  %10.3f  %13.3f  %10.3f  %8.3f\n",
      r$szn, r$n, r$rmse_dre, r$rmse_wdre_raw_age, r$rmse_ens, r$rmse_naive))
  tot["n"]      <- tot["n"]      + r$n
  tot["dre"]    <- tot["dre"]    + r$rmse_dre           * r$n
  tot["rawage"] <- tot["rawage"] + r$rmse_wdre_raw_age  * r$n
  tot["ens"]    <- tot["ens"]    + r$rmse_ens            * r$n
  tot["naive"]  <- tot["naive"]  + r$rmse_naive          * r$n
}
cat(rep("-",65),"\n",sep="")
cat(sprintf("  %-12s  %4d  %10.3f  %13.3f  %10.3f  %8.3f\n",
    "Súly. átl", tot["n"],
    tot["dre"]/tot["n"], tot["rawage"]/tot["n"],
    tot["ens"]/tot["n"], tot["naive"]/tot["n"]))

rmse_fin <- c(tot["dre"],tot["rawage"],tot["ens"]) / tot["n"]
cat(sprintf("\n  Legjobb RMSE: %s (%.3f)\n", models[which.min(rmse_fin)], min(rmse_fin)))

# ── 2. Spearman rang-korreláció ───────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  Spearman rang-korreláció\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat(sprintf("  %-12s  %4s  %10s  %13s  %10s\n",
    "Target szn","n","DRE(shrk)","wDRE(raw+age)","Ensemble"))
cat(rep("-",55),"\n",sep="")

tot_sp <- c(n=0,dre=0,rawage=0,ens=0)
for (r in results) {
  cat(sprintf("  %-12s  %4d  %10.4f  %13.4f  %10.4f\n",
      r$szn, r$n, r$sp_dre, r$sp_wdre_raw_age, r$sp_ens))
  tot_sp["n"]      <- tot_sp["n"]      + r$n
  tot_sp["dre"]    <- tot_sp["dre"]    + r$sp_dre           * r$n
  tot_sp["rawage"] <- tot_sp["rawage"] + r$sp_wdre_raw_age  * r$n
  tot_sp["ens"]    <- tot_sp["ens"]    + r$sp_ens            * r$n
}
cat(rep("-",55),"\n",sep="")
cat(sprintf("  %-12s  %4d  %10.4f  %13.4f  %10.4f\n",
    "Súly. átl", tot_sp["n"],
    tot_sp["dre"]/tot_sp["n"], tot_sp["rawage"]/tot_sp["n"], tot_sp["ens"]/tot_sp["n"]))

sp_fin <- c(tot_sp["dre"],tot_sp["rawage"],tot_sp["ens"]) / tot_sp["n"]
cat(sprintf("\n  Legjobb Spearman: %s (%.4f)\n", models[which.max(sp_fin)], max(sp_fin)))

# ── 3. Top-K pontosság ────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  Top-K pontosság  (|predikált top-K ∩ valós top-K| / K)\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat(sprintf("  %-12s  %3s  %3s  %8s  %13s  %10s  │  %8s  %13s  %10s\n",
    "Target szn","K10","K20",
    "DRE top10","wDRE+age t10","Ens top10",
    "DRE top20","wDRE+age t20","Ens top20"))
cat(rep("-",90),"\n",sep="")

for (r in results) {
  cat(sprintf("  %-12s  %3d  %3d  %8.2f  %13.2f  %10.2f  │  %8.2f  %13.2f  %10.2f\n",
      r$szn, r$k10, r$k20,
      r$topk10_dre, r$topk10_wdre_raw_age, r$topk10_ens,
      r$topk20_dre, r$topk20_wdre_raw_age, r$topk20_ens))
}

# ── 4. Nested K diagnosztika ──────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  Nested K diagnosztika — leakage ellenőrzés\n")
cat("  (K-értékek csak múltbeli szezonokból számolva)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

stat_cols_k <- c("pts_100","twop_att_100","threep_att_100","fta_100",
                 "orb_100","drb_100","ast_100","stl_100","blk_100","tov_100","pf_100")

compute_nested_k <- function(past_data) {
  sapply(stat_cols_k, function(col) {
    d <- past_data |> filter(Min >= MIN_PERC, !is.na(.data[[col]])) |>
      select(Játékos, val=all_of(col))
    if (nrow(d) < 10) return(K[[match(col, names(K))]])
    gm   <- mean(d$val)
    pm   <- d |> group_by(Játékos) |> summarise(m=mean(val), nn=n(), .groups="drop")
    n_pl <- nrow(pm)
    n_ob <- nrow(d)
    n0   <- (n_ob - sum(pm$nn^2)/n_ob) / (n_pl - 1)
    ssb  <- sum(pm$nn * (pm$m - gm)^2)
    ssw  <- sum((d$val - d |> left_join(pm |> select(Játékos,m),by="Játékos") |>
                   pull(m))^2)
    msb  <- ssb / (n_pl - 1)
    msw  <- ssw / (n_ob - n_pl)
    icc  <- max((msb - msw) / (msb + (n0-1)*msw), 0)
    if (icc < 0.01) return(2000)
    round((1 - icc) / icc * n0)
  })
}

cat(sprintf("  %-20s  %8s  %8s  %8s\n", "Stat","K(global)","K(nested2)","K(nested3)"))
cat(rep("-",50),"\n",sep="")

past2 <- raw |> filter(Szezon %in% c("2022-23","2023-24"))
past3 <- raw |> filter(Szezon %in% c("2022-23","2023-24","2024-25"))
k_nested2 <- compute_nested_k(past2)
k_nested3 <- compute_nested_k(past3)
k_global  <- unlist(K)

for (i in seq_along(stat_cols_k)) {
  s <- stat_cols_k[i]
  cat(sprintf("  %-20s  %8.0f  %8.0f  %8.0f\n",
      s, k_global[i], pmin(k_nested2[s],2000), pmin(k_nested3[s],2000)))
}

# Nested K RMSE az utolsó foldra (2025-26)
past_data_n <- raw |> filter(Szezon %in% c("2022-23","2023-24","2024-25"))
k_n <- compute_nested_k(past_data_n)
lg_n <- past_data_n |> filter(Min>=200) |> group_by(Szezon) |>
  summarise(across(all_of(stat_cols_k), \(x) mean(x,na.rm=TRUE), .names="lg_{.col}"),
            .groups="drop")

per_nested <- raw |> filter(Szezon=="2024-25") |>
  left_join(lg_n |> filter(Szezon=="2024-25"), by="Szezon") |>
  mutate(
    DRE_nested_shr = round(
      -8.424
      + 0.792  * ((Min*pts_100         + k_n["pts_100"]*lg_pts_100)         / (Min+k_n["pts_100"]))
      - 0.719  * ((Min*twop_att_100    + k_n["twop_att_100"]*lg_twop_att_100)/ (Min+k_n["twop_att_100"]))
      - 0.552  * ((Min*threep_att_100  + k_n["threep_att_100"]*lg_threep_att_100)/(Min+k_n["threep_att_100"]))
      - 0.159  * ((Min*fta_100         + k_n["fta_100"]*lg_fta_100)          / (Min+k_n["fta_100"]))
      + 0.135  * ((Min*orb_100         + k_n["orb_100"]*lg_orb_100)          / (Min+k_n["orb_100"]))
      + 0.400  * ((Min*drb_100         + k_n["drb_100"]*lg_drb_100)          / (Min+k_n["drb_100"]))
      + 0.544  * ((Min*ast_100         + k_n["ast_100"]*lg_ast_100)          / (Min+k_n["ast_100"]))
      + 1.680  * ((Min*stl_100         + k_n["stl_100"]*lg_stl_100)          / (Min+k_n["stl_100"]))
      + 0.764  * ((Min*blk_100         + k_n["blk_100"]*lg_blk_100)          / (Min+k_n["blk_100"]))
      - 1.360  * ((Min*tov_100         + k_n["tov_100"]*lg_tov_100)          / (Min+k_n["tov_100"]))
      - 0.108  * ((Min*pf_100          + k_n["pf_100"]*lg_pf_100)            / (Min+k_n["pf_100"])),
    2)
  )

target_26 <- results[["2025-26"]]$data
nested_join <- target_26 |>
  inner_join(per_nested |> select(Játékos, DRE_nested_shr), by="Játékos") |>
  filter(!is.na(DRE_nested_shr))

rmse_global_26 <- results[["2025-26"]]$rmse_dre
rmse_nested_26 <- sqrt(mean((nested_join$DRE_nested_shr - nested_join$DRE_target)^2, na.rm=TRUE))

cat(sprintf("\n  Leakage teszt (2025-26 target, n=%d):\n", nrow(nested_join)))
cat(sprintf("  DRE(shrink, global K):  RMSE = %.3f\n", rmse_global_26))
cat(sprintf("  DRE(shrink, nested K):  RMSE = %.3f\n", rmse_nested_26))
cat(sprintf("  Leakage-hatás:          ΔRMSE = %+.3f\n", rmse_nested_26 - rmse_global_26))

# ── β sweep: wDRE(raw+age) különböző decay értékekkel ────────────────────────
cat("\n══════════════════════════════════════════════════════════════════════════════\n")
cat("  β sweep — wDRE(raw+age), shrinkage nélkül\n")
cat("══════════════════════════════════════════════════════════════════════════════\n\n")
cat(sprintf("  %6s  %7s  %7s\n", "β", "R²", "RMSE"))
cat(rep("-", 28), "\n", sep="")

beta_seq <- seq(0.10, 0.95, by = 0.05)
rmse_fn  <- function(p, a) sqrt(mean((p - a)^2, na.rm=TRUE))

beta_results <- lapply(beta_seq, function(beta) {
  r2_list <- numeric(0); rmse_list <- numeric(0); n_list <- numeric(0)

  for (target_szn in szn_list[-1]) {
    t_idx     <- szn_idx[target_szn]
    past_szns <- szn_list[szn_idx[szn_list] < t_idx]
    prev_szn  <- szn_list[t_idx - 1]
    prev_year <- as.integer(substr(prev_szn, 1, 4))

    target <- raw |>
      filter(Szezon == target_szn, Min >= MIN_PERC, !is.na(DRE)) |>
      select(Játékos, DRE_target = DRE)

    wdre_b <- raw |>
      filter(Szezon %in% past_szns, Min >= MIN_PERC, !is.na(DRE)) |>
      mutate(dt = t_idx - szn_idx[Szezon], decay = beta^dt) |>
      group_by(Játékos) |>
      summarise(
        wDRE_raw = sum(DRE * Min * decay, na.rm=TRUE) / sum(Min * decay, na.rm=TRUE),
        .groups = "drop"
      ) |>
      left_join(pi_dedup, by="Játékos") |>
      mutate(
        kor      = prev_year - szul_ev,
        wDRE_age = wDRE_raw + get_aging_delta(kor)
      )

    joined_b <- target |>
      inner_join(wdre_b |> select(Játékos, wDRE_age), by="Játékos") |>
      filter(!is.na(wDRE_age), !is.na(DRE_target))

    if (nrow(joined_b) < 5) next
    r2_list   <- c(r2_list,   cor(joined_b$wDRE_age, joined_b$DRE_target, use="c")^2)
    rmse_list <- c(rmse_list, rmse_fn(joined_b$wDRE_age, joined_b$DRE_target))
    n_list    <- c(n_list,    nrow(joined_b))
  }

  list(
    r2   = sum(r2_list   * n_list) / sum(n_list),
    rmse = sum(rmse_list * n_list) / sum(n_list)
  )
})

best_r2   <- which.max(sapply(beta_results, `[[`, "r2"))
best_rmse <- which.min(sapply(beta_results, `[[`, "rmse"))

for (i in seq_along(beta_seq)) {
  marker <- ""
  if (i == best_r2   && i == best_rmse) marker <- " ← legjobb R² és RMSE"
  else if (i == best_r2)   marker <- " ← legjobb R²"
  else if (i == best_rmse) marker <- " ← legjobb RMSE"
  cat(sprintf("  %6.2f  %7.4f  %7.3f%s\n",
      beta_seq[i], beta_results[[i]]$r2, beta_results[[i]]$rmse, marker))
}
cat(rep("-", 28), "\n", sep="")
cat(sprintf("  β=0.50 (eredeti):  R²=%.4f  RMSE=%.3f\n",
    beta_results[[which(abs(beta_seq - 0.50) < 0.001)]]$r2,
    beta_results[[which(abs(beta_seq - 0.50) < 0.001)]]$rmse))

# ── Mit mér valójában? ────────────────────────────────────────────────────────
cat("══════════════════════════════════════════════════════════════\n")
cat("  Mit mér a wDRE*?\n")
cat("══════════════════════════════════════════════════════════════\n\n")
cat("  [1] Retrospektív (mit teljesített): wDRE_obs (shrinkage nélküli)\n")
cat("  [2] True talent (mennyit ér most): wDRE (+ DRE-szintű shrinkage)\n")
cat("  [3] Forward-looking (mennyit fog): wDRE* (+ aging adj.)\n\n")
cat("  A time-series CV a [2] → [3] lánc prediktív erejét teszteli.\n")
cat("  Referencia: DRE és BPM az N szezon egyetlen előző szezonját\n")
cat("  használja — ez az inherens információhátránya a wDRE*-nak szemben.\n\n")
