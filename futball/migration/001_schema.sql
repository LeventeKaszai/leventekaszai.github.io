-- Football pilot migráció: identity / football / football_lab sémák
-- Hozzáférés-vezérlés: EZEK A SÉMÁK SOSEM kerülnek fel a Supabase "Exposed schemas"
-- listájára. Minden adatelérés Next.js API route-okon keresztül, service_role
-- kulccsal, szerver oldalon történik -- nincs közvetlen PostgREST/GraphQL elérés
-- sem a public, sem a lab táblákhoz.

CREATE SCHEMA IF NOT EXISTS identity;
CREATE SCHEMA IF NOT EXISTS football;
CREATE SCHEMA IF NOT EXISTS football_lab;

-- ─────────────────────────────────────────────────────────────────────────
-- identity: sportágakon átívelő játékos-disambiguation
-- ─────────────────────────────────────────────────────────────────────────

CREATE TABLE identity.player_identity (
  player_uid     UUID PRIMARY KEY,
  canonical_name TEXT NOT NULL,
  birth_date     DATE,
  primary_sport  TEXT NOT NULL,
  created_at     TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE identity.player_identity_source_map (
  sport             TEXT NOT NULL,
  source_system     TEXT NOT NULL,           -- 'twelve' | 'transfermarkt'
  source_player_id  TEXT NOT NULL,           -- player_id (Twelve) vagy player_url (TM)
  player_uid        UUID NOT NULL REFERENCES identity.player_identity(player_uid),
  match_confidence  TEXT NOT NULL,           -- 'auto' | 'auto_r2a' | 'auto_r2b' | 'auto_r2c' | 'manual' | 'unmatched'
  matched_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (sport, source_system, source_player_id)
);

-- ─────────────────────────────────────────────────────────────────────────
-- football: publikálásra kész, letisztított (API route-ok ezt olvassák
-- cikkekhez/vizualizációkhoz)
-- ─────────────────────────────────────────────────────────────────────────

CREATE TABLE football.matches (
  match_id   INTEGER PRIMARY KEY,
  match_date DATE NOT NULL,
  season     TEXT NOT NULL,
  league     TEXT NOT NULL,
  home_team  TEXT NOT NULL,
  away_team  TEXT NOT NULL,
  home_goals INTEGER,
  away_goals INTEGER
);

CREATE TABLE football.player_profiles (
  player_uid        UUID PRIMARY KEY REFERENCES identity.player_identity(player_uid),
  display_name      TEXT NOT NULL,
  position          TEXT,
  current_team      TEXT,
  nationality       TEXT,
  birth_date        DATE,
  transfermarkt_url TEXT
);

CREATE TABLE football.player_season_stats (
  player_uid  UUID NOT NULL REFERENCES identity.player_identity(player_uid),
  season      TEXT NOT NULL,
  league      TEXT NOT NULL,
  quality     TEXT NOT NULL,
  kpi         TEXT NOT NULL,
  value_per90 REAL,
  rank        INTEGER,
  rank_of     INTEGER,
  PRIMARY KEY (player_uid, season, league, quality, kpi)
);

CREATE TABLE football.team_season_stats (
  team_name TEXT NOT NULL,
  season    TEXT NOT NULL,
  league    TEXT NOT NULL,
  quality   TEXT NOT NULL,
  kpi       TEXT NOT NULL,
  value     REAL,
  rank      INTEGER,
  rank_of   INTEGER,
  PRIMARY KEY (team_name, season, league, quality, kpi)
);

-- ─────────────────────────────────────────────────────────────────────────
-- football_lab: nyers scrape / közbenső modell-output (nem publikálásra,
-- csak a saját Analytics Lab admin nézetnek és API route-ok belső
-- számításainak)
-- ─────────────────────────────────────────────────────────────────────────

CREATE TABLE football_lab.tm_rosters (
  comp_name              TEXT,
  region                 TEXT,
  country                TEXT,
  season_start_year      REAL,
  squad                  TEXT,
  player_num             TEXT,
  player_name            TEXT,
  player_position        TEXT,
  player_dob             DATE,             -- SQLite-ban REAL sorszám volt, DATE-re konvertálva
  player_age             REAL,
  player_nationality     TEXT,
  current_club           TEXT,
  player_height_mtrs     TEXT,
  player_foot            TEXT,
  date_joined            TEXT,
  joined_from            TEXT,
  contract_expiry        TEXT,
  player_market_value_euro REAL,
  player_url             TEXT,
  liga                   TEXT
);

CREATE TABLE football_lab.coach_spells (
  team             TEXT,
  coach            TEXT,
  spell_start_id   INTEGER,
  spell_start_date DATE,
  spell_end_id     INTEGER,
  spell_end_date   DATE,                   -- utolsó megerősített meccs-dátum edzőként (mindig kitöltve)
  is_censored_end  BOOLEAN NOT NULL,        -- TRUE: a vég dátuma csak az adatgyűjtés jelenlegi határa, NEM megerősített távozás (nincs hozzá coach_changes bejegyzés)
  source           TEXT
);

CREATE TABLE football_lab.coach_changes (
  team             TEXT,
  szezon           TEXT,
  out_coach        TEXT,
  in_coach         TEXT,
  match_before_id  INTEGER,
  match_before_date DATE,
  match_after_id   INTEGER,
  match_after_date DATE
);

CREATE TABLE football_lab.coach_adjusted_ratings (
  team          TEXT,
  coach         TEXT,
  n_matches     INTEGER,
  xt_adj_diff   REAL,
  xt_adj_se     REAL,
  npxg_adj_diff REAL,
  npxg_adj_se   REAL,
  adj_xt        REAL,
  adj_oppxt     REAL,
  adj_npxg      REAL,
  adj_oppnpxg   REAL
);

CREATE TABLE football_lab.team_finances (
  team_canonical  TEXT,
  season          TEXT NOT NULL,             -- szezon_std-ből, kanonikus "YYYY-YY" formátum
  season_raw      TEXT,                      -- eredeti vegyes formátumú szezon-mező, audit célra
  csapat          TEXT,
  naptari_ev_kezdeten TEXT,
  naptari_ev_vegen    TEXT,
  ertekesites_netto_arbevetele REAL,
  egyeb_bevetelek REAL,
  osszesen REAL,
  szemelyi_jellegu_raforditas REAL,
  adozas_elotti_eredmeny REAL,
  adozott_eredmeny REAL,
  sajat_toke REAL,
  osszes_forras REAL,
  forgoeszkozok REAL,
  jatekjogok_merleg REAL,
  szponzoracio REAL,
  berlet_ertekesites REAL,
  jegyertekesites REAL,
  reklamdij REAL,
  egyeb_bevetel_19 REAL,
  sportletesitmeny_es_marketing_jogok_hasznositasa REAL,
  tv_es_egyeb_jogdijak REAL,
  vagyoni_erteku_jogokbol_befolyo REAL,
  webaruhaz REAL,
  sportakademiai_tamogatas REAL,
  latvanycsapatsport_tamogatas REAL,
  onkormanyzati_tamogatas REAL,
  egyeb_bevetel_27 REAL,
  berjarulek REAL,
  utazas_szallas_edzotaboroztatas REAL,
  berleti_dijak REAL,
  jatekosugynokok REAL,
  nevezes_versenyeztetes REAL,
  sportegeszsegugyi_szolgaltatasok REAL,
  hirdetesi_koltsegek REAL,
  biztonsagi_szolgaltatas_koltsegei REAL,
  etkezesi_koltseg REAL,
  merkozesnapi_igenybevett_szolgaltatasok REAL,
  letesitmennyel_kapcsolatos_igenybe_vett_szolg REAL,
  marketingkoltsegek REAL,
  mezszponzor REAL,
  tulajdonosi_tamogatas REAL,
  szakszovetsegi_tamogatas REAL,
  tao REAL,
  letszam REAL,
  helyezes REAL,
  nb1_pontok_idoszak_a_szezon_kezdete REAL,
  lott_gol REAL,
  kapott_gol REAL,
  nb2_pontok REAL,
  nb2_lott_gol REAL,
  nb2_kapott_gol REAL,
  market_value_m REAL,
  average_market_value_ezer REAL,
  average_age_tm REAL,
  average_age_fbref REAL,
  foreigners_tm REAL,
  squad_size REAL,
  foreigners_rate REAL,
  atlag_hazai_nezoszam_magyarfutball_hu REAL,
  kiadas_ezer_eur REAL,
  erkezok REAL,
  bevetel_ezer_eur REAL,
  tavozok REAL,
  coaches_during_season REAL,
  coaches_during_season_without_caretaker REAL,
  players_used_fbref REAL,
  hazai_jatekosok_aranya_percent REAL
);

CREATE TABLE football_lab.team_finances_season (
  team       TEXT,
  season     TEXT NOT NULL,
  osszesen   REAL,
  szemelyi_jellegu_raforditas REAL,
  forras     TEXT
);

CREATE TABLE football_lab.nb1_model_dataset (
  szezon      TEXT,
  team        TEXT,
  total_market_value_euro REAL,
  avg_age     REAL,
  squad_size  INTEGER,
  foreigners  INTEGER,
  pts_per_match REAL,
  promoted    BOOLEAN
);

CREATE TABLE football_lab.nb2_model_dataset (
  szezon      TEXT,
  team        TEXT,
  total_market_value_euro REAL,
  avg_age     REAL,
  squad_size  INTEGER,
  foreigners  INTEGER,
  pts_per_match REAL,
  promoted    BOOLEAN
);

CREATE TABLE football_lab.nb1_squad_values_historical (
  season_start_year REAL,
  szezon TEXT,
  squad  TEXT,
  squad_size INTEGER,
  avg_age REAL,
  foreigners INTEGER,
  avg_market_value_euro REAL,
  total_market_value_euro REAL,
  team_canonical TEXT
);

CREATE TABLE football_lab.nb2_squad_values_historical (
  season_start_year REAL,
  szezon TEXT,
  squad  TEXT,
  squad_size INTEGER,
  avg_age REAL,
  foreigners INTEGER,
  avg_market_value_euro REAL,
  total_market_value_euro REAL,
  team_canonical TEXT
);

CREATE TABLE football_lab.nb2_squad_values_historical_raw (
  season_start_year REAL,
  szezon TEXT,
  squad  TEXT,
  squad_size INTEGER,
  avg_age REAL,
  foreigners INTEGER,
  avg_market_value_euro REAL,
  total_market_value_euro REAL
);

CREATE TABLE football_lab.nb1_final_tables_historical (
  szezon TEXT,
  team_canonical TEXT,
  team_tm TEXT,
  rank INTEGER,
  pl INTEGER,
  w INTEGER,
  d INTEGER,
  l INTEGER,
  gf INTEGER,
  ga INTEGER,
  gd INTEGER,
  pts INTEGER
);

CREATE TABLE football_lab.nb2_tables_historical_raw (
  season_start_year REAL,
  szezon TEXT,
  rang INTEGER,
  squad TEXT,
  played INTEGER,
  pts INTEGER
);

CREATE TABLE football_lab.nb1_2627_goal_estimates (
  team TEXT,
  exp_goals REAL,
  exp_opp_goals REAL,
  forras TEXT,
  gd REAL,
  generalva DATE
);

CREATE TABLE football_lab.nb1_2627_poisson_params (
  league_avg_goals REAL,
  home_factor REAL,
  away_factor REAL,
  rho_fit REAL,
  generalva DATE
);

CREATE TABLE football_lab.nb1_2627_preseason_forecast (
  team TEXT,
  helyezes INTEGER,
  pred_pont_per_meccs REAL,
  pred_pontszam_33meccsre REAL,
  modell TEXT,
  generalva DATE
);

CREATE TABLE football_lab.nb1_2627_round1_ah_odds (
  home TEXT,
  away TEXT,
  line REAL,
  odds_home REAL,
  odds_away REAL,
  p_home_cover REAL,
  generalva DATE
);

CREATE TABLE football_lab.nb1_2627_round1_poisson_odds (
  home TEXT,
  away TEXT,
  lambda_h REAL,
  lambda_a REAL,
  p_home REAL,
  p_draw REAL,
  p_away REAL,
  p_over25 REAL,
  p_btts REAL,
  odds_1 REAL,
  odds_x REAL,
  odds_2 REAL,
  odds_over25 REAL,
  odds_btts REAL,
  generalva DATE
);

CREATE TABLE football_lab.nb2_2627_preseason_forecast (
  team TEXT,
  helyezes INTEGER,
  pred_pont_per_meccs REAL,
  pred_pontszam_30meccsre REAL,
  modell TEXT,
  generalva DATE
);

CREATE TABLE football_lab.value_model_coefs (
  feature     TEXT,
  coefficient REAL,
  position    TEXT
);

CREATE TABLE football_lab.value_model_residuals (
  player_uid          UUID REFERENCES identity.player_identity(player_uid),
  player_url          TEXT,
  squad                TEXT,
  position             TEXT,
  label                TEXT,
  log_value_actual     REAL,
  log_value_predicted  REAL,
  residual             REAL,
  value_actual_eur     REAL,
  value_predicted_eur  REAL,
  value_gap_pct        REAL
);

CREATE TABLE football_lab.match_team_stats_long (
  match_id  INTEGER,
  team      TEXT,
  opponent  TEXT,
  home_away TEXT,
  kpi       TEXT,
  value     REAL
);

-- 2026-08-04: a Next.js Lab (sportsanalytics) player_url-re és squad-ra
-- LATERAL joinolva kérdezi le a legfrissebb szezon tm_rosters sorát minden
-- játékosra/csapatra (player list, player profile, team roster) -- index
-- nélkül ez teljes tábla-scan volt joinonként (663 sorra kb. 200ms egy
-- lekérdezésen belül). Index nélkül végzett EXPLAIN ANALYZE ezt igazolta,
-- indexeléssel ~3ms-re esett.
CREATE INDEX IF NOT EXISTS idx_tm_rosters_player_url_season
  ON football_lab.tm_rosters (player_url, season_start_year DESC);
CREATE INDEX IF NOT EXISTS idx_tm_rosters_squad_season
  ON football_lab.tm_rosters (squad, season_start_year DESC);
