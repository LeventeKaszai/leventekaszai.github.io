-- NB2 vegleges tabella-tortenet: a nb2_tables_historical_raw (csak M+Pont,
-- kanonizalatlan csapatnevek) helyett teljes W/D/L/GF/GA/GD bontas,
-- kanonizalt csapatnevekkel -- ugyanaz a sema, mint nb1_final_tables_historical.

CREATE TABLE football_lab.nb2_final_tables_historical (
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

DROP TABLE football_lab.nb2_tables_historical_raw;
