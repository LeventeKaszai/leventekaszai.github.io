"""
Import 2025-26 season data from updated CSVs into bball_10_tables.db
"""
import sqlite3
import csv
import os

BASE = "/Users/kaszailevente/Library/Mobile Documents/com~apple~CloudDocs/Kosárlabda"
DB_PATH = os.path.join(BASE, "bball_10_tables.db")
CSV_DIR = os.path.join(BASE, "basketball_database")

NEW_SEASON = "2025-26"

def read_csv(filename):
    path = os.path.join(CSV_DIR, filename)
    with open(path, encoding="utf-8") as f:
        reader = csv.DictReader(f, delimiter=";")
        return list(reader)

def to_float(val):
    if val is None or val.strip() == "":
        return None
    try:
        return float(val.replace(",", "."))
    except ValueError:
        return None

def to_int(val):
    if val is None or val.strip() == "":
        return None
    try:
        return int(float(val.replace(",", ".")))
    except ValueError:
        return None

con = sqlite3.connect(DB_PATH)
cur = con.cursor()


# ── 1. player_advanced_stats ─────────────────────────────────────────────────
print("=== player_advanced_stats ===")
rows = [r for r in read_csv("player_advanced_stats-Table 1.csv") if r["Szezon"] == NEW_SEASON]
cur.execute("DELETE FROM player_advanced_stats WHERE Szezon = ?", (NEW_SEASON,))
insert_sql = """INSERT INTO player_advanced_stats VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"""
for r in rows:
    cur.execute(insert_sql, (
        r["Játékos"], r["Szezon"], r["Csapat"],
        to_int(r["G"]), to_int(r["Min"]), to_float(r["Min PG"]),
        to_int(r["GmSc"]), to_float(r["GmSc PG"]), to_float(r["GmSc 100"]),
        to_int(r["adj. GmSc"]), to_float(r["adj. GmSc PG"]), to_float(r["adj. GmSc 100"]),
        to_float(r["VAL"]), to_float(r["PER"]), to_float(r["USG%"]),
        to_float(r["% Team Poss"]), to_int(r["PTS Prod"]), to_float(r["Floor %"]),
        to_int(r["ORtg"]), to_int(r["DRtg"]), to_float(r["Net Rtg"]),
        to_float(r["OWS"]), to_float(r["DWS"]), to_float(r["WS"]), to_float(r["WS40"]),
        to_int(r["MVP Index"]), to_int(r["Gy"]), to_int(r["V"]), to_float(r["Gy%"]),
    ))
print(f"  Beillesztve: {len(rows)} sor")


# ── 2. player_scoring_stats ──────────────────────────────────────────────────
print("=== player_scoring_stats ===")
rows = [r for r in read_csv("player_scoring_stats-Table 1.csv") if r["Szezon"] == NEW_SEASON]
cur.execute("DELETE FROM player_scoring_stats WHERE Szezon = ?", (NEW_SEASON,))
insert_sql = """INSERT INTO player_scoring_stats VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"""
for r in rows:
    cur.execute(insert_sql, (
        r["Játékos"], r["Szezon"], r["Csapat"],
        to_int(r["G"]), to_int(r["Min"]), to_float(r["Min PG"]),
        to_int(r["PTS"]), to_float(r["PTS PG"]), to_float(r["PTS 100"]),
        to_int(r["Close made"]), to_int(r["Close att."]),
        to_float(r["Close made PG"]), to_float(r["Close att. PG"]),
        to_float(r["Close made 100"]), to_float(r["Close att. 100"]), to_float(r["Close %"]),
        to_int(r["Mid. made"]), to_int(r["Mid. att."]),
        to_float(r["Mid. made PG"]), to_float(r["Mid. att. PG"]),
        to_float(r["Mid. made 100"]), to_float(r["Mid. att. 100"]), to_float(r["Mid. %"]),
        to_int(r["2P made"]), to_int(r["2P att."]),
        to_float(r["2P made PG"]), to_float(r["2P att. PG"]),
        to_float(r["2P made 100"]), to_float(r["2P att. 100"]), to_float(r["2P%"]),
        to_int(r["3P made"]), to_int(r["3P att."]),
        to_float(r["3P made PG"]), to_float(r["3P att. PG"]),
        to_float(r["3P made 100"]), to_float(r["3P att. 100"]), to_float(r["3P%"]),
        to_int(r["FG made"]), to_int(r["FG att."]),
        to_float(r["FG made PG"]), to_float(r["FG att. PG"]),
        to_float(r["FG made 100"]), to_float(r["FG att. 100"]), to_float(r["FG%"]),
        to_float(r["EFG%"]),
        to_int(r["FT made"]), to_int(r["FT att."]),
        to_float(r["FT made PG"]), to_float(r["FT att. PG"]),
        to_float(r["FT made 100"]), to_float(r["FT att. 100"]), to_float(r["FT%"]),
        to_float(r["TS%"]),
        to_float(r["Close att. rate"]), to_float(r["Mid. att. rate"]),
        to_float(r["3P att. rate"]), to_float(r["FT rate"]),
    ))
print(f"  Beillesztve: {len(rows)} sor")


# ── 3. player_ast_reb_stats ──────────────────────────────────────────────────
print("=== player_ast_reb_stats ===")
rows = [r for r in read_csv("player_ast_reb_stats-Table 1.csv") if r["Szezon"] == NEW_SEASON]
cur.execute("DELETE FROM player_ast_reb_stats WHERE Szezon = ?", (NEW_SEASON,))
insert_sql = """INSERT INTO player_ast_reb_stats VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"""
for r in rows:
    cur.execute(insert_sql, (
        r["Játékos"], r["Szezon"], r["Csapat"],
        to_int(r["G"]), to_int(r["Min"]), to_float(r["Min PG"]),
        to_int(r["AST"]), to_float(r["AST PG"]), to_float(r["AST 100"]), to_float(r["AST%"]),
        to_float(r["AST/ TOV"]),
        to_int(r["TOV"]), to_float(r["TOV PG"]), to_float(r["TOV 100"]), to_float(r["TOV%"]),
        to_int(r["DRB"]), to_int(r["ORB"]), to_int(r["TRB"]),
        to_float(r["DRB PG"]), to_float(r["ORB PG"]), to_float(r["TRB PG"]),
        to_float(r["DRB 100"]), to_float(r["ORB 100"]), to_float(r["TRB 100"]),
        to_float(r["DRB%"]), to_float(r["ORB%"]), to_float(r["TRB%"]),
        to_int(r["PFA"]), to_float(r["PFA PG"]), to_float(r["PFA 100"]),
        to_int(r["PF"]), to_float(r["PF PG"]), to_float(r["PF 100"]),
        to_float(r["Min / 5 PF"]),
        to_int(r["STL"]), to_float(r["STL PG"]), to_float(r["STL%"]), to_float(r["STL/ TOV"]),
        to_int(r["BLK"]), to_float(r["BPG PG"]), to_float(r["BLK 100"]), to_float(r["BLK%"]),
    ))
print(f"  Beillesztve: {len(rows)} sor")


# ── 4. detailed_match_stats ──────────────────────────────────────────────────
print("=== detailed_match_stats ===")
rows = [r for r in read_csv("detailed_match_stats-Table 1.csv") if r["Szezon"] == NEW_SEASON]
cur.execute("DELETE FROM detailed_match_stats WHERE Szezon = ?", (NEW_SEASON,))
placeholders = ",".join(["?"] * 75)
insert_sql = f"INSERT INTO detailed_match_stats VALUES ({placeholders})"
for r in rows:
    cur.execute(insert_sql, (
        r["Dátum"], r["Szezon"], r["Szakasz"], r["Csapat"], r["Ellenfél"], r["Hely"], r["Kimen."],
        to_int(r["PTS"]), to_int(r["Opp. PTS"]), to_int(r["Net PTS"]),
        to_float(r["POSS"]), to_float(r["Pace"]),
        to_float(r["ORTG"]), to_float(r["DRTG"]),
        to_float(r["GmSc"]), to_float(r["Opp. GmSc"]),
        to_int(r["VAL"]), to_int(r["Opp. VAL"]),
        to_int(r["MIN"]),
        to_int(r["2P made"]), to_int(r["2P att."]), to_float(r["2P%"]),
        to_int(r["3P made"]), to_int(r["3P att."]), to_float(r["3P%"]),
        to_float(r["EFG%"]),
        to_int(r["FT made"]), to_int(r["FT att."]), to_float(r["FT%"]),
        to_float(r["FT made rate"]), to_float(r["TS%"]),
        to_int(r["AST"]), to_float(r["AST%"]),
        to_int(r["TOV"]), to_float(r["TOV%"]),
        to_int(r["DRB"]), to_int(r["ORB"]), to_int(r["TRB"]),
        to_float(r["DRB%"]), to_float(r["ORB%"]), to_float(r["TRB%"]),
        to_int(r["STL"]), to_float(r["STL%"]),
        to_int(r["BLK"]), to_float(r["BLK%"]),
        to_int(r["PF"]), to_int(r["PFA"]),
        to_int(r["Opp. 2P made"]), to_int(r["Opp. 2P att."]), to_float(r["Opp. 2P%"]),
        to_int(r["Opp. 3P made"]), to_int(r["Opp. 3P att."]), to_float(r["Opp. 3P%"]),
        to_float(r["Opp. EFG%"]),
        to_int(r["Opp. FT made"]), to_int(r["Opp. FT att."]), to_float(r["Opp. FT%"]),
        to_float(r["Opp. FT made rate"]), to_float(r["Opp. TS%"]),
        to_int(r["Opp. AST"]), to_float(r["Opp. AST%"]),
        to_int(r["Opp. TOV"]), to_float(r["Opp. TOV%"]),
        to_int(r["Opp. DRB"]), to_int(r["Opp. ORB"]), to_int(r["Opp. TRB"]),
        to_float(r["Opp. DRB%"]), to_float(r["Opp. ORB%"]), to_float(r["Opp. TRB%"]),
        to_int(r["Opp. STL"]), to_float(r["Opp. STL%"]),
        to_int(r["Opp. BLK"]), to_float(r["Opp. BLK%"]),
        to_int(r["Opp. PF"]), to_int(r["Opp. PFA"]),
    ))
print(f"  Beillesztve: {len(rows)} sor")


# ── 5. on_off_stats ──────────────────────────────────────────────────────────
# CSV columns: #;Játékos;Szezon;Csapat;Min;Min%;On40;Net40;Net Rtg;Pace;
#              Team PTS 40;Team Rtg;Team Close att. rate;Team 3P att. rate;
#              Team EFG %;Team FT made rate;Team ORB %;Team AST %;Team TOV %;Team BLK %;
#              Opp PTS 40;Opp Rtg;Opp Close att. rate;Opp 3P att. rate;
#              Opp EFG %;Opp FT made rate;Opp ORB %;Opp AST %;Opp TOV %;Opp BLK %
# DB columns:  Játékos, Szezon, Csapat, Min%, On40, Net40, Net RTG, Net Pace,
#              Net Tm PTS 40, Net Tm RTG, Net Tm Close att. rate, Net Tm 3P att. rate,
#              Net Tm EFG %, Net Tm FT made rate, Net Tm ORB %, Net Tm AST %,
#              Net Tm TOV %, Net Tm BLK %,
#              Net Opp PTS 40, Net Opp RTG, Net Opp Close att. rate, Net Opp 3P att. rate,
#              Net Opp EFG %, Net Opp FT made rate, Net Opp ORB %, Net Opp AST %,
#              Net Opp TOV %, Net Opp BLK %
print("=== on_off_stats ===")
rows = [r for r in read_csv("on_off_stats-Table 1.csv") if r.get("Szezon", "").strip() == NEW_SEASON]
cur.execute("DELETE FROM on_off_stats WHERE Szezon = ?", (NEW_SEASON,))
insert_sql = """INSERT INTO on_off_stats VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"""
for r in rows:
    cur.execute(insert_sql, (
        r["Játékos"], r["Szezon"], r["Csapat"],
        to_float(r["Min%"]), to_float(r["On40"]), to_float(r["Net40"]),
        to_float(r["Net Rtg"]),      # → Net RTG
        to_float(r["Pace"]),          # → Net Pace
        to_float(r["Team PTS 40"]),   # → Net Tm PTS 40
        to_float(r["Team Rtg"]),      # → Net Tm RTG
        to_float(r["Team Close att. rate"]),
        to_float(r["Team 3P att. rate"]),
        to_float(r["Team EFG %"]),
        to_float(r["Team FT made rate"]),
        to_float(r["Team ORB %"]),
        to_float(r["Team AST %"]),
        to_float(r["Team TOV %"]),
        to_float(r["Team BLK %"]),
        to_float(r["Opp PTS 40"]),    # → Net Opp PTS 40
        to_float(r["Opp Rtg"]),       # → Net Opp RTG
        to_float(r["Opp Close att. rate"]),
        to_float(r["Opp 3P att. rate"]),
        to_float(r["Opp EFG %"]),
        to_float(r["Opp FT made rate"]),
        to_float(r["Opp ORB %"]),
        to_float(r["Opp AST %"]),
        to_float(r["Opp TOV %"]),
        to_float(r["Opp BLK %"]),
    ))
print(f"  Beillesztve: {len(rows)} sor")


# ── 6. opponent_stats (raw_opponent_stats CSV) ────────────────────────────────
# Szezon "2025-26" → Season 2526; strip " ellenfelek" from Csapat
def szezon_to_int(s):
    # "2025-26" → 2526, "2022-23" → 2223
    parts = s.split("-")
    return int(parts[0][-2:] + parts[1])

print("=== opponent_stats ===")
rows = [r for r in read_csv("raw_opponent_stats-Table 1.csv") if r["Szezon"] == NEW_SEASON]
new_season_int = szezon_to_int(NEW_SEASON)
cur.execute("DELETE FROM opponent_stats WHERE Season = ?", (new_season_int,))
insert_sql = """INSERT INTO opponent_stats VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"""
for r in rows:
    csapat = r["Csapat"].replace(" ellenfelek", "").strip()
    cur.execute(insert_sql, (
        csapat, new_season_int,
        to_int(r["G"]),
        to_float(r["PTS PG"]), to_float(r["Opp. PTS PG"]),
        to_float(r["Pace"]), to_float(r["ORtg"]), to_float(r["DRtg"]), to_float(r["Net RTG"]),
        to_float(r["Close made PG"]), to_float(r["Close att. PG"]), to_float(r["Close %"]),
        to_float(r["Mid. made PG"]), to_float(r["Mid. att. PG"]), to_float(r["Mid. %"]),
        to_float(r["2P made PG"]), to_float(r["2P att. PG"]), to_float(r["2P%"]),
        to_float(r["3P made PG"]), to_float(r["3P att. PG"]), to_float(r["3P%"]),
        to_float(r["FG made PG"]), to_float(r["FG att. PG"]), to_float(r["FG%"]),
        to_float(r["EFG%"]),
        to_float(r["FT made PG"]), to_float(r["FT att. PG"]), to_float(r["FT%"]),
        to_float(r["TS%"]),
        to_float(r["Close att. rate"]), to_float(r["Mid. att. rate"]),
        to_float(r["3P att. rate"]), to_float(r["FT att. rate"]),
        to_float(r["AST PG"]), to_float(r["AST%"]),
        to_float(r["TOV PG"]), to_float(r["TOV%"]),
        to_float(r["DRB PG"]), to_float(r["ORB PG"]), to_float(r["TRB PG"]),
        to_float(r["DRB%"]), to_float(r["ORB%"]), to_float(r["TRB%"]),
        to_float(r["STL PG"]), to_float(r["STL%"]),
        to_float(r["BLK PG"]), to_float(r["BLK%"]),
        to_float(r["PF PG"]), to_float(r["PFA PG"]),
    ))
print(f"  Beillesztve: {len(rows)} sor")


con.commit()
con.close()
print("\nKész! DB frissítve.")
