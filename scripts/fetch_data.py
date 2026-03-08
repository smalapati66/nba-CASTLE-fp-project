"""
Script to fetch necessary data from nba_api, only use of python in the project.
"""
import json
import time
from collections import defaultdict
from pathlib import Path
from nba_api.stats.endpoints import (
    leaguedashptdefend,
    leaguedashplayerstats,
    leaguehustlestatsplayer,
    teamplayeronoffdetails,
)

DATA_DIR = Path("data/raw")
DATA_DIR.mkdir(parents=True, exist_ok=True)


def save_raw_json(data, filename):
    """Helper to save data into a json file"""
    filepath = DATA_DIR / f"{filename}.json"
    with open(filepath, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=4)
    print(f"--- Captured: {filepath} ---")


def get_resultset_rows(payload):
    """Return first resultSet headers/rows from an nba_api response dict."""
    result_sets = payload.get("resultSets", [])
    if not result_sets:
        return [], []
    rs0 = result_sets[0]
    return rs0.get("headers", []), rs0.get("rowSet", [])


def rows_by_header(payload):
    """Convert first resultSet rows to dicts keyed by header."""
    headers, rows = get_resultset_rows(payload)
    return [dict(zip(headers, row)) for row in rows]


def to_float(value):
    """Best-effort float conversion."""
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def to_int(value):
    """Best-effort int conversion."""
    try:
        return int(value)
    except (TypeError, ValueError):
        return None


def fetch_dict(fetcher_name, factory, retries=3, backoff_seconds=2):
    """Run endpoint fetch with retry/backoff for nba stats timeouts."""
    last_err = None
    for attempt in range(1, retries + 1):
        try:
            payload = factory().get_dict()
            time.sleep(0.5)
            return payload
        except Exception as exc:  # noqa: BLE001 - we want resilient data pulls
            last_err = exc
            print(f"[{fetcher_name}] attempt {attempt}/{retries} failed: {exc}")
            if attempt < retries:
                time.sleep(backoff_seconds * attempt)
    raise last_err


def extract_team_ids(advanced_payload):
    """Collect unique team IDs from advanced player stats."""
    team_ids = set()
    for row in rows_by_header(advanced_payload):
        team_id = to_int(row.get("TEAM_ID"))
        if team_id is not None and team_id > 0:
            team_ids.add(team_id)
    return sorted(team_ids)


def extract_onoff_rows(payload):
    """
    Extract (player_id, on_def, off_def) rows from TeamPlayerOnOffDetails payload.
    Handles slight header variations across versions.
    """
    out = []
    for result_set in payload.get("resultSets", []):
        headers = result_set.get("headers", [])
        rows = result_set.get("rowSet", [])

        pid_key = None
        for candidate in ("VS_PLAYER_ID", "PLAYER_ID"):
            if candidate in headers:
                pid_key = candidate
                break

        on_key = "ON_DEF_RATING" if "ON_DEF_RATING" in headers else None
        off_key = "OFF_DEF_RATING" if "OFF_DEF_RATING" in headers else None

        if not (pid_key and on_key and off_key):
            continue

        for row in rows:
            row_map = dict(zip(headers, row))
            pid = to_int(row_map.get(pid_key))
            on_def = to_float(row_map.get(on_key))
            off_def = to_float(row_map.get(off_key))
            if pid is None or on_def is None or off_def is None:
                continue
            out.append((pid, on_def, off_def))

    return out


def aggregate_onoff_rows(all_rows):
    """
    Aggregate on/off rows by player ID (mean across team contexts if needed).
    Output format mirrors nba_api style for easy parsing downstream.
    """
    grouped = defaultdict(lambda: {"on": [], "off": []})
    for pid, on_def, off_def in all_rows:
        grouped[pid]["on"].append(on_def)
        grouped[pid]["off"].append(off_def)

    row_set = []
    for pid, values in grouped.items():
        on_mean = sum(values["on"]) / len(values["on"])
        off_mean = sum(values["off"]) / len(values["off"])
        row_set.append([pid, on_mean, off_mean])

    row_set.sort(key=lambda r: r[0])

    return {
        "resource": "castle_player_onoff",
        "parameters": {},
        "resultSets": [
            {
                "name": "CastlePlayerOnOff",
                "headers": ["PLAYER_ID", "ON_DEF_RATING", "OFF_DEF_RATING"],
                "rowSet": row_set,
            }
        ],
    }


def fetch_castle_raw_materials(season="2023-24"):
    """Function to fetch from nba_api, saves output into json"""
    print(f"Starting extraction for {season} season...")

    # 1. Player-defense stats (overall)
    player_tracking = fetch_dict(
        "tracking_overall",
        lambda: leaguedashptdefend.LeagueDashPtDefend(season=season, timeout=120),
    )
    save_raw_json(player_tracking, f"player_tracking_{season.replace('-', '_')}")

    # 2. Matchup shot-zone splits
    tracking_2pt = fetch_dict(
        "tracking_2pt",
        lambda: leaguedashptdefend.LeagueDashPtDefend(
            season=season,
            defense_category="2 Pointers",
            timeout=120,
        ),
    )
    save_raw_json(tracking_2pt, f"player_tracking_2pt_{season.replace('-', '_')}")

    tracking_3pt = fetch_dict(
        "tracking_3pt",
        lambda: leaguedashptdefend.LeagueDashPtDefend(
            season=season,
            defense_category="3 Pointers",
            timeout=120,
        ),
    )
    save_raw_json(tracking_3pt, f"player_tracking_3pt_{season.replace('-', '_')}")

    tracking_rim = fetch_dict(
        "tracking_rim",
        lambda: leaguedashptdefend.LeagueDashPtDefend(
            season=season,
            defense_category="Less Than 6Ft",
            timeout=120,
        ),
    )
    save_raw_json(tracking_rim, f"player_tracking_rim_{season.replace('-', '_')}")

    # 3. General advanced stats
    adv_stats = fetch_dict(
        "advanced",
        lambda: leaguedashplayerstats.LeagueDashPlayerStats(
            season=season,
            measure_type_detailed_defense="Advanced",
            timeout=120,
        ),
    )
    save_raw_json(adv_stats, f"player_advanced_{season.replace('-', '_')}")

    # 4. Hustle data for disruptions (steals, blocks, deflections)
    hustle_stats = fetch_dict(
        "hustle",
        lambda: leaguehustlestatsplayer.LeagueHustleStatsPlayer(
            season=season,
            timeout=120,
        ),
    )
    save_raw_json(hustle_stats, f"player_hustle_{season.replace('-', '_')}")

    # 5. Team on/off defense by player (aggregated across teams)
    team_ids = extract_team_ids(adv_stats)
    all_onoff_rows = []
    for team_id in team_ids:
        payload = fetch_dict(
            f"onoff_team_{team_id}",
            lambda team_id=team_id: teamplayeronoffdetails.TeamPlayerOnOffDetails(
                team_id=team_id,
                season=season,
                timeout=120,
            ),
        )
        all_onoff_rows.extend(extract_onoff_rows(payload))

    onoff_payload = aggregate_onoff_rows(all_onoff_rows)
    save_raw_json(onoff_payload, f"player_onoff_{season.replace('-', '_')}")


if __name__ == "__main__":
    # can loop through more seasons here if we want more historical data
    seasons = ["2024-25", "2025-26"]
    for s in seasons:
        fetch_castle_raw_materials(s)
