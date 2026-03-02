"""
Script to fetch necessary data from nba_api, only use of python in the project.
"""
import json
from pathlib import Path
from nba_api.stats.endpoints import leaguedashptdefend, leaguedashplayerstats

DATA_DIR = Path("data/raw")
DATA_DIR.mkdir(parents=True, exist_ok=True)

def save_raw_json(data, filename):
    """Helper to save data into a json file"""
    filepath = DATA_DIR / f"{filename}.json"
    with open(filepath, "w", encoding='utf-8') as f:
        json.dump(data, f, indent=4)
    print(f"--- Captured: {filepath} ---")


def fetch_castle_raw_materials(season="2023-24"):
    """Function to fetch from nba_api, saves output into json"""
    print(f"Starting extraction for {season} season...")

    # Add more parts to function to fetch more types of data
    # 1. Player-defense stats
    player_tracking = leaguedashptdefend.LeagueDashPtDefend(
        season=season
    ).get_dict()
    save_raw_json(player_tracking, f"player_tracking_{season.replace('-', '_')}")

    # 2. General advanced stats
    adv_stats = leaguedashplayerstats.LeagueDashPlayerStats(
        season=season,
        measure_type_detailed_defense="Advanced"
    ).get_dict()
    save_raw_json(adv_stats, f"player_advanced_{season.replace('-', '_')}")

if __name__ == "__main__":
    # can loop through more seasons here if we want more historical data
    seasons = ["2024-25", "2025-26"]
    for s in seasons:
        fetch_castle_raw_materials(s)
