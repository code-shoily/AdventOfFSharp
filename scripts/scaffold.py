#!/usr/bin/env python3
"""
Scaffold new AoC day files for F#.

Usage:
  python scripts/scaffold.py <year> <day>

Creates:
  Year{YYYY}/Day{DD}.fs   — solution module with boilerplate
  Inputs/{YYYY}_{DD}.txt  — input file (fetched from AoC if AOC_SESSION_KEY is set)

Environment:
  AOC_SESSION_KEY — your Advent of Code session cookie for fetching inputs
"""

import os
import sys
import argparse
from pathlib import Path
from urllib import request

REPO_DIR = Path(__file__).parent.parent.resolve()

BOILERPLATE = '''/// Year {year}/{day} - 
/// Link: https://adventofcode.com/{year}/day/{day}
/// Difficulty: 
/// Tags: 
/// Remarks:
module Year{year}.Day{day_padded}

open Common.Helpers
open Common.Types

let parse (rawInput: string seq) =
    rawInput
    |> Seq.toList

let solvePart1 input =
    0

let solvePart2 input =
    0

let solve (rawInput: string seq) =
    let input = parse rawInput
    BothInt(solvePart1 input, solvePart2 input)
'''


def fetch_input(year: int, day: int) -> str:
    """Fetch input from Advent of Code."""
    session_key = os.environ.get("AOC_SESSION_KEY", "")
    
    if not session_key:
        print("⚠️  AOC_SESSION_KEY not found. Creating empty input file.")
        return ""
    
    url = f"https://adventofcode.com/{year}/day/{day}/input"
    req = request.Request(url)
    req.add_header("Cookie", f"session={session_key}")
    req.add_header("User-Agent", "github.com/mafinar/AdventOfFScaffold.py")
    
    try:
        with request.urlopen(req) as resp:
            if resp.status == 200:
                return resp.read().decode("utf-8")
            else:
                print(f"❌ Failed to fetch input: HTTP {resp.status}")
                return ""
    except Exception as e:
        print(f"❌ Failed to fetch input: {e}")
        return ""


def write_if_missing(path: Path, content: str):
    """Write file only if it doesn't exist."""
    if path.exists():
        print(f"  - Skipping: {path} (exists)")
        return
    
    path.write_text(content, encoding="utf-8")
    print(f"  - Created:  {path}")


def scaffold(year: int, day: int):
    """Create scaffolding for a new day."""
    year_str = str(year)
    day_padded = f"{day:02d}"
    
    # Paths
    year_dir = REPO_DIR / f"Year{year_str}"
    inputs_dir = REPO_DIR / "Inputs"
    
    src_path = year_dir / f"Day{day_padded}.fs"
    input_path = inputs_dir / f"{year_str}_{day_padded}.txt"
    
    # Ensure directories exist
    year_dir.mkdir(exist_ok=True)
    inputs_dir.mkdir(exist_ok=True)
    
    print(f"\n🎯 Scaffolding Year {year} Day {day_padded}")
    print("=" * 40)
    
    # Fetch or create input
    input_content = fetch_input(year, day)
    write_if_missing(input_path, input_content)
    
    # Create solution file
    solution_content = BOILERPLATE.format(
        year=year_str,
        day=day,
        day_padded=day_padded
    )
    write_if_missing(src_path, solution_content)
    
    print("\n✨ Done!")
    print(f"\nNext steps:")
    print(f"  1. Add to Year{year_str}/Year{year_str}.fsproj:")
    print(f'     <Compile Include="Day{day_padded}.fs"/>')
    print(f"  2. Edit {src_path} to add the title, difficulty, and tags")
    print(f"  3. Implement parse, solvePart1, and solvePart2")
    print(f"  4. Input is at: {input_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Scaffold new Advent of Code day files for F#"
    )
    parser.add_argument("year", type=int, help="Year (e.g., 2024)")
    parser.add_argument("day", type=int, help="Day (1-25)")
    
    args = parser.parse_args()
    
    if not (2015 <= args.year <= 2030):
        print(f"Error: Year {args.year} seems out of range")
        sys.exit(1)
    
    if not (1 <= args.day <= 25):
        print(f"Error: Day {args.day} must be between 1 and 25")
        sys.exit(1)
    
    scaffold(args.year, args.day)


if __name__ == "__main__":
    main()
