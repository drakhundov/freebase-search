# Freebase Path Finder

A Haskell implementation of a Freebase path finder. This tool uses Breadth-First Search (BFS) to find the shortest path between two Freebase MIDs (Machine Identifiers).

## Features
- Efficient BFS algorithm for pathfinding.
- Works with Freebase MIDs to identify relationships between entities.

## Prerequisites
- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) installed on your system.

## Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/your-repo/freebase-search.git
   cd freebase-search```
## Usage
1. Run
    ```ghc Main.hs --outputdir=build -o pathfinder && ./pathfinder```