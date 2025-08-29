# Haskell_Chess_AI

## Project Identification

**Name: Haskell Chess Bot**

**Author: Filip Kolomazník**

**Subject: Neprocedurální programování, NPRG005**

**Time Period: Summer semester 2025**

## About the Project

This Haskell program is a simplified chess move selection engine that reads a chess board configuration from input and outputs a recommended move for the current player. It uses the minimax algorithm with alpha-beta pruning to evaluate possible moves up to a fixed depth (4) and chooses the best move according to a basic material evaluation function. The code is written in Haskell and demonstrates how to represent a chess board, generate legal moves for each piece, and implement a minimax search for game AI in a functional style.

## Installation

To run this project, you`ll need:

- GHC (Glasgow Haskell Compiler) – version 9.x or later recommended

- Stack (optional, for dependency management and builds)

## 1. Clome the repository

Clone this repository using this command:

```
git clone https://github.com/WorgenPlayCZ/Haskell_Chess_AI.git
cd Haskell_Chess_AI
```

## 2. Run the Program

Using GHC:

```
ghci
ghci > :load ChessBot.hs
ghci > :main 
```

Now the program wait for input. Input format can be found in User Documentation.