# User documentation for Haskell Chess Bot

**Filip Kolomazník**

**Neprocedurální programování, NPRG005**

**Summer semester 2025**

## Compilation and Running

To use the program, you will need the Glasgow Haskell Compiler (GHC) installed and compile it using GHC:

```
ghc -O2 -o chessbot ChessBot.hs
```

Or you can use already compiled file `chessbot.exe`

Executable (chessbot) file can be run using this command from the command line:

```
./chessbot
```

The program will then wait for you to input a board configuration string (see format below), and after you press Enter it will calculate and print the best move.

## Input Format

The program expects a single-line string representing the chess board and the current player's turn. The format is:

- 64 characters representing the board squares in row-major order (rank by rank from top to bottom, and file from left to right):

    - Use `'.'` (dot) for an empty square.

    - Use uppercase letters `K, Q, R, B, N, P` for White pieces (King, Queen, Rook, Bishop, Knight, Pawn).

    - Use lowercase letters `k, q, r, b, n, p` for Black pieces.

- Optionally, after the 64 characters, add one letter indicating whose turn it is:

    - `'W'` if it is White's turn (this is the default if no letter is provided).

    - `'B'` if it is Black's turn.

The board order assumes the first 8 characters correspond to the top rank (rank 8) from file `a` to `h`, the next 8 characters to the next rank (rank 7), and so on, down to the last 8 characters for the bottom rank (rank 1). For example, the very first character of the string is the piece at square a8, and the 64th character is the piece at h1.

## Example Input:

```
rnbqkbnrpppppppp................................PPPPPPPPRNBQKBNRW
```

This string represents the initial chess position (Black pieces on rank 8 and 7, empty ranks 6–3, White pieces on ranks 2 and 1) with White to move (the last character `W`).

## Output Format

The program will output a Haskell data structure representation of the chosen move. Moves are represented as:

```
Just (Move fromIndex toIndex promotion)
```

or `Nothing` if no moves are available. Here:

- `fromIndex` is the starting square index (0–63) of the piece to move.

- `toIndex` is the destination square index (0–63).

- `promotion` is `Nothing` for a normal move, or `Just PieceType` if a pawn promotion occurs on this move.

Indices are numbered from 0 to 63 as described above (0 = a8, 7 = h8, 8 = a7, ..., 56 = a1, 63 = h1). For convenience, you can translate these indices to chess notation:

- File (column) = index mod 8 (0→a, 1→b, ..., 7→h)

- Rank (row) = 8 − (index div 8) (since 0 is rank 8, 7 is rank 8, 8 is rank 7, ..., 56 is rank 1)

## Example Output:

```
Just (Move 36 27 Nothing)
```

This output means the best move is from index 36 to index 27 with no promotion. Using the index mapping, index 36 corresponds to e4 and index 27 to d5. In standard chess notation, this move would be "Pawn from e4 to d5" (likely capturing an opponent piece on d5). If the output were `Nothing`, it would indicate no legal moves were available (for instance, if the game state is checkmate or stalemate for the player to move).

## Usage Example

Suppose we have a scenario with a White pawn on e4 (index 36), and Black has a queen on d5 (27) and a rook on f5 (29). We want to find White’s best move:

Input:

```
...........................q.r........P........................W
```

Output:

```
Just (Move 39 30 Nothing)
```

*(Note: The output is not formatted in traditional chess notation, but rather as the internal Move data. It’s mainly intended for developers or as part of a larger system. A user-friendly interface would translate these moves into standard notation.)*