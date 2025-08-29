# Developer documentation for Haskell Chess Bot

**Filip Kolomazník**

**Neprocedurální programování, NPRG005**

**Summer semester 2025**

This document explains the structure and logic of the code for programmers who want to understand, maintain, or modify it. The code is organized into several parts: data type definitions, board parsing, move generation, game state evaluation, the minimax search with alpha-beta pruning, and the main I/O loop.

## Data Types and Representations
The program definies custom data types to reprent chess game.

```
data Piece = King | Queen | Rook | Knight | Bishop | Pawn
  deriving (Eq, Show)

data Color = Black | White
  deriving (Eq, Show)

data Figure = Figure Piece Color
  deriving (Eq, Show)

data Cell = Empty | PlacedFigure Figure
  deriving (Eq, Show)

data GameState = GameState
  { board :: Board
  , playerTurn :: Color
  } deriving (Eq, Show)

data Move = Move Position Position (Maybe Piece)
  deriving (Eq, Show)

type Board = ([Cell], Int)
type Position = Int
```
- Piece: An enumeration of the types of chess pieces (King, Queen, Rook, Knight, Bishop, Pawn).

- Color: Color of a piece, either White or Black.

- Figure: A combination of a Piece and a Color, i.e., a specific chess piece belonging to one player.

- Cell: Represents a square on the chess board. It can be `Empty` or contain a `PlacedFigure` with a specific Figure.

- GameState: Encapsulates the current board and which player's turn it is. The board is stored as a `Board` type, and `playerTurn` indicates whose move is next.

- Move: Represents a move from one position to another, possibly with a pawn promotion. It stores:

    - the starting square index (`from`),

    - the target square index (`to`),

    - an optional Piece for promotion (`Maybe Piece` is `Just <piece>` if this move results in a pawn being promoted to that piece, or `Nothing` otherwise).

The **Board** type is defined as `([Cell], Int)`. The first component is a list of 64 `Cell` values, representing the squares of the chessboard in a 0-indexed list. The second component is an `Int` used in this program to count moves (it starts at 0 and increments with each move). This could be used for move counters or tracking half-moves, though in this code it’s mainly incidental (for example, it could be used to track the number of plies or for the fifty-move rule, but no such rule is implemented here).

**Position** is simply an `Int` index into the board list (0 through 63). As described in the user documentation, the indexing scheme is such that:

- 0 corresponds to square a8 (top-left from White’s perspective),

- 7 is h8 (top-right),

- 8 is a7,

- ...,

- 56 is a1 (bottom-left),

- 63 is h1 (bottom-right).

Helper functions to convert between `(file, rank)` coordinates and the linear index are provided:

```
fromIndex :: Int -> (Int, Int)
fromIndex i = (i `mod` 8, i `div` 8)

toIndex :: (Int, Int) -> Int
toIndex (x, y) = y * 8 + x

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8
```

- `fromIndex` converts a linear index to (x,y) coordinates (with x as file 0–7 and y as rank 0–7).

- `toIndex` does the opposite, computing the index from (x,y).

- `inBounds` checks if a coordinate pair is inside the 8x8 board bounds.

## Board Parsing and Initialization

The program reads the board configuration from an input string and converts it into the internal `GameState`. The key functions for this are `parseBoard` and `parseGameState`, along with some helpers:

```
charToCell :: Char -> Cell
charToCell '.' = Empty
charToCell c
  | isAsciiUpper c = PlacedFigure (Figure (toPiece (toUpper c)) White)
  | otherwise      = PlacedFigure (Figure (toPiece (toUpper c)) Black)

toPiece :: Char -> Piece
toPiece 'K' = King
toPiece 'Q' = Queen
toPiece 'R' = Rook
toPiece 'B' = Bishop
toPiece 'N' = Knight
toPiece 'P' = Pawn
toPiece _   = error "Unknown piece"
```

- `charToCell` converts a character from the input string into a `Cell`. A dot `'.'` becomes `Empty`. Any letter is interpreted as a piece: uppercase letters denote White pieces, lowercase denote Black. The letter (case-insensitive) is mapped to a `Piece` type via `toPiece`. For example, `'P'` or `'p'` becomes a Pawn, `'K'` or `'k'` becomes a King, etc. Note: The use of `toUpper` ensures that the piece type is recognized regardless of case, while the guard `isAsciiUpper` sets the color (uppercase = White, lowercase = Black).

- `toPiece` maps a character (`K, Q, R, B, N, P`) to the corresponding `Piece` constructor. If an unexpected character is encountered, it throws an error.

Next, `parseBoard` splits the input string into the board portion and the turn indicator:

```
parseBoard :: String -> Board
parseBoard input =
  let (boardStr, playerChar) = splitAt 64 input
      cells = map charToCell boardStr
      turn = if null playerChar || head playerChar == 'W' then 0 else 1
  in (cells, turn)
```

- It takes the first 64 characters of the input as `boardStr` and converts each character to a `Cell` using `charToCell`. This yields the list of 64 cells.

- It then looks at the 65th character (if present) to determine whose turn it is. If there is no extra character or if it is `'W'`, it sets `turn` to 0 (we interpret 0 to mean White’s turn). If the character is `'B'`, it sets turn to `1` (Black’s turn). Essentially, `turn` is 0 for White and 1 for Black.

- The function returns a `Board` which is the tuple `(cells, turn)`.

Finally, `parseGameState` builds a `GameState` from the input:

```
parseGameState :: String -> GameState
parseGameState input =
  let (cells, turnInt) = parseBoard input
      turn = if turnInt == 0 then White else Black
  in GameState (cells, 0) turn
```

- It uses `parseBoard` to get the cells and turn indicator.

- Converts the numeric turn indicator to the `Color` type `(0 -> White, 1 -> Black)`.

- Initializes a `GameState` with the parsed board (note: it uses `(cells, 0)` for the board, setting the move counter to 0) and the current player's turn as determined.

At this point, the board is loaded into memory as a list of Cells, and the GameState is ready for move generation and evaluation.

## Move Generation

At this point, the board is loaded into memory as a list of Cells, and the GameState is ready for move generation and evaluation.

```
generateMoves :: GameState -> [Move]
generateMoves (GameState (cells, _) player) =
  concat [ generatePieceMoves figure pos cells 
         | (pos, PlacedFigure figure) <- zip [0..] cells
         , figureColor figure == player ]
```

- `generateMoves` scans through all board positions (0 to 63) along with the content of each cell. For each cell that has a `PlacedFigure figure`, it checks if that figure’s color matches the `player` whose turn it is. If yes, it calls `generatePieceMoves figure pos cells` to get all possible moves for that piece from that position.

- It concatenates the lists of moves for all the pieces, resulting in a complete list of legal moves for the current player in the given `GameState`.

The logic for each piece’s moves is handled by `generatePieceMoves`, which dispatches to specific helper functions based on the piece type:

```
generatePieceMoves :: Figure -> Position -> [Cell] -> [Move]
generatePieceMoves (Figure piece color) pos board = case piece of
  Pawn   -> generatePawnMoves color pos board
  Knight -> generateKnightMoves color pos board
  Rook   -> slidingMoves pos color board [(-1, 0), (1, 0), (0, -1), (0, 1)]
  Bishop -> slidingMoves pos color board [(-1, 1), (1, 1), (-1, -1), (1, -1)]
  Queen  -> slidingMoves pos color board 
              [(-1, 0), (1, 0), (0, -1), (0, 1),
               (-1, 1), (1, 1), (-1, -1), (1, -1)]
  King   -> generateKingMoves color pos board
```

This case analysis ensures the correct movement rules are applied for each piece type:

- **Pawn**: uses `generatePawnMoves`

- **Knight**: uses `generateKnightMoves`

- **Rook**: uses `slidingMoves` with straight directions

- **Bishop**: uses `slidingMoves` with diagonal directions

- **Queen**: uses `slidingMoves` with both straight and diagonal directions (eight directions total)

- **King**: uses `generateKingMoves`

Let's go through each briefly:

### Pawn Moves 
– `generatePawnMoves color pos board`:
```
generatePawnMoves :: Color -> Position -> [Cell] -> [Move]
generatePawnMoves color pos board =
  let dir = if color == White then -8 else 8
      (x, y) = fromIndex pos
      oneStep = pos + dir
      twoStep = pos + 2 * dir
      promotionRow = case color of
        White -> 0
        Black -> 7
      validIdx i = i >= 0 && i < 64
      isEmpty i = validIdx i && board !! i == Empty
      isOpponent i = validIdx i && case board !! i of
          PlacedFigure (Figure _ c) -> c /= color
          _ -> False

      -- Forward moves
      forwardMoves =
        [ Move pos oneStep (if snd (fromIndex oneStep) == promotionRow 
                             then Just Queen else Nothing)
        | isEmpty oneStep ] ++
        [ Move pos twoStep Nothing
        | (color == White && y == 6 || color == Black && y == 1)
        , isEmpty oneStep, isEmpty twoStep ]

      -- Capture moves
      diagOffsets = [-1, 1]
      captureMoves =
        [ Move pos target (if snd (fromIndex target) == promotionRow 
                            then Just Queen else Nothing)
        | dx <- diagOffsets
        , let (nx, ny) = (x + dx, y + if color == White then -1 else 1)
        , inBounds (nx, ny)
        , let target = toIndex (nx, ny)
        , isOpponent target
        ]
  in forwardMoves ++ captureMoves
```

For a pawn, movement depends on color (because pawns move in opposite directions for White vs Black):

- `dir` is set to -8 for White pawns (which means moving to a lower index, since White moves upwards on the board from White’s perspective and our index 0 is top-left) and +8 for Black pawns (moving to a higher index).

- `(x, y)` is the pawn’s current coordinate. y is the rank index (0 top rank, 7 bottom rank).

- `promotionRow` is the rank index at which a pawn promotion occurs (0 for White pawns reaching the top, 7 for Black pawns reaching the bottom).

- Helper lambdas:

    - `validIdx` checks if an index is within 0–63.

    - `isEmpty` i checks if the square at index `i` is on the board and empty.

    `isOpponent i` checks if a square has a piece of the opposite color (and is on the board).

Using these, pawn moves are generated in two categories:

1. **Forward moves**:

     - A pawn can move one step forward if the square directly ahead (`oneStep`) is empty. If that move would land the pawn on the promotion rank, the `Move` is created with a `Just Queen` to indicate promotion to a Queen (this code promotes all pawns to queens for simplicity).

    - Additionally, if the pawn is on its starting rank (White pawn at rank 2, which corresponds to `y == 6` in 0-indexed coordinate, or Black pawn at rank 7, `y == 1`), it may move two steps forward (`twoStep`) provided that both the one-step and two-step squares are empty (no pieces blocking). The two-step move is not a promotion (pawns cannot promote from a two-step move because that implies they started on rank 2/7 and moved to rank 4/5, not the last rank).

2. **Capture moves**:

    - Pawns can capture one square diagonally forward. The `diagOffsets` list `[-1, 1]` represents a move one file to the left or right.

    - For each diagonal offset `dx`, the target coordinate is `(x + dx, y + forwardY)`, where `forwardY` is `-1` for White (moving up one rank) or `+1` for Black (moving down one rank).

    - If that target coordinate is on the board (`inBounds`) and contains an opponent’s piece (`isOpponent target` returns True), a capture move `Move pos target ...` is generated. As with forward moves, if the capture lands the pawn on the last rank (`promotionRow`), it includes `Just Queen` in the move to indicate promotion.

This implementation covers normal pawn moves, initial two-square advances, captures, and promotions. (It does not handle en passant captures or any special pawn rules beyond these basics.)

### Knight Moves 
– `generateKnightMoves color pos board`:
```
generateKnightMoves :: Color -> Position -> [Cell] -> [Move]
generateKnightMoves color pos board =
  [ Move pos idx Nothing
  | (dx, dy) <- directions
  , let (x, y) = fromIndex pos
  , let (nx, ny) = (x + dx, y + dy)
  , inBounds (nx, ny)
  , let idx = toIndex (nx, ny)
  , let target = board !! idx
  , case target of
      Empty -> True
      PlacedFigure (Figure _ c') -> c' /= color
  ]
  where
    directions = [(-2, -1), (-2, 1),
                  (-1, -2), (-1, 2),
                  (1, -2),  (1, 2),
                  (2, -1),  (2, 1)]
```

Knights move in "L" shapes: 2 in one direction and 1 perpendicular. The `directions` list enumerates all 8 possible relative moves a knight can make (dx, dy pairs).
For each potential move:

- Calculate `(nx, ny)` by adding dx, dy to the current position `(x, y)`.

- Check that `(nx, ny)` is within bounds.

- Compute the linear index `idx` for the target square.

- Check the content of `board !! idx`:

    - If it’s `Empty`, the move is valid.

    - If it’s a `PlacedFigure` with an opponent’s color (`c' /= color`), the move is a valid capture.

    - If it’s a piece of the same color, the move is blocked and not included.

- Each valid move is collected as `Move pos idx Nothing` (knights do not involve promotions in this implementation, and the `Nothing` indicates no special promotion piece).

### Sliding Piece Moves 
– Rook, Bishop, Queen use the helper `slidingMoves`:

```
slidingMoves :: Int -> Color -> [Cell] -> [(Int, Int)] -> [Move]
slidingMoves pos color board dirs = concatMap slide dirs
  where
    (x, y) = fromIndex pos
    slide (dx, dy) = go 1
      where
        go i =
          let (nx, ny) = (x + i*dx, y + i*dy)
          in if not (inBounds (nx, ny)) then []
             else 
               let idx = toIndex (nx, ny)
                   target = board !! idx
               in case target of
                    Empty -> Move pos idx Nothing : go (i + 1)
                    PlacedFigure (Figure _ c') ->
                      if c' /= color 
                      then [Move pos idx Nothing]  -- capture opponent and stop
                      else []                      -- same-color piece blocks
```

This function takes a starting `pos`, the `color` of the piece, the board, and a list of direction vectors dirs. It will trace moves in each direction until it hits the board boundary or another piece.

- For each direction `(dx, dy)` in `dirs`, it calls the local function `slide`:

    - `go i` computes the i-th step in that direction from the starting position.

    - It calculates new coordinates `(nx, ny)` = `(x + i*dx, y + i*dy)`.

    - If the new position is out of bounds, it stops (`[]`).

    - If it’s in bounds, it computes the index and looks at `target` cell:

        - If `target` is `Empty`, this move is valid (`Move pos idx Nothing`) and it continues to the next step `go (i+1)` to keep sliding further in this direction.

        - If `target` has a piece:

            - If the piece is of the opposite color (`c' /= color`), then a capturing move `Move pos idx Nothing` is added (the move is included, but the sliding in that direction stops after capturing the opponent).

            - If the piece is the same color, the sliding is blocked and stops with no move added (you cannot move or capture through your own piece).

- The moves from all directions are concatenated into one list. This covers all moves for sliding pieces in those directions.

This generic `slidingMoves` is used for:

- Rook (straight lines): directions `[(-1,0), (1,0), (0,-1), (0,1)]` which correspond to left, right, up, down.

- Bishop (diagonals): directions `[(-1,1), (1,1), (-1,-1), (1,-1)]` which are the four diagonal directions.

- Queen (combining both): eight directions (rook + bishop directions).

### King Moves
- `generateKingMoves color pos board`:

```
generateKingMoves :: Color -> Position -> [Cell] -> [Move]
generateKingMoves color pos board =
  [ Move pos idx Nothing
  | (dx, dy) <- directions
  , let (x, y) = fromIndex pos
  , let (nx, ny) = (x + dx, y + dy)
  , inBounds (nx, ny)
  , let idx = toIndex (nx, ny)
  , let target = board !! idx
  , case target of
      Empty -> True
      PlacedFigure (Figure _ c') -> c' /= color
  ]
  where
    directions = [(-1, -1), (-1, 0), (-1, 1),
                  (0, -1),           (0, 1),
                  (1, -1),  (1, 0),  (1, 1)]
```

Kings move one square in any direction. The `directions` list here enumerates the 8 surrounding squares (horizontal, vertical, diagonal moves by 1).
The comprehension is very similar to the knight’s:

- For each adjacent direction, compute (`nx, ny`).

- If it’s within bounds, and the target square is either empty or occupied by an opponent’s piece, include the move.

- The King moves are listed as `Move pos idx Nothing` (no special flag; castling is not implemented in this code).

**Note**: This code does not implement special moves like **castling** for the King or **en passant** for pawns. It also does not check for moves that would put or leave your own king in check. It generates moves based purely on piece movement capabilities and simple occupancy. So, illegal moves (like moving into check) are not filtered out. This is a simplified model suitable for a basic AI but not a full rules-enforcing engine.

## Game State Evaluation

To guide the minimax search, the program needs a way to evaluate how good a given board position is for a player. The `evaluate` function provides a heuristic evaluation of a `GameState`:

```
pieceValue :: Piece -> Int
pieceValue Pawn   = 1
pieceValue Knight = 3
pieceValue Bishop = 3
pieceValue Rook   = 5
pieceValue Queen  = 9
pieceValue King   = 1_000_000_007  -- hezké prvočíslo (a "nice prime number")

evaluate :: GameState -> Int
evaluate (GameState (cells, _) _) = material White - material Black
  where
    material col = sum 
      [ pieceValue p 
      | cell <- cells
      , PlacedFigure (Figure p c) <- [cell]  -- pattern match to filter placed pieces
      , c == col
      ]
```

- `pieceValue` assigns an integer score to each piece type. This is a common simple material scoring system in chess:

    - Pawn = 1

    - Knight = 3

    - Bishop = 3

    - Rook = 5

    - Queen = 9

    - King = 1,000,000,007 (one billion and seven)

The King is given an extremely large value (1,000,000,007, which is noted as a prime number in the comment) to ensure that losing a King (checkmate) outweighs any combination of other pieces. In practice, this makes the engine treat checkmate as the highest priority (since if a King is captured, the game is over). The use of underscores in the numeric literal is enabled by the `{-# LANGUAGE NumericUnderscores #-}` pragma at the top of the code. This pragma allows writing numeric literals with underscores for readability – the underscores are ignored by the compiler
ghc.gitlab.haskell.org
 (so `1_000_000_007` is equal to 1000000007).

- `evaluate` computes a score for the state by summing up the values of White’s pieces and subtracting the sum of Black’s pieces. In other words, **positive scores mean White is ahead in material, negative scores mean Black is ahead**. It ignores positional factors and just counts material.

    - It does this by a list comprehension over all cells: for each `PlacedFigure (Figure p c)` in the board, if the piece’s color `c` matches the color we are summing for, it adds that piece’s value.

    - Then `material White` gives total `White material`, material Black gives Black’s total, and it returns the difference.

This evaluation function is simplistic but provides the necessary feedback for the minimax algorithm to decide which positions are better. It also inherently encodes that a state where a King is missing (i.e., checkmate for that side) will have a huge swing in value because one side’s King (value 1e9) is gone.

## Minimax and Alpha-Beta Pruning

At the heart of the engine is the minimax search algorithm with alpha-beta pruning to choose the best move for the current player. The function `minimaxAB` ties everything together, and it uses a helper `alphabeta` function to recursively search game states. There are also two unused helper functions `minValue` and `maxValue` (they appear to be an earlier attempt at implementing minimax without alpha-beta, and are not used in the final logic).

```
minimaxAB :: GameState -> Int -> Maybe Move
minimaxAB state@(GameState _ turn) depth =
  case generateMoves state of
    [] -> Nothing
    moves ->
      let scored = [ ( alphabeta (applyMove state m) (depth - 1) (-inf) inf (turn == Black)
                     , m)
                   | m <- moves ]
          bestMove = if turn == White
                     then maximumBy compareFst scored
                     else minimumBy compareFst scored
      in Just $ snd bestMove
  where inf = 1_000_000_007
```

- `minimaxAB` takes a `GameState` and a search `depth` (how many plies to look ahead) and returns a `Maybe Move` – the chosen best move or Nothing if no moves are possible.

- It first generates all possible moves for the current state. If there are no moves (game over or stalemate), it returns `Nothing`.

- If there are moves, it evaluates each move by calling the recursive `alphabeta` on the state resulting from that move:

  - `applyMove state m` computes the new `GameState` after making move `m`.

  - It calls `alphabeta` on this new state with `depth - 1` (since we have made one move, we search the remaining depth), and initial alpha = -inf, beta = inf bounds.

  - The last parameter `(turn == Black)` is a boolean indicating whether the current player was Black (since if the current turn is Black, after applying the move it will be White’s turn, and vice versa). This boolean is used inside `alphabeta` to determine maximizing/minimizing.

- The result is a list of tuples `(score, move)` for each possible move.

- If it’s White’s turn, we want the move with the maximum score (White tries to maximize the evaluation). If it’s Black’s turn, we want the move with the minimum score (Black tries to minimize the evaluation, since the score is from White’s perspective). The code uses `maximumBy compareFst` or `minimumBy compareFst` to select the best `(score, move)` tuple accordingly.

- It then returns the `Move` part of that tuple (wrapped in `Just`). Thus, we get the best move according to the minimax search.

The `inf` defined here as `1_000_000_007` is used as a practical "infinity" for alpha-beta initialization – it’s the same large number as the King’s value, ensuring it’s beyond any realistic score in normal play, and effectively representing positive infinity for the search bounds.

```
alphabeta :: GameState -> Int -> Int -> Int -> Bool -> Int
alphabeta state depth alpha beta maximizingPlayer
  | depth == 0 || isTerminal state = evaluate state
  | maximizingPlayer = goMax moves alpha
  | otherwise        = goMin moves beta
  where
    moves = generateMoves state

    goMax [] a = a
    goMax (m:ms) a =
      let val = alphabeta (applyMove state m) (depth - 1) a beta False
          a'  = max a val
      in if beta <= a' 
         then a'                  -- Beta cut-off
         else goMax ms a'

    goMin [] b = b
    goMin (m:ms) b =
      let val = alphabeta (applyMove state m) (depth - 1) alpha b True
          b'  = min b val
      in if b' <= alpha 
         then b'                  -- Alpha cut-off
         else goMin ms b'
```

Parameters of `alphabeta`:

- `state`: current game state to evaluate.

- `depth`: remaining search depth (how many moves ahead to explore from this state).

- `alpha`: the current best score achievable for the maximizing player (initially -∞).

- `beta`: the current best score achievable for the minimizing player (initially +∞).

- `maximizingPlayer`: a `Bool` indicating whose turn it is from the perspective of the algorithm. True means it’s a node where we want to maximize the evaluation (i.e., it’s White’s turn in terms of evaluation perspective), and `False` means minimize (Black’s turn).

The algorithm:

- If `depth == 0` (reached the desired lookahead limit) or the state is terminal (game over), it returns the heuristic evaluation of the state (`evaluate state`) directly. This is the base case of the recursion.

- If `maximizingPlayer` is True (we are to maximize at this node):

  - It iterates through moves (if there are no moves, it just returns the current alpha which would have been set to evaluation in base or something – but if terminal, it would have returned above anyway).

  - For each move, it applies the move to get a new state, and calls alphabeta on that with `maximizingPlayer = False` (since the next level will be minimizing). Notice it passes the current `alpha` value and the same `beta`.

  - It then updates `a' = max a val` where `a` is the running alpha (best max so far) and `val` is the value of the move just evaluated.

  - If this updated `a'` is >= beta, it means the maximizing side found a move that is so good that the minimizing side (at a higher level) would avoid this branch (because beta is the best option for the minimizing side above). This is the alpha-beta cutoff: the loop breaks and returns `a'` without exploring further moves (pruning the search tree).

  - Otherwise, it continues with the remaining moves.

  - `goMax` returns the final alpha value which represents the best score the maximizer can secure from this state.

- If `maximizingPlayer` is False (so we are minimizing at this node, i.e., it’s effectively Black’s turn from White’s perspective):

  - It similarly iterates through moves and uses `goMin`:

  - For each move, calls `alphabeta` on the resulting state with `maximizingPlayer = True` (next level is maximizer’s turn), and passes along the current `alpha` and `beta`.

  - Updates `b' = min b val` as the potential beta (best score for minimizer so far).

  - If `b' <= alpha`, a cutoff occurs (the minimizer found a move that is so bad for the maximizer that the maximizer above would avoid this branch).

  - Continues otherwise, and returns the final beta (best score the minimizer can force).

These two processes (`goMax` and `goMin`) implement the alpha-beta pruning optimization, which avoids exploring branches that can't possibly influence the final decision. This dramatically improves efficiency by pruning unnecessary search paths in the game tree.

The initial call to `alphabeta` for each move in `minimaxAB` was made with `alpha = -inf`, `beta = inf`. The boolean `maximizingPlayer` was set based on the current turn: it passes `(turn == Black)`. This might seem counter-intuitive at first glance, but recall how `minimaxAB` is using it:

- If it’s White’s turn (`turn == Black` is False), it calls `alphabeta(newState, depth-1, -inf, inf, False)`. The first move it explores will be with `maximizingPlayer = False` because after White makes a move, it will be Black's turn, which should be a minimizing level.

- If it’s Black’s turn (`turn == Black` is True), it calls `alphabeta(newState, depth-1, -inf, inf, True)`. After Black makes a move, it will be White's turn, which is a maximizing level.

So this boolean essentially flips the role as we go one ply down. Inside `alphabeta`, the code then flips it each recursive call (`goMax` calls next with False, `goMin` calls next with True), alternating between maximizing and minimizing levels as the game state alternates turns.

```
isTerminal :: GameState -> Bool
isTerminal (GameState (cells, _) _) =
  not (hasKing cells White && hasKing cells Black)

hasKing :: [Cell] -> Color -> Bool
hasKing cells col = any matchKing cells
  where
    matchKing (PlacedFigure (Figure King c)) = c == col
    matchKing _ = False
```

- `hasKing cells col` returns True if any cell contains a King of the given color.

- `isTerminal` checks if not (White has a King and Black has a King). In other words, if either White’s king or Black’s king is missing from the board, the state is terminal (game over). This is a simple way to detect checkmate (or situations where a king is captured or removed). In a real chess game, a king is never actually taken off the board (games end before a king is "captured"), but for this engine, if a king is not found, it treats it as an end condition.

- When `isTerminal` is true, `alphabeta` will return `evaluate state` immediately. Notably, if a king is missing, the evaluation will be extremely large in magnitude because of the King’s high piece value (making the side that lost their king have a huge negative score).

## Applying Moves to the Game State

The `applyMove` function produces a new `GameState` by executing a given move on the current state:

```
applyMove :: GameState -> Move -> GameState
applyMove (GameState (cells, n) turn) (Move from to promo) =
  let PlacedFigure fig = cells !! from
      fig' = case promo of
               Just p  -> Figure p (figureColor fig)
               Nothing -> fig
      newCells = updateCell to (PlacedFigure fig') $
                 updateCell from Empty cells
      newBoard = (newCells, n + 1)
  in GameState newBoard (nextPlayer turn)
```

- It pattern matches the `GameState` to get the current `cells` (board array) and move count `n`, as well as `turn` (whose turn it is).

- Pattern matches the `Move` into `from`, `to`, and `promo`.

- It then looks up the piece (`fig`) at the `from` position. There’s an assumption here that `cells !! from` is a `PlacedFigure fig` (which should hold true if the move was generated correctly).

- If the move has a promotion (`promo` is `Just p`), it creates a new `Figure p (figureColor fig)`. This means it takes the pawn (which `fig` should be in this case) and changes its piece type to `p` (e.g., Queen) while keeping the same color. If `promo` is `Nothing`, it leaves the figure as-is.

- It uses `updateCell` to produce a new list of cells after the move:

  - First, it sets the `to` index with the `PlacedFigure fig'` (the moving piece, possibly promoted).

  - Then it sets the `from` index to `Empty` (the piece moved away from its original square).

  - `updateCell` is a helper that creates a new list with one position updated:
    ```
    updateCell :: Int -> Cell -> [Cell] -> [Cell]
updateCell i val board = take i board ++ [val] ++ drop (i + 1) board
    ```

    This takes the portion of the list before index `i`, then the new value, then the portion after `i`. It is used in a functional way to immutably update the board.

  - The new board is packaged as `(newCells, n+1)` – incrementing the move counter.

  - Finally, it returns a new `GameState` with this updated board and the turn switched to the other player (`nextPlayer turn` gives the opposite Color: White becomes Black, Black becomes White).

This function does not enforce any rules beyond moving the piece: it will overwrite whatever was at the destination (which could be Empty or an opponent's piece, effectively capturing it). It handles pawn promotion by changing the pawn to a new piece type if applicable. It doesn't specifically handle en passant (as noted) or removal of an en-passant captured pawn, etc. It's a straightforward state transition for a move from the list generated earlier.

## Main Program Flow

The `main` function ties together input, move selection, and output:

```
main :: IO ()
main = do
  input <- getLine
  let result = chooseMoveFromString input
  print result
```

- It reads a line from standard input (using `getLine`). This should be the board string described in the User Documentation.

- It calls `chooseMoveFromString input` to compute the best move.

- It prints the result.

The function `chooseMoveFromString` is a convenience that parses the input and runs the minimax search:

```
chooseMoveFromString :: String -> Maybe Move
chooseMoveFromString input =
  let (cells, turnInt) = parseBoard input
      turn = if turnInt == 0 then White else Black
      gameState = GameState (cells, 0) turn
  in minimaxAB gameState 4
```

This essentially duplicates what `parseGameState` does (it manually parses the board and turn). It creates a `GameState` and then calls `minimaxAB` with a search depth of 4. The result is a `Maybe Move`, which is exactly what `main` prints out.

Depth 4 means the algorithm looks ahead 4 plies (a "ply" is one move by one player, so 4 plies could be two moves by White and two by Black if depth is interpreted as plies). In other terms, it's exploring sequences of White->Black->White->Black moves (if White is the one to move initially). A deeper depth would make the AI stronger but slower, and a shallower depth would make it faster but weaker.

## Additional Notes

- The code uses the `Data.Function ((&))` operator in one place (when constructing an example board `board1`). The `&` operator is a reverse application operator: `value & f` is the same as `f value`. It is used to apply a series of `updateCell` operations in a readable way. For instance:

```
board1 = [Empty | _ <- [0..63]]
         & updateCell 27 (PlacedFigure (Figure Queen Black))
         & updateCell 29 (PlacedFigure (Figure Rook Black))
         & updateCell 36 (PlacedFigure whitePawn)
```

This starts with an all-empty board and then updates cell 27, 29, and 36 with specific pieces. Using `&` allows writing these updates in left-to-right order as seen above, instead of nested function calls. This is mainly for demonstration and is not critical to the main algorithm (which uses the parsed input to set up the board).

- Immutability: All state transformations (`updateCell`, `applyMove`, etc.) produce new lists and new `GameState` values rather than modifying in-place. This is idiomatic in Haskell, leveraging immutability and pure functions for game state transitions.

- Performance considerations: A depth-4 alpha-beta search on a chess position is not very computationally heavy (since this engine does not consider the full complexity of chess rules). The move generation is simplified, and the branching factor is manageable. The heavy use of lists and recursion is natural in Haskell, though a more optimized engine might use advanced techniques (bitboards, etc.). Nonetheless, for educational and small-scale use, this approach is clear and sufficient.

- Limitations: As mentioned, the engine does not handle all chess rules:

  - No castling moves for King or Rook.

  - No en passant captures for pawns.

  - No draw conditions (like stalemate detection beyond no moves, insufficient material, fifty-move rule, repetition, etc.).

  - No check or checkmate detection in move generation (it only indirectly detects checkmate when a King is missing). This means it might generate moves that leave the king in check or move a king into check, which would be illegal in a proper chess game.

  - The evaluation is purely material-based; it doesn’t account for positional factors or check/checkmate directly (except via the King’s disappearance).

  Despite these limitations, the program can still play a rudimentary game of chess focusing on material gain.

## Conclusion

This documentation provided an overview of how the Haskell chess move selection program works and how to use it. The code demonstrates key concepts in functional programming applied to game AI: using algebraic data types to model game entities, pure functions to generate new game states, and a recursive minimax search with alpha-beta pruning to make decisions. Developer-oriented explanations are given, so you can understand and modify it.