import Data.Char (isAsciiUpper, toUpper)
import Data.Function ((&))
import Data.List (maximumBy, minimumBy)
{-# LANGUAGE NumericUnderscores #-} -- Allow numeric literals with underscores for readability

main :: IO ()
main = do
  input <- getLine
  let result = chooseMoveFromString input
  print result
  -- The main function reads a board state string from input, computes the best move, and prints it

data Piece = King | Queen | Rook | Knight | Bishop | Pawn
  deriving (Eq, Show)
  -- Chess piece type (all six types of pieces)

data Color = Black | White
  deriving (Eq, Show)
  -- Player and piece color

data Figure = Figure Piece Color
  deriving (Eq, Show)
  -- A Figure pairs a piece with its color (e.g., a white knight or black pawn)

data Cell = Empty | PlacedFigure Figure
  deriving (Eq, Show)
  -- A board cell can either be empty or contain a specific figure

data GameState = GameState
  { board :: Board
  , playerTurn :: Color
  } deriving (Eq, Show)
  -- The game state, including the current board and whose turn it is

data Move = Move Position Position (Maybe Piece)  -- from -> to + promotion
  deriving (Eq, Show)
  -- A chess move from one position to another, with an optional promotion piece (Just piece if a pawn is promoted, otherwise Nothing)

type Board = ([Cell], Int)
-- The board is represented as a tuple: a list of 64 Cells and an Int (e.g. move count or turn indicator)

type Position = Int
-- Board positions are numbered 0-63 (0 = top-left corner, 63 = bottom-right)

nextPlayer :: Color -> Color
nextPlayer White = Black
nextPlayer Black = White
-- Switches to the other player color

charToCell :: Char -> Cell
charToCell '.' = Empty
charToCell c
  | isAsciiUpper c = PlacedFigure (Figure (toPiece (toUpper c)) White)
  | otherwise = PlacedFigure (Figure (toPiece (toUpper c)) Black)
-- Converts a character from the input board string into a Cell
-- '.' denotes an empty square
-- An uppercase letter (e.g. 'K') represents a White piece; a lowercase letter (e.g. 'k') represents a Black piece
-- The letter (ignoring case) is converted to a Piece type via toPiece

toPiece :: Char -> Piece
toPiece 'K' = King
toPiece 'Q' = Queen
toPiece 'R' = Rook
toPiece 'B' = Bishop
toPiece 'N' = Knight
toPiece 'P' = Pawn
toPiece _   = error "Unknown piece"
-- Maps a character to the corresponding Piece type. Expects one of K, Q, R, B, N, P (case-insensitive)
-- Any other character will cause an error

figureColor :: Figure -> Color
figureColor (Figure _ c) = c
-- Gets the Color of a Figure (White or Black)

compareFst :: Ord a => (a, b) -> (a, b) -> Ordering
compareFst (a1, _) (a2, _) = compare a1 a2
-- Utility function to compare two tuples by their first element. Useful for sorting moves by score

parseBoard :: String -> Board
parseBoard input =
  let (boardStr, playerChar) = splitAt 64 input
      cells = map charToCell boardStr
      turn = if null playerChar || head playerChar == 'W' then 0 else 1
  in (cells, turn)
-- Parses a string into a Board
-- The first 64 characters of the string represent the board squares (row by row). Each character is converted to a Cell
-- If an extra character is provided after 64 chars, it indicates whose turn it is: 'W' for White (0), 'B' for Black (1). Defaults to White's turn if not specified

parseGameState :: String -> GameState
parseGameState input =
  let (cells, turnInt) = parseBoard input
      turn = if turnInt == 0 then White else Black
  in GameState (cells, 0) turn
-- Creates a GameState from an input string by parsing the board and determining the turn
-- The second element of Board (Int) is used as a move counter here, initialized to 0 for a new state

pieceValue :: Piece -> Int
pieceValue Pawn = 1
pieceValue Knight = 3
pieceValue Bishop = 3
pieceValue Rook = 5
pieceValue Queen = 9
pieceValue King = 1_000_000_007 -- Nice prime number
-- Assigns a basic material value to each piece for the evaluation function
-- The King is given an extremely high value (effectively infinite) to ensure that losing a King is considered the worst outcome

evaluate :: GameState -> Int
evaluate (GameState (cells, _) _) = material White - material Black
  where
    material col = sum [pieceValue p | cell <- cells, PlacedFigure (Figure p c) <- [cell], c == col]
-- Evaluates the board position, returning a score (material balance)
-- Sums the values of all White pieces and subtracts the sum of all Black pieces
-- A positive result means White is ahead in material, negative means Black is ahead

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
-- Generates all possible moves for the given figure (piece and color) at position `pos` on the board
-- It delegates to the specific move generation function based on the piece type

generatePawnMoves :: Color -> Position -> [Cell] -> [Move]
generatePawnMoves color pos board =
  let dir = if color == White then -8 else 8  -- direction: White pawns move "up" (negative index), Black "down" (positive index)
      (x, y) = fromIndex pos
      oneStep = pos + dir
      twoStep = pos + 2 * dir
      promotionRow = case color of
        White -> 0  -- White pawn promotes at row 0 (top rank)
        Black -> 7  -- Black pawn promotes at row 7 (bottom rank)
      validIdx i = i >= 0 && i < 64
      isEmpty i = validIdx i && board !! i == Empty
      isOpponent i = validIdx i &&
        case board !! i of
          PlacedFigure (Figure _ c) -> c /= color -- cell contains a piece of the opposite color
          _ -> False

      -- Forward moves (one step or two steps forward if on starting rank)
      forwardMoves =
        [ Move pos oneStep (if snd (fromIndex oneStep) == promotionRow then Just Queen else Nothing)
        | isEmpty oneStep ] ++
        [ Move pos twoStep Nothing
        | (color == White && y == 6 || color == Black && y == 1) -- pawn is on its starting rank (y=6 for White pawns, y=1 for Black pawns)
        , isEmpty oneStep, isEmpty twoStep ]

      -- Capture moves (diagonal moves to capture opponent pieces)
      diagOffsets = [-1, 1]
      captureMoves =
        [ Move pos target (if snd (fromIndex target) == promotionRow then Just Queen else Nothing)
        | dx <- diagOffsets
        , let (nx, ny) = (x + dx, y + if color == White then -1 else 1) -- move one row forward (depending on color) and one column left or right
        , inBounds (nx, ny)
        , let target = toIndex (nx, ny)
        , isOpponent target
        ]
  in forwardMoves ++ captureMoves
-- Generates all possible pawn moves from position `pos`:
--  - Forward moves: one-step forward if empty, and two-step forward if on the starting rank and both squares are empty
--  - Diagonal captures: one-step diagonally forward-left or forward-right if there is an opponent piece to capture
-- Automatically handles promotion: if a pawn move reaches the last rank, it is marked as promoting to a Queen (Just Queen)

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
      Empty -> True -- can always move to an empty square
      PlacedFigure (Figure _ c') -> c' /= color -- can move if target has an opponent's piece
  ]
  where
    directions = [(-2, -1), (-2, 1), (-1, -2), (-1, 2),
                  (1, -2), (1, 2), (2, -1), (2, 1)]
-- Generates all possible knight moves (in an L-shape)
-- The knight moves in 8 possible (dx, dy) offsets. It can land on any square that is inside the board and not occupied by a friendly piece (it can jump over pieces)

slidingMoves :: Foldable t => Int -> Color -> [Cell] -> t (Int, Int) -> [Move]
slidingMoves pos color board dirs = concatMap slide dirs
  where
    (x, y) = fromIndex pos
    slide (dx, dy) = go 1
      where
        go i =
          let (nx, ny) = (x + i*dx, y + i*dy)
          in if not (inBounds (nx, ny)) then []
             else let idx = toIndex (nx, ny)
                      target = board !! idx
                  in case target of
                      Empty -> Move pos idx Nothing : go (i + 1)  -- square is empty, add move and continue further in this direction
                      PlacedFigure (Figure _ c') ->
                        if c' /= color then [Move pos idx Nothing] else [] -- square has an opponent's piece, add capture move and stop (can't go beyond), else stop
-- Generates moves for sliding pieces (Rook, Bishop, Queen) in each specified direction
-- It continues moving step by step in a direction until it goes out of bounds or hits a piece
-- If it hits an empty square, it can continue further. If it hits an opponent's piece, that move (capture) is included and then it stops in that direction. If it hits a friendly piece, it stops immediately without adding a move

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
      Empty -> True -- can move to empty square
      PlacedFigure (Figure _ c') -> c' /= color -- or capture opponent's piece
  ]
  where
    directions = [(-1, -1), (-1, 0), (-1, 1),
                  (0, -1),          (0, 1),
                  (1, -1),  (1, 0),  (1, 1)]
-- Generates all possible king moves (one step in any direction around the king)
-- ensuring the target square is on the board and not occupied by a friendly piece

fromIndex :: Int -> (Int, Int)
fromIndex i = (i `mod` 8, i `div` 8)
-- Converts a linear index [0..63] into (x, y) board coordinates
-- For example, index 0 -> (0,0), index 7 -> (7,0), index 8 -> (0,1), index 63 -> (7,7)

toIndex :: (Int, Int) -> Int
toIndex (x, y) = y * 8 + x
-- Converts (x, y) coordinates back into a linear index (0-63)

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8
-- Checks if the given (x, y) coordinates are within the 8x8 board boundaries

minimaxAB :: GameState -> Int -> Maybe Move
minimaxAB state@(GameState _ turn) depth =
  case generateMoves state of
    [] -> Nothing -- no moves available
    moves ->
      let scored = [ (alphabeta (applyMove state m) (depth - 1) (-inf) inf (turn == Black), m)
                   | m <- moves ]
          bestMove = if turn == White
                     then maximumBy compareFst scored -- White chooses the move with the maximum score
                     else minimumBy compareFst scored -- Black chooses the move with the minimum score
      in Just $ snd bestMove
  where inf = 1_000_000_007
-- Determines the best move for the current player using minimax with alpha-beta pruning to the given depth
-- It generates all possible moves, evaluates each move by recursively searching (with alphabeta)
-- and then picks the move with the optimal score (maximizing for White, minimizing for Black)
-- Returns Nothing if there are no moves (e.g., game is terminal)

generateMoves :: GameState -> [Move]
generateMoves (GameState (cells, _) player) =
  concat [generatePieceMoves f i cells | (i, PlacedFigure f@(Figure _ c)) <- zip [0..] cells, c == player]
-- Generates all legal moves for the current player by finding all pieces of that color on the board and collecting their moves

applyMove :: GameState -> Move -> GameState
applyMove (GameState (cells, n) turn) (Move from to promo) =
  let PlacedFigure fig = cells !! from
      fig' = case promo of
               Just p -> Figure p (figureColor fig) -- if there is a promotion, create the new Figure with same color
               Nothing -> fig -- otherwise, the figure remains the same
      newCells = updateCell to (PlacedFigure fig') $
                 updateCell from Empty cells  -- move the piece: empty the from-cell and place the figure in the to-cell
      newBoard = (newCells, n + 1)
  in GameState newBoard (nextPlayer turn)
-- Applies a move to the game state, returning the new game state after the move
-- It removes the piece from the `from` position and places it (or its promotion replacement) at the `to` position
-- The move counter (n) is incremented, and the turn switches to the other player

alphabeta :: GameState -> Int -> Int -> Int -> Bool -> Int
alphabeta state depth alpha beta maximizingPlayer
  | depth == 0 || isTerminal state = evaluate state
  | maximizingPlayer = goMax moves alpha  -- maximizing player's turn
  | otherwise         = goMin moves beta  -- minimizing player's turn
  where
    moves = generateMoves state

    goMax [] a = a
    goMax (m:ms) a =
      let val = alphabeta (applyMove state m) (depth - 1) a beta False
          a' = max a val
      in if beta <= a' then a' else goMax ms a' -- prune branch if alpha crosses beta

    goMin [] b = b
    goMin (m:ms) b =
      let val = alphabeta (applyMove state m) (depth - 1) alpha b True
          b' = min b val
      in if b' <= alpha then b' else goMin ms b'
-- The recursive alpha-beta pruning algorithm
-- `maximizingPlayer` indicates whether the current depth corresponds to the player trying to maximize the score or minimize it
-- Alpha (α) is the best score the maximizing player can guarantee so far, and Beta (β) is the best score the minimizing player can guarantee
-- The function returns the evaluated score of the state
-- It prunes branches when it finds a result that is worse than a previously explored alternative (i.e., when α ≥ β)

isTerminal :: GameState -> Bool
isTerminal (GameState (cells, _) _) =
  not (hasKing cells White && hasKing cells Black)
-- Checks if the game state is terminal (i.e., one of the kings is missing, so either White or Black has no king)

hasKing :: [Cell] -> Color -> Bool
hasKing cells col = any matchKing cells
  where
    matchKing (PlacedFigure (Figure King c)) = c == col
    matchKing _ = False
-- Checks if there's a King of the given color present in the list of cells

chooseMoveFromString :: String -> Maybe Move
chooseMoveFromString input =
  let (cells, turnInt) = parseBoard input
      turn = if turnInt == 0 then White else Black
      gameState = GameState (cells, 0) turn
  in minimaxAB gameState 4
-- Helper that takes an input string describing a board and returns the chosen best move
-- It parses the input into a GameState and runs `minimaxAB` with search depth 4 to get the best move
---------------------------

-- Example usage / test data below:

board1 :: [Cell]
board1 = [Empty | _ <- [0..63]] -- start with all squares empty
         & updateCell 27 (PlacedFigure (Figure Queen Black))  -- place a Black Queen at index 27 (d5)
         & updateCell 29 (PlacedFigure (Figure Rook Black)) -- place a Black Rook at index 29 (f5)
         & updateCell 36 (PlacedFigure (Figure Pawn White)) -- place a White Pawn at index 36 (e4)
-- The & operator is used to thread the board through multiple updateCell calls for convenience.

updateCell :: Int -> Cell -> [Cell] -> [Cell]
updateCell i val board = take i board ++ [val] ++ drop (i + 1) board
-- Simple helper function to update the cell at index `i` in the board list
-- It returns a new list where the element at position i is replaced with `val`