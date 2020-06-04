module API where
import Data.Maybe
import Data.List
-- a data type that denotes the numbers that each player may select from
data Number_1to9 = N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 deriving (Eq, Show, Ord, Bounded)


data List a = Nil | Cons a (List a) -- Using the inbuilt list type as I don't want to rewrite comphrensions

-- starts off empty and records each move as a player selects a number
data GameState = GameState [Number_1to9] deriving (Show, Eq) -- Every 2nd number is P2, read above for why Im using []


data Solution = Solution Number_1to9 Number_1to9 Number_1to9 deriving (Show, Eq)

-- the possible results of a valid number selection
data ValidSelection a = Player1Wins Solution | Player2Wins Solution | Draw | KeepPlaying a deriving (Show)

-- the possible results of any number selection
data NumberSelected = InvalidSelection | AlreadySelected Number_1to9 | Selected (ValidSelection GameState) deriving (Show)


-- Data type that represents a winning solution and who won it
data Winner = P1Win Solution | P2Win Solution deriving (Show)

{-  
    Rules for the game: Two players pick numbers sequentially from a pool of 1 to 9
    and each time the state is checked to see if they've won or not. Exactly _3_ numbers must
    equal 15, if none do then you can't delcare a win. Draw happens when P1 has 5 #s P2 has 4 and no wins.
-}

-- Checks if a number exists in a given gamestate already; i.e., if it's been picked yet. 
-- Evaluates to True if the number exists in the GameState, False otherwise.
checkExists :: Number_1to9 -> GameState -> Bool
checkExists x (GameState y) = case y of
    [] -> False 
    (q:qs) -> case (x == q)  of
        True -> True 
        False -> checkExists x (GameState qs)

-- Helper function, maps a Number_1to9 to the corresponding Interger representation. 
-- Example Usage: convertNumToInt N7 -> 7
convertNumToInt :: Number_1to9 -> Int
convertNumToInt number = case number of
    N1 -> 1
    N2 -> 2
    N3 -> 3
    N4 -> 4
    N5 -> 5
    N6 -> 6
    N7 -> 7
    N8 -> 8
    N9 -> 9

-- Makes a new GameState with an empty list.
newGame :: GameState -- 10%
newGame = GameState []

-- select a number from the game state
-- If game has ended (i.e., there are no more moves left to make), return Invalid, else return selected outcome
-- If it has been selected, return AlreadySelected instead.
-- Example Usage: select N7 (GameState [N6, N7]) -> AlreadySelected N7
selectNumber :: Number_1to9 -> GameState -> NumberSelected
selectNumber number game = case (checkExists number game) of
    True -> AlreadySelected number
    False -> if (checkGameOver game) then InvalidSelection else Selected (createValid (addToGame game number))

-- Test function that takes a tuple of Number_1to9 and checks if any two elements are the same. If so, fail.
-- Used in filtering out duplicate element tuples in combination generation. Example Usage: (N1,N1,N2) -> False
testRepeats :: (Number_1to9, Number_1to9, Number_1to9) -> Bool
testRepeats (a, b, c) = if ((a == b) || (a == c) || (b == c)) then False else True
  

-- Generates the cartesian product of a given list of Number_1to9s, then filters those with duplicate elements.
-- Only guarantees all resulting tuples will not contain the same element twice, order defines uniqueness.
-- Returns the list of all tuples that meet the above criteria.
-- Example usage: generateTrips [N1,N2,N3] -> [(N1,N2,N3),(N1,N3,N2),(N2,N1,N3),(N2,N3,N1),(N3,N1,N2),(N3,N2,N1)]
generateTrips :: [Number_1to9] -> [(Number_1to9, Number_1to9, Number_1to9)]
generateTrips numbers = filter testRepeats ((((\a b c -> (a, b, c)) <$> numbers) <*> numbers) <*> numbers)
-- Anon function maps 3 numbers to a tuple, then map to get a, apply to get b, and apply again to get c

createSolution :: (Number_1to9, Number_1to9, Number_1to9) -> Solution
createSolution (a, b, c) = Solution a b c 

-- Helper function that takes a tuple triplet of Number_1to9's and sums them. Could be made more generic but eh
-- Example usage: (N1,N2,N3) -> 6
sumTuple :: (Number_1to9, Number_1to9, Number_1to9) -> Int
sumTuple (a, b, c) = (convertNumToInt a) + (convertNumToInt b) + (convertNumToInt c)

-- Finds a solution from a given list of Number_1to9 tuple triplets. Type is tuple to enforce size.
-- Returns nothing if no solutions found, else will return the first tuple that sums to 15.
-- Example Usage: findSolution [(N1,N2,N3), (N7, N3, N5), (N9,N4,N2)] -> Just Solution N7 N3 N5
findSolution :: [(Number_1to9, Number_1to9, Number_1to9)] -> Maybe Solution
findSolution combos = case combos of
    [] -> Nothing -- Shouldn't be reached
    (x:xs) -> if ((sumTuple x) == 15) then Just (createSolution x) else findSolution xs


-- Takes a list of Number_1to9's and returns if you can achieve a sum of 15 of a triplet.
-- Returns Nothing if no Solution, or Just Solution if one was found.
-- Example Usage: checkSum15 [N1,N2] -> Nothing
checkSum15 :: [Number_1to9] -> Maybe Solution
checkSum15 numbers 
   | (length numbers) < 3 = Nothing -- Not enough to win yet. Below will find one if exists else also return Nothing
   | (length numbers) >= 3 = findSolution (filter testRepeats (generateTrips (numbers)))
   | otherwise = Nothing
    
-- Grabs every second element of the input list; outside functions give context about which player we mean.
-- In general, grabs the most recent player who made a moves moves so far.
-- Example usage: grabPlayerMoves [N2,N1,N4,N5,N8,N3] -> [N2, N4, N8]
grabPlayerMoves :: [Number_1to9] -> [Number_1to9]
grabPlayerMoves numbers = case numbers of 
    (x:_:xs) -> x : grabPlayerMoves xs -- Grab the second element each time, shift the start point
    x -> x

-- Checks if a player has won according to the GameState. Odd move counts means player 1 just moved, else player 2.
-- Returns a Maybe Winner dependent on if a solution can be found or not, and who found it using Winner data type.
-- Example usage: checkWin (GameState [N2,N1,N4,N5,N8,N3]) -> Nothing
checkWin :: GameState -> Maybe Winner -- did someone win? who?
checkWin (GameState game) = case game of
    [] -> Nothing -- Empty, no moves made, can't be a winner
    (x:xs) -> case (odd (length (x:xs)), checkSum15 (grabPlayerMoves game)) of -- True == Player 1, False == Player 2
        (True, Just solution) ->  Just (P1Win solution)
        (False, Just solution) -> Just (P2Win solution) 
        (_, Nothing) -> Nothing

-- Checks if the game is over; either draw is true or a solution exists. Returns true if so. Used to extend API
checkGameOver :: GameState -> Bool
checkGameOver game = case (checkWin game, checkDraw game) of
    (Just _, _) -> True
    (_, True) -> True
    (_, _) -> False


-- Checks if the game has ended in a draw. It has ended in a draw if there are no moves left && no wins;
-- Example usage: checkDraw (GameState [N2,N5,N4,N9,N1,N6,N7,N3,N8]) -> True
checkDraw :: GameState -> Bool
checkDraw (GameState game) = case game of
    [] -> False -- Empty set, means no way to get draw as you haven't picked any moves
    (x:xs) -> case ((checkWin (GameState game)), length (x:xs)) of -- You have some moves; check # made, + win cons
        (Nothing, 9) -> True -- Only a draw if moves == 9 && no wins, every other case is potential
        (_, _) -> False -- Then there's a winner somewhere, or we have moves left (i.e. length != 9)
    

-- Creates a valid selection from a game state. Returns the relevant shaped ValidSelection based on board.
-- Example usage: createValid (GameState [N9,N1,N2,N3,N4,N5,N6,N7,N8]) -> Player1Wins (Solution N9 N2 N4)
createValid :: GameState -> ValidSelection GameState
createValid game 
    | isJust (checkWin game) = case (checkWin game) of -- First checks for wins, then draws, else keep going
        Just (P1Win solution) -> Player1Wins solution 
        Just (P2Win solution) -> Player2Wins solution
        Nothing -> error "You shouldn't have found this!" -- Maybe replace with KeepPlaying purely to never fail
    | checkDraw game = Draw
    | otherwise = KeepPlaying game -- KeepPlaying game if not a draw (i.e. moves left) and no win

-- Appends a selected valid move to a GameState and returns the new state. Used for building state up.
-- Example usage: addToGame (GameState [N1]) N2 -> GameState [N1,N2]
addToGame :: GameState -> Number_1to9 -> GameState
addToGame (GameState game) number = GameState (number : game)
 
-------- HERE MARKS THE IO PROGRAM -------

-- Takes a GameState and prints a string representation of it according to spec. Any numbers not chosen will be shown.
-- First converts the internal Number_1to9's to ints, sorts it then feeds it onto construction function
-- Example: printBoard [N1,N2] -> [    3 4 5 6 7 8 9]
printBoard :: GameState -> [Char]
printBoard (GameState gameboard) = determineBoardOptions (sort ((convertNumToInt) <$> gameboard)) [1..9] ""

-- Helper function which constructs the string representation of a given board state.
-- Takes two int lists: the first is the (sorted) list of numbers chosen so far, the second is a constant 
-- list of all numbers 1-9. Also takes a string to allow a string to be built up recursively.
-- Returns the string representation of the board, or an invalid string to let user know it's not correct.
determineBoardOptions :: [Int] -> [Int] -> [Char] -> [Char]
determineBoardOptions gameNums constNums currentBoard = case (gameNums, constNums) of
    ((x:xs), (y:ys)) -> case (x == y) of
        True -> determineBoardOptions xs ys (currentBoard ++ "  ") -- If same, add a space and move both down
        False -> determineBoardOptions gameNums ys (currentBoard ++ show (y) ++ " ") -- no match move ys only
    ([], [y]) -> determineBoardOptions [] [] (currentBoard ++ show (y)) -- On last number don't add the space
    ([], (y:ys)) -> determineBoardOptions [] ys (currentBoard ++ show (y) ++ " ") -- Just add rest of ys if out of moves
    ([], []) -> "[" ++ currentBoard ++ "]" -- Base case, both now empty.
    (_, _) -> "Invalid board" -- Either wasn't sorted or weren't same length; you left [1..9] before gameNums finished


-- Function which prompts the player for an input. If the player doesn't select valid input (denoted as a char 1-9)
-- Re-prompt until they do. Returns Nothing if q is pressed indicating quit, else returns a Maybe Number_1to9.
promptPlayer :: GameState -> IO (Maybe Number_1to9)
promptPlayer game = 
    do
        putStr ">>> " 
        x <- getChar -- Get their input
        putChar '\n'
        let y = charToNum x -- Try and convert it
        if (isJust y) then return y 
        else do
            if x == 'q' then return Nothing
            else do
                putStrLn "Invalid input" 
                promptPlayer game

-- Determines what to print based on the length of a game states moves. Odd indicates player 2 is ABOUT
-- to make a move, even is opposite. Example: printWhosTurn [N1,N2] -> "Player 1 to move"
printWhosTurn :: GameState -> [Char]
printWhosTurn (GameState moves) = case (odd (length moves)) of
    True -> "Player 2 to move"
    False -> "Player 1 to move"

-- Takes a Solution and prints a string representation of it. Used in gameLoop for end of game messages.
printSolution :: Solution -> [Char]
printSolution (Solution a b c) = show(convertNumToInt a) ++ " + " ++ show(convertNumToInt b) 
    ++ " + " ++ show(convertNumToInt c) ++ " = 15"

-- Main driving logic for the game itself. Takes a game state, and runs through the motions of a given move.
-- Asks for input, tries to apply it with API, decides what to do next based on response. If already selected
-- move or a KeepPlaying is found, recurses to loop to next move.
gameLoop :: GameState -> IO ()
gameLoop game =
    do
        putStrLn (printBoard game) 
        putStrLn (printWhosTurn game)
        response <- promptPlayer game
        if (isNothing response) then do
            putStrLn "Bye!" -- We got a 'q', so exit game
        else do -- Then we MUST have a Just Number_1to9
            let number = fromJust response
            let move = selectNumber number game

            case move of
                Selected valid -> do
                    -- Selected( ValidSelection GameState ) is shape here
                    case valid of
                        Player1Wins solution -> putStrLn ("Player 1 Wins!" ++ " " ++ (printSolution solution))
                        Player2Wins solution -> putStrLn ("Player 2 Wins!" ++ " " ++ (printSolution solution))
                        Draw -> putStrLn "The game is a draw"
                        KeepPlaying newState -> gameLoop newState  -- a is a new game state

                AlreadySelected _ -> do
                    putStrLn "Already selected"
                    gameLoop game
                InvalidSelection -> do -- Unlikely to ever be seen
                    putStrLn "Invalid game state achieved"
                    gameLoop game
            

-- Starts a game of Number Scrabble. Calls gameLoop for logic
playPick15 :: IO ()
playPick15 =  
    do
        putStrLn "press 'q' to quit"
        let game = newGame
        gameLoop game
        


-- Converts a Char to a Maybe Number_1to9, depending on if it can be mapped properly.
-- Criteria is just 1 -> N1 etc
charToNum :: Char -> Maybe Number_1to9
charToNum character = case character of 
    '1' -> Just N1
    '2' -> Just N2
    '3' -> Just N3
    '4' -> Just N4
    '5' -> Just N5
    '6' -> Just N6
    '7' -> Just N7
    '8' -> Just N8
    '9' -> Just N9 
    _ -> Nothing