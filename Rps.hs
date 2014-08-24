import Data.Char (toUpper)
import System.IO (stdout, hFlush)
import System.Random

-- Data type definition
data Winner = PLAYER | COMPUTER | DRAW deriving Show
data Choice = ROCK | PAPER | SCISSORS deriving (Show, Read, Enum, Bounded)

-- Winning moves
winner :: Choice -> Choice -> Winner
winner ROCK PAPER       = COMPUTER
winner PAPER SCISSORS   = COMPUTER
winner SCISSORS ROCK    = COMPUTER
winner PAPER ROCK       = PLAYER
winner SCISSORS PAPER   = PLAYER
winner ROCK SCISSORS    = PLAYER
winner x y              = DRAW

-- Scoring
updateScores :: (Num a) => (a, a) -> Winner -> (a, a)
updateScores (scorePlayer, scoreComputer) PLAYER = (scorePlayer + 1, scoreComputer)
updateScores (scorePlayer, scoreComputer) COMPUTER = (scorePlayer, scoreComputer + 1)
updateScores scores DRAW = scores

-- Play a single round
instance Random Choice where
    randomR (a, b) g =
        case randomR (fromEnum a, fromEnum b) g of
            (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

play :: StdGen -> IO (Winner, StdGen)
play g = do
    putStr "Choose between rock, paper and scissors: "
    hFlush stdout
    input <- getLine
    playerChoice <- return (read (map toUpper input))
    (computerChoice, g) <- return (random g :: (Choice, StdGen))
    putStrLn $ "Computer: " ++ (show computerChoice)
    return ((winner playerChoice computerChoice), g)

-- "Play again" prompt
askPlayAgain :: IO Bool
askPlayAgain = do
    putStr "Play again? [y/n] "
    hFlush stdout
    input <- getLine
    case input of
        "y" -> return True
        "n" -> return False
        _   -> askPlayAgain

-- Main loop: plays, shows result, update scores and ask to play again
playRound :: (Num a, Show a) => StdGen -> (a, a) -> IO ()
playRound g scores = do
    (whoWon, g) <- play g
    case whoWon of PLAYER   -> putStrLn "The player won"
                   COMPUTER -> putStrLn "The computer won"
                   DRAW     -> putStrLn "It was a draw"

    scores <- return (updateScores scores whoWon)
    putStrLn $ "Score is " ++ (show $ fst scores) ++ " to " ++ (show $ snd scores)

    again <- askPlayAgain
    case again of
        False -> return ()
        True -> playRound g scores

-- Entry point: just starts playRound with null scores
main = do
    g <- newStdGen
    playRound g (0, 0)
