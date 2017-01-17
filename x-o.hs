import Control.Monad
import Control.Applicative
import Data.Either
import Data.List
import Data.List.Split


data Symbol = X | O deriving (Show, Eq)
type Cell = Either Int Symbol
type Grid = [[Cell]]

data GameState = Game { getGrid :: Grid, activePlayer :: Symbol }

initialState :: GameState
initialState = Game ( map (map Left) $ chunksOf 3 [1..9] ) X



showSquare :: (Show a, Show b) => Either a b -> String
showSquare (Left k) = " " ++ show k ++ " "
showSquare (Right k) = " " ++ show k ++ " "


showGrid :: Grid -> String
showGrid g = unlines . surround "+---+---+---+" . map ( concat . surround "|" . map showSquare ) $ g
             where surround x xs = [x] ++ intersperse x xs ++ [x]

printGrid :: Grid -> IO ()
printGrid = putStr . showGrid

possibleMoves :: Grid -> [Int]
possibleMoves g = [k | Left k <- concat g]


makeMove :: Int -> GameState -> Maybe GameState
makeMove m (Game grid player)
        | m `elem` possibleMoves grid = Just $ Game (map (map replace) grid) (switch player)
        | otherwise = Nothing
        where
        replace (Left k) | k == m = Right player
        replace x = x

        switch :: Symbol -> Symbol
        switch X = O
        switch O = X


playGame :: GameState -> IO ()
playGame g@(Game grid player) = do
    printGrid grid
    putStrLn $ "Player " ++ show player ++ " turn!"
    putStr $ "Choose your move <<<<<<< "
    move <- fmap read getLine
    case makeMove move g of
      Nothing -> do
        putStrLn "Invalid move, choose again!"
        playGame g
      Just ng -> playGame ng


main :: IO ()
main = playGame initialState
