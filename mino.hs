--Justin RUELLAND & Raphael MALAK TD5 L2
--Enseignant Beatrice Napolitano
module Main where

import Data.Function ((&))
import GHC.Base
import System.IO ( Handle
                 , withFile
                 , IOMode(ReadMode)
                 , hGetContents
                 , hGetLine
                 , hClose
                 , openFile)
import Data.List ( elemIndices
                 , nub
                 , sortOn
                 )

data Tile = W | EU | EV | T | M | R deriving (Eq, Enum)

type Y = Int
type X = Int
type Coord = (Y, X)
type Game = ([[Tile]], Coord)

instance Read Tile where
  readsPrec _ ['*'] = pure (W, "")
  readsPrec _ ['o'] = pure (EU, "")
  readsPrec _ ['x'] = pure (EV, "")
  readsPrec _ ['T'] = pure (T, "")
  readsPrec _ ['M'] = pure (M, "")
  readsPrec _ _     = empty

instance Show Tile where
  show W  = "\ESC[m*"
  show EU = "\ESC[mo"
  show EV = "\ESC[36m\ESC[41mx"
  show T  = "\ESC[mT"
  show M  = "\ESC[mM"
  show R = "Pas de solution"

readGame :: Handle -> IO Game
readGame h = do
  dimL <- hGetLine h
  dimC <- hGetLine h
  contents <- lines <$> hGetContents h 
  let maze = map read . words <$> contents
  let (x,y) = emplacement T maze
  return (maze, (x,y))

showGame :: Game -> String
showGame (ts, _) = map (unwords . map show) ts
                 & unlines

main :: IO ()
main = do
  putStrLn "File where labyrinth is stored:"
  ln <- getLine
  game <- openFile ln ReadMode 
  (maze,(x,y)) <- readGame game
  putStrLn "Labyrinth read:"
  putStrLn $ showGame (maze, (x,y))
  hClose game
  let solmaze = verif (solve (maze, (x,y)))
  putStrLn "Solution:"
  putStrLn $ showGame solmaze

verif:: Maybe Game -> Game
verif solmaze 
  | solmaze == Nothing = ([[R]], (0,0))
  | otherwise = extract solmaze

extract:: Maybe Game -> Game
extract (Just maze) = maze

emplacement :: Tile -> [[Tile]] -> (Int, Int)
emplacement the table = (posx, (indicecol the (table!!(posx - 1))))
                    where posx = positionx the table

positionx :: Tile -> [[Tile]] -> Int
positionx _ [] = 0
positionx the (x:xs) 
  | elem the x = 1
  | otherwise =  1 + (positionx the xs)

indicecol:: Tile -> [Tile] -> Int
indicecol _ [] = 0
indicecol the (x:xs)
  | x == the = 1
  | otherwise = 1 + (indicecol the xs)

step:: Game -> [Game]
step jeu = (stepR jeu) ++ (stepL jeu) ++ (stepU jeu) ++ (stepD jeu)

stepR:: Game -> [Game]
stepR (table, (x,y))
  | y == (length (table !! 0))  = []
  | trouve (table, (x,y)) = []
  | elem (table!!(x-1)!!y) [M] = [(table, (x,y+1))]
  | elem (table!!(x-1)!!y) [EU] = [ modifR (table, (x,y))]
  | otherwise = [] 

modifR :: Game -> Game 
modifR (table, (x,y)) = (take (x-1) table ++ [l] ++ drop (x) table, (x,y+1))
   where 
     k = table!!(x-1)
     l = take (y) k ++ [EV] ++ drop (y+1) k

stepL:: Game -> [Game]
stepL (table, (x,y))
  | y == 1 = []
  | trouve (table, (x,y)) = []
  | elem (table!!(x-1)!!(y-2)) [M] = [(table, (x,y-1))]
  | elem (table!!(x-1)!!(y-2)) [EU] = [ modifL (table, (x,y))]
  | otherwise = [] 

modifL :: Game -> Game
modifL (table, (x,y)) = (take (x-1) table ++ [l] ++ drop (x) table, (x,y-1))
   where 
     k = table!!(x-1)
     l = take (y-2) k ++ [EV] ++ drop (y-1) k

stepU:: Game -> [Game]
stepU (table, (x,y))
  | x == 1 = []
  | trouve (table, (x,y)) = []
  | elem (table!!(x-2)!!(y-1)) [M] = [(table, (x-1,y))]
  | elem (table!!(x-2)!!(y-1)) [EU] = [ modifU (table, (x,y))]
  | otherwise = [] 

modifU :: Game -> Game 
modifU (table, (x,y)) = (take (x-2) table ++ [l] ++ drop (x-1) table, (x-1,y))
   where 
     k = table!!(x-2)
     l = take (y-1) k ++ [EV] ++ drop y k

stepD:: Game -> [Game]
stepD (table, (x,y))
  | x == (length table)  = []
  | trouve (table, (x,y)) = []
  | elem (table!!x!!(y-1)) [M] = [(table, (x+1,y))]
  | elem (table!!x!!(y-1)) [EU] = [ modifD (table, (x,y))]
  | otherwise = [] 

modifD :: Game -> Game 
modifD (table, (x,y)) = (take (x) table ++ [l] ++ drop (x+1) table, (x+1,y))
   where 
     k = table!!(x)
     l = take (y-1) k ++ [EV] ++ drop y k


trouve:: Game -> Bool
trouve (table, (x,y)) = table!!(x-1)!!(y-1) == M

stepink :: Int -> Game -> [Game]
stepink 1 jeu = step jeu
stepink k jeu = stepink (k-1) jeu >>= step

solveink :: Int -> Game -> [Game]
solveink 1 jeu = filter trouve (step jeu)
solveink k jeu = filter trouve (stepink k jeu)

solve::Game -> Maybe Game 
solve jeu = solvebis 1 jeu

solvebis :: Int -> Game -> Maybe Game
solvebis n (table, (x,y))  
  | n > (length table)*(length (table!!0)) = Nothing 
  | solveink n (table, (x,y)) /= [] = Just (solveink n (table, (x,y))!!0)
  | otherwise = solvebis (n+1) (table, (x,y)) 










 
