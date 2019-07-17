-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module TicTacToe where

import Data.List as List
import Control.Parallel.Strategies as Par

data Position = Position NoCro NoCro NoCro
                         NoCro NoCro NoCro
                         NoCro NoCro NoCro
  deriving (Show, Eq)

showPosition :: Position -> String
showPosition (Position ul um ur ml mm  mr ll lm lr) =
  (showNoCro ul) ++ " | " ++ (showNoCro um) ++ " | " ++ (showNoCro ur) ++
  "\n----------\n" ++
  (showNoCro ml) ++ " | " ++ (showNoCro mm) ++ " | " ++ (showNoCro mr) ++
  "\n----------\n" ++
  (showNoCro ll) ++ " | " ++ (showNoCro lm) ++ " | " ++ (showNoCro lr)

position0 = Position Empty Empty Empty Empty Empty Empty Empty Empty Empty

listToPosition [ul, um, ur, ml, mm,  mr, ll, lm, lr] = Position ul um ur ml mm  mr ll lm lr
positionToList (Position ul um ur ml mm  mr ll lm lr) = [ul, um, ur, ml, mm,  mr, ll, lm, lr]

data NoCro = X | O | Empty
  deriving (Eq, Show)

showNoCro X = "X"
showNoCro O = "O"
showNoCro Empty = " "

-- positions that can be reached via a single move
moves :: Position -> [Position]
moves position =
  case gameOver position of
    Just _ -> []
    Nothing ->
      map listToPosition (moves' (nextPlayer position) (positionToList position))

-- generate a list of all possible nought / cross placements
moves' :: NoCro -> [NoCro] -> [[NoCro]]
moves' player [] = []
moves' player (Empty : rest) = (player : rest) : (map ((:) Empty) (moves' player rest))
moves' player (p : rest) = map ((:) p) (moves' player rest)

-- find nocros along winning axes of TTT
axes :: Position -> [[NoCro]]
axes (Position ul um ur ml mm mr ll lm lr) =
  [[ul, um, ur], [ml, mm, mr], [ll, lm, lr], -- lines
   [ul, ml, ll], [um, mm, lm], [ur, mr, lr], -- columns
   [ul, mm, lr], [ur, mm, ll]] -- diagonals

-- did Noughts or Crosses win the game already?
won :: Position -> NoCro -> Bool
won position nocro =
  any (all ((==) nocro)) (axes position)

gameOver :: Position -> Maybe NoCro
gameOver position =
  if won position X
  then Just X
  else if won position O
  then Just O
  else if all ((/=) Empty) (positionToList position)
  then Just Empty
  else Nothing

nextPlayer :: Position -> NoCro
nextPlayer position =
  let lis = positionToList position
      xs = filter ((==) X) lis
      os = filter ((==) O) lis
  in if length xs <= length os
     then X
     else O


parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f xs = runEval (parallelMap' f xs)

parallelMap' :: (a -> b) -> [a] -> Eval [b]
parallelMap' f [] = return []
parallelMap' f (x:xs) = 
  do x' <- rpar (f x) -- f x, aber in einem separaten "Prozess"
     xs' <- parallelMap' f xs
     return (x':xs')

data Tree a = Tree a [Tree a]
    deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Tree a subTrees) = Tree (f a) (map (\ subTree -> mapTree f subTree) subTrees)

instance Functor Tree where
  -- fmap f (fmap g x) = fmap (f . g) x
  -- fmap id x = x
  fmap = mapTree

generateTree :: (a -> [a]) -> a -> Tree a
generateTree f a = Tree a (map (generateTree f) (f a))

-- TicTacToe Spielbaum Berechnung
gameTree :: Position -> Tree Position
gameTree position = generateTree moves position

countTree :: Tree a -> Integer
countTree (Tree _ subTrees) = 1 + foldr (+) 0 (map countTree subTrees)

-- alles aus der Sicht des Computers!
-- grobe Schätzung für Qualität einer Position
staticScore :: Position -> Integer
staticScore position = 
  if won position X
  then 1
  else if won position O
  then -1
  else 0

maximize :: Tree Integer -> Integer
maximize (Tree static []) = static
maximize (Tree static subTrees) =  
  maximum (map minimize subTrees)

minimize :: Tree Integer -> Integer
minimize (Tree static []) = static
minimize (Tree static subTrees) = 
  minimum (map maximize subTrees)

-- Bewertungsfunktion, 0. Version
evaluate0 :: Position -> Integer
evaluate0 position = (maximize . fmap staticScore . gameTree) position

-- Spielbaum auf eine bestimmte Tiefe beschränken
pruneTree :: Integer -> Tree a -> Tree a
pruneTree 0 (Tree a subTrees) = Tree a []
pruneTree n (Tree a subTrees) =
  Tree a (map (pruneTree (n-1)) subTrees)

evaluate1 position = (maximize . fmap staticScore . pruneTree 5 . gameTree) position

maximizeTree :: Tree (a, Integer) -> Tree (a, Integer)
maximizeTree tree@(Tree _ []) = tree -- Tree static []
maximizeTree (Tree (position, static) subTrees) = 
  -- minSubTrees :: [Tree (s, Integer)]
  let minSubTrees = parallelMap minimizeTree subTrees
      treeScore (Tree (_, score) _) = score -- Tree (position, score), _
      maxScore = maximum (map treeScore minSubTrees)
  in Tree (position, maxScore) minSubTrees
  

minimizeTree :: Tree (a, Integer) -> Tree (a, Integer)
minimizeTree tree@(Tree _ []) = tree -- Tree static []
minimizeTree (Tree (position, static) subTrees) = 
  let maxSubTrees = parallelMap maximizeTree subTrees
      treeScore (Tree (_, score) _) = score
      minScore = minimum (map treeScore maxSubTrees)
  in Tree (position, minScore) maxSubTrees

evaluate position = (maximizeTree . fmap (\ position -> (position, staticScore position)) . pruneTree 5 . gameTree) position

maxSubTree :: Tree (a, Integer) -> Tree (a, Integer)
maxSubTree (Tree a subTrees) = 
  let compareTrees (Tree (_, score1) _) (Tree (_, score2) _) = compare score1 score2
  in maximumBy compareTrees subTrees

putAt :: Position -> Integer -> NoCro -> Position
putAt position index nocro = 
  let lis = positionToList position
      i = fromInteger index
  in listToPosition ((take i lis) ++ [nocro] ++ (drop (i + 1) lis))

playGame :: Position -> IO ()
playGame position =
  do putStrLn (showPosition position)
     case gameOver position of
      Just X -> putStrLn "Computer wins!"
      Just O -> putStrLn "Player wins!"
      Just Empty -> putStrLn "Tie"
      Nothing -> 
        case nextPlayer position of
          O -> do putStrLn "Player's turn"
                  putStrLn "enter index (0-8)"
                  s <- getLine -- s :: String, getLine :: IO String, works on monads
                  let index = (read s) :: Integer
                  playGame (putAt position index O)
          X ->
            do putStrLn "Computer's turn"
               let (Tree (nextposition, _) _) = maxSubTree (evaluate position)
               playGame nextposition