{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.BDD.Tape
import Data.BDD.Types

data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Eq, Show)
data Move = Move {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos
    deriving Show

check :: Int -> Int -> TapeM s Bool
check n m = start >>= go >>= possible
  where
    start :: TapeM s NodeId
    start = startStep NodeFalse (Pos 0 0)

    -- |Construct a BDD representing the initial position
    startStep :: NodeId -> Pos -> TapeM s NodeId
    startStep !acc pos = apply BOr acc =<< foldM (startStep' pos) NodeTrue positions

    startStep' :: Pos -> NodeId -> Pos -> TapeM s NodeId
    startStep' pos0 !acc pos
        -- The knight is here
        | pos0 == pos = apply BAnd acc =<< build (EIdRef (vis pos) :&: EIdRef (here pos))
        -- Or it isn't
        | otherwise   = apply BAnd acc =<< build (ENot (EIdRef (vis pos)) :&: ENot (EIdRef (here pos)))
     
    -- |Perform the computation until a fixpoint is reached
    go :: NodeId -> TapeM s NodeId
    go !prev = do
        my <- step prev
        if my == prev
        then pure my
        else go my

    -- |Perform one step
    step :: NodeId -> TapeM s NodeId
    step !prev = foldM (stepMove prev) prev moves

    -- |Perform a move: first, restrict the BDD only to the vertices
    -- consistent with the move's source, then construct an updated BDD
    -- representing the state after the move is made.
    -- Moreover, it accumulates the result over many moves.
    stepMove :: NodeId -> NodeId -> Move -> TapeM s NodeId
    stepMove prev !acc (Move from to) = do
        c <- build $
            (ENot . EIdRef $ here from)
               :&: (EIdRef $ here to)
               :&: (EIdRef $ vis to)

        restrict (here from) True prev >>= restrict (vis to) False
            >>= restrict (here to) False >>= apply BAnd c >>= apply BOr acc
    
    -- |Variable denoting that the knight has visited the position
    vis :: Pos -> Index
    vis (Pos x y) = Index $ 2 * (x * m + y)

    -- |Variable denoting that the knight is currently at the position
    here :: Pos -> Index
    here (Pos x y) = Index $ 2 * (x * m + y) + 1

    -- |Valid move differences ;)
    diffs :: [(Int, Int)]
    diffs = [id, uncurry $ flip (,)] <*> ((,) <$> [-1, 1] <*> [2, -2])

    moves :: [Move]
    moves = [ Move (Pos x y) (Pos x' y')
            | Pos x y <- positions
            , (dx, dy) <- diffs
            , let (x', y') = (x + dx, y + dy)
            , 0 <= x' && x' < n
            , 0 <= y' && y' < m
            ]

    -- |Check if the BDD represents a full circuit
    possible :: NodeId -> TapeM s Bool
    possible nodeId = (NodeFalse /=) <$> foldM possibleGo nodeId positions

    possibleGo :: NodeId -> Pos -> TapeM s NodeId
    possibleGo node pos = restrict (vis pos) True node

    -- |All possible positions
    positions :: [Pos]
    positions = Pos <$> [0..n - 1] <*> [0..m - 1]

main :: IO ()
main = do
    (n, m) <- readLn
    print $ run (check n m)
