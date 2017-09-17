{-# LANGUAGE ScopedTypeVariables #-}
module GameLogic where

import GameTypes
import GameLog
import Control.Lens hiding (levels)
import Linear
import Data.Maybe
import Data.Char
import qualified Data.Array as A
import Data.Map as M
import Data.Default
import Control.Concurrent
import Control.Monad.RWS.Lazy

{- * Initialization -}
newWorld :: Minefield -> World
newWorld minef = World
               { _mode = Active
               , _time = 0
               ,_minefield = minef
               }

newMinefield:: (Integer,Integer) -> Minefield
newMinefield s@(x,y) = placeEnt Wall (atBorder s) minefield
   where minefield = A.array (V2 0 0,V2 x y) $ zip (V2 <$> [0..x] <*> [0..y]) $ repeat Floor

-- | Tests whether a position is on the edge of the map.
atBorder :: (Integer,Integer) -> Position -> Bool
atBorder (bx,by) (V2 x y) = x `elem` [bx,0] || y `elem` [by,0]

-- | Places an entity at a any position in a dungeon for which a predicate holds.
placeEnt :: Entity Stats -> (Position -> Bool) -> Minefield -> Minefield
placeEnt ent test = imap (\p c -> if test p then ent else c)




