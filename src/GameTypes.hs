{-# Language TemplateHaskell #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric #-}
{-# Language DefaultSignatures #-}
{-# Language DeriveAnyClass #-}
module GameTypes where

import Control.Lens
import Control.Monad.RWS.Lazy
import qualified GHC.Generics as Gen
import Linear
import Data.Array
import Data.Map as M
import Data.Default
import Graphics.Gloss.Data.Picture

-- | define an alias for our transformer
type GameT = RWST GameConfig Log World Identity

-- | type of Log Entry
data LogLevel = Debug | Info | Warning | Error deriving (Eq, Show,Ord)

-- | Alias for our Log
type Log = [(LogLevel, String)]

-- | World represents the current game state with a game mide,
-- a time counter,

data World
   = World
   { _mode :: Mode
   , _time :: Integer
   , _minefield :: Minefield
   } deriving (Show,Eq)

-- | Mode can be one of three things:
--
--   - Active – the game is currently running; player can control hero
--   - Ended – the game has ended; hero or boss is dead
--   - Paused – the game is paused; player can only unpause or restart the game

data Mode
   = Active
   | Ended
   | Paused
    deriving (Show,Eq)

-- | An standard, immutable array which associates all immovable, passive
--   entities on the minefield with its coordinates (Position)

type Minefield = Array Position (Entity Stats)

-- | An standard, lazy map which associates all movable, active entities on the
--   minefield with its coordinates (Position)
type Entities = M.Map Position (Entity Stats)

-- | An standard, lazy map for the graphics; keys are the associated file names
type Pics = M.Map String Picture

-- | Position of an entity in the world
type Position = V2 Integer

type WorldCondition = Position -> World -> Bool
type WorldConsequence = Position -> GameT ()

newtype Events = Events {unEvent :: Map String (WorldCondition, WorldConsequence)}

instance Show Events where
   show (Events m) = show $ keys m

instance Eq Events where
   (Events a) == (Events b) = keys a == keys b

-- | Entity can be one of things:

--    -Mine - if revealed loose the Game
--    -Floor - can be covered or revealed, can be flagged, can contain a mine or an Integer
data Entity s
   = Floor {_stats :: s, _on :: Events }
   | Wall
   deriving (Show, Eq, Functor)

data Stats 
   = Stats
   { _covered :: Bool
   , _flagged :: Bool
   , _mine :: Bool
   , _number :: Integer
   , _alreadyUpdated :: Bool
   } deriving (Show,Eq)

instance Default Stats where 
   def = Stats 0 North 0 True

-- | Map of Effects
newtype Effects = Effects {_unEffect :: Map String (EffectCondition Integer Item, EffectConsequence Integer Item)}

instance Show Effects where
  show (Effects m) = show $ keys m

instance Eq Effects where
  (Effects a) == (Effects b) = keys a == keys b

-- | Converts to a vector (V2 Integer)
toVector :: Direction -> DirVector
toVector North = V2 (-1)  0
toVector South = V2   1   0
toVector West  = V2   0 (-1)
toVector East  = V2   0   1

-- | Gets a Direction from a vector (V2 Integer)
fromVector :: DirVector -> Direction
fromVector vec =
  case abs <$> vec of
       v | v^._x >  v^._y && vec^._x >  0 -> South
         | v^._x >  v^._y && vec^._x <= 0 -> North
         | v^._x <= v^._y && vec^._y >  0 -> East
         | otherwise                      -> West

data Action 
   = Move { _dir :: Direction }
   | Pause
   | Check 
    deriving (Show,Eq)

data GameConfig = GameConfig
        { _pictures :: Pics
        , _tileSize :: Float
        , _levels   :: Array Integer World
        , _keyMap   :: Char -> Maybe Action
        }

{- * Lenses for the World type -}
$(makeLenses ''World)

{- * Lenses for the Stats type -}
$(makeLenses ''Stats)

{- * Prisms for the Action type -}
$(makePrisms ''Action)

{- * Traversals for the Action type -}
$(makeLenses ''Action)

{- * Prisms for the Entity type -}
$(makePrisms ''Entity)

{- * Traversals for the Entity type -}
$(makeLenses ''Entity)

{- * Traversals for the Effects type -}
$(makeLenses ''Effects)

{- * Traversals for the EffectTrigger type -}
$(makeLenses ''EffectTrigger)

$(makeLenses ''GameConfig)



