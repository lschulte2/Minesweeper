module GameDraw
         (renderWorld
         )  where

import GameTypes
import Linear
import Graphics.Gloss.Data.Picture hiding (pictures)
import Graphics.Gloss as G hiding (pictures)
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Control.Lens
import Data.Maybe
import Data.Array
import Data.ByteString as B
import qualified Data.Map as M
import Control.Monad.Trans.RWS.Lazy

renderWorld :: GameT Picture
renderWorld = do 
      opts  <- ask
      w     <- get
      let worldSize = w^.minefield.to bounds.to snd
      return $ addStatView opts w $ adjustPos opts worldSize $ Pictures $ renderEnt opts <$> w^.minefield.to itoList

adjustPos :: GameConfig -> V2 Integer -> Picture -> Picture
adjustPos opts ws = translate (ws^._y.to fromInteger.to (*(ts/2)).to negate) (ws^.x.to fromInteger.to (*(ts/2)))
   where ts = opts^.tileSize

renderEnt :: GameConfig -> (Position,Entity Stats) -> Picture
renderEnt opts (p,Floor (Stats True False _ )) = placeAt opts p $ getPicOrError "FLOORcovered.png" $ opts^.pictures.at "FLOOR.png"
renderEnt opts (p,Floor (Stats True True _ )) = placeAt opts p $ getPicOrError "FLOORwithFlag.png" $ opts^.pictures.at "FLOORwithFlag.png"  
renderEnt opts (p,Floor (Stats False False True _ )) = placeAt opts p $ getPicOrError "FLOORmine.png" $ opts^.pictures.at "FLOORmine.png"  
renderEnt opts (p,Floor (Stats True False False number )) = if number > 0 then  else placeAt opts p $ getPicOrError "FLOORuncovered.png" $ opts^.pictures.at "FLOORuncovered.png"


getPicOrError :: String -> Maybe Picture -> Picture
getPicOrError key = fromMaybe (error $ "Missing graphics: " ++  key)

withBG :: GameConfig -> String -> Picture -> Picture 
withBG opts key pic = Pictures [getPicOrError key $ opts^.pictures.at key, pic]

placeAt :: GameConfig -> Position -> Picture -> Picture
placeAt opts pos = translate (pos^.y.to fromInteger.to (*ts)) (pos^._x.to fromInteger.to negate.to (*ts))
   where
      ts = opts^.tileSize


