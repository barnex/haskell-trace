module Object where

import Data.Maybe
import Data.List
import Ray
import Types
import Vector
import System.Random
import Control.Monad.Trans.RWS

type JefDeMonad = RWST Env () StdGen IO

type Object = Env -> Ray -> RecDepth -> RWST Env () StdGen IO (Maybe (Distance, Colour))

data Env = Env { 
           scene :: [Object],
           backgroundColour::Colour,
           light :: Vector
    }

--
-- | findColour determines the color seen by a ray
findColour :: Env -> Ray -> RecDepth -> JefDeMonad Colour
findColour env ray recDepth =
  if recDepth > 0 then 
    do
      let shapes = scene env
      let f shape = shape env ray recDepth
      maybeDistancesAndColours <- sequence $ f <$> shapes
      let sortedTs = sortOn fst $ catMaybes maybeDistancesAndColours
      case sortedTs of
        [] -> return $ backgroundColour env
        (x:_) -> return $ snd x
  else
     if recDepth < 0 then
        return undefined
      else 
        return $ Colour 0.0 0.5 0.0 -- TODO: what colour should we select here?
    

