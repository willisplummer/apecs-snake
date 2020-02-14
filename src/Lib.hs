{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( main
    ) where

import Apecs
import Apecs.Gloss
import Linear
import System.Random
import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)

newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position

newtype PosArr = PosArr [Position] deriving Show
instance Component PosArr where type Storage PosArr = Map PosArr

data Dir = North | South | East | West deriving (Show, Eq)

newtype Direction = Direction Dir deriving Show
instance Component Direction where type Storage Direction = Map Direction

data Food = Food deriving Show
instance Component Food where type Storage Food = Map Food

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

newtype Score = Score Int deriving (Show, Num)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score

makeWorld "World" [''Position, ''PosArr, ''Direction, ''Player, ''Food, ''Score, ''Camera]

type System' a = System World a

playerSpeed :: Float
playerSpeed = 20

xmin, xmax :: Float
xmin = -100
xmax = 100

playerPos, scorePos :: V2 Float
playerPos = V2 0 (-120)
scorePos  = V2 xmin (-170)

initialize :: System' ()
initialize = do
  playerEty <- newEntity (Player, PosArr [Position playerPos], Direction North)
  return ()

stepPosition :: Float -> System' ()
stepPosition dT = cmap updatePosArr
    where
        update (Player, PosArr arr, ) =

        updatePosArr (Player, PosArr pArr, Direction East) = PosArr $ trans <$> pArr
            where
                trans :: Position -> Position
                trans (Position p) = Position (p + dT *^ V2 (-playerSpeed) 0)
        updatePosArr (Player, PosArr pArr, Direction West) = PosArr $ trans <$> pArr
            where
                trans :: Position -> Position
                trans (Position p) = Position (p + dT *^ V2 playerSpeed 0)

        updatePosArr (Player, PosArr pArr, Direction North) = PosArr $ trans <$> pArr
            where
                trans :: Position -> Position
                trans (Position p) = Position (p + dT *^ V2 0 playerSpeed)

        updatePosArr (Player, PosArr pArr, Direction South) = PosArr $ trans <$> pArr
            where
                trans :: Position -> Position
                trans (Position p) = Position (p + dT *^ V2 0 (-playerSpeed))

clampPlayer :: System' ()
clampPlayer = cmap $ \(Player, Position (V2 x y))
                   -> Position (V2 (min xmax . max xmin $ x) y)

step :: Float -> System' ()
step dT = do
  stepPosition dT
  clampPlayer

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
  cmap $ \(Player, Direction dir) -> case dir of
      West -> Direction West
      _ -> Direction East

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  cmap $ \(Player, Direction dir) -> case dir of
      East -> Direction East
      _ -> Direction West

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) =
  cmap $ \(Player, Direction dir) -> case dir of
      South -> Direction South
      _ -> Direction North

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) =
  cmap $ \(Player, Direction dir) -> case dir of
      North -> Direction North
      _ -> Direction South

handleEvent (EventKey (SpecialKey KeyEsc) Down   _ _) = liftIO exitSuccess

handleEvent _ = return ()

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y

square :: Picture
square = Line [(0,0),(0,1),(1,1),(1,0)]

draw :: System' Picture
draw = do
  player  <- foldDraw $ \(Player, PosArr pArr) -> translate' (head pArr) . color white  . scale 10 20 $ square

  Score s <- get global
  let score = color white . translate' (Position scorePos) . scale 0.1 0.1 . Text $ "Score: " ++ show s

  return $ player <> score


main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play (InWindow "Snake" (220, 360) (10, 10)) black 60 draw handleEvent step
