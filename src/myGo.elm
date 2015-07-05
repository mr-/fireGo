module MyGo where

import Signal exposing (Signal, sampleOn, foldp, (<~))
import Signal
import String
import Mouse
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (map, reverse, partition)
import Color
import Maybe exposing (withDefault, Maybe (Just, Nothing))
import Maybe.Extra exposing (join)
import Json.Encode as JE
import Json.Decode as JD exposing ( (:=) )



-- JSON

encodePoint : (Int, Int) -> String
encodePoint p = let (x, y) = p
                    rep = JE.object [ ("x", JE.int x), ("y", JE.int y)]
                in JE.encode 0 rep

decoder = JD.maybe (JD.object2 (,) ("x" := JD.int) ("y" := JD.int))

decodePoint : String -> Maybe (Int, Int)
decodePoint s = JD.decodeString decoder s |> Result.toMaybe |> join

mdecodePoint : Maybe String -> Maybe (Int, Int)
mdecodePoint s = s `Maybe.andThen` (\x -> JD.decodeString decoder x |> Result.toMaybe |> join)

-----------
-- Model --
-----------
type Color = Black | White
                   
type alias Play = { point : (Int, Int)
                  , color : Color
                  }
                  
type alias State = { board : List Play
                   , captures : (Int, Int)
                   }

initialState : List Play
initialState = []


------------
-- Update --
------------

swap : Color -> Color
swap c =
  if | c == Black -> White
     | c == White -> Black
     
getLastColor : List Play -> Color
getLastColor state = case state of
  [] -> Black
  x :: xs -> x.color

hasCoordinates : (Int, Int) -> Play -> Bool
hasCoordinates p play = p == play.point

unwrapMaybe : Maybe (Int, Int) -> List Play -> List Play
unwrapMaybe point state = case point of
    Just p  -> click p state
    Nothing -> state

-- Modifies a state adding a stone
click : (Int, Int) -> List Play -> List Play
click point state = case partition (hasCoordinates point) state of 
    ([], []) -> [{point = point, color = Black}]
    ([], _) -> {point = point, color = swap (getLastColor state) } :: state
    (d, rest) -> rest
    
----------
-- View --
----------
toColor : Color -> Color.Color
toColor c = case c of
    Black -> Color.black
    White -> Color.white

display : List Play -> Element
display s = collage 600 600 <| grid ++ drawStones s

drawStones : List Play -> List Form
drawStones state = case state of 
    []      -> [] 
    x :: xs -> (drawLastStone x) :: map stone xs

drawLastStone : Play -> Form
drawLastStone play =
    let fill = move (p play.point) <| filled (toColor (swap play.color)) <| circle 5
    in group [stone play, fill]

p : (Int, Int) -> (Float, Float)
p point = let q a = toFloat (-270 + 30 * a)
              (x,y) = point
          in (q x, q y)

-- Draws a stone 
stone : Play -> Form
stone play =
    let 
        fill = move (p play.point) <| filled (toColor play.color) <| circle 15
        border = move (p play.point) <| outlined (solid Color.black) <| circle 15
    in group [fill, border]


-- Draw the board lines
-- The distances are hardcoded: 30px between lines and 540px the full board
grid : List Form
grid =
    let segV s i = traced (solid Color.black) <| segment (i,-s/2) (i,s/2)
        segH s i = traced (solid Color.black) <| segment (-s/2,i) (s/2,i)
        coords = map (\x -> x*30) [-9..9]
        cPoints = [(0,0)
                  ,(-180,0),(180,0),(0,-180),(0,180)
                  ,(-180,-180),(-180,180),(180,-180),(180,180)
                  ]
        fill (x,y) = move (x, y) <| filled Color.black <| circle 4
    in (map (\x -> (segV 540 x)) coords) ++
       (map (\x -> (segH 540 x)) coords) ++
       (map fill cPoints)
       
       
main : Signal Element
main = display <~ (foldp unwrapMaybe initialState (Signal.map (\x -> x `Maybe.andThen` decodePoint) incomingMove))

-------------
-- Signals --
-------------

-- Transform from pixels to coordinates
toCoords : (Int,Int) -> (Int,Int)
toCoords (x,y) =
    let x' = (x - 15) // 30
        y' = (600 - y - 15) // 30
    in (x', y')

-- Update with the coordinates when clicked
input : Signal (Int, Int)
input = sampleOn Mouse.clicks (Signal.map toCoords Mouse.position)


port moveSink : Signal String
port moveSink = Signal.map encodePoint input

port debugSink : Signal String
port debugSink = Signal.map2 (String.append) (Signal.map toString incomingMove) (Signal.map (\x -> mdecodePoint x |> toString) incomingMove)

port incomingMove : Signal (Maybe String)
