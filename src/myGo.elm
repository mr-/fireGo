import Signal exposing (Signal, sampleOn, foldp, (<~))
import Signal
import Mouse
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (map, reverse, partition)
import Color
import Array exposing (Array)
import Array
import Maybe exposing (withDefault, Maybe (Just, Nothing))
import Html exposing (li, text, ul)

-----------
-- Model --
-----------


type Color = Black | White
                   
type alias Play = { point : (Int, Int)
                  , color : Color
                  }

-- The initial states contains an array filled with "-1"
-- If a stone is placed, it's changed to 0 (black) or 1 (white)
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

-- Modifies a state adding a stone
click : (Int, Int) -> List Play -> List Play
click point state = case partition (hasCoordinates point) state of 
    ([], []) -> [{point = point, color = Black}]
    ([], _) -> {point = point, color = swap (getLastColor state) } :: state
    (d, rest) -> rest
    
----------
-- View --
----------

display : List Play -> Element
display s = collage 600 600 <| grid ++ drawStones s

drawStones : List Play -> List Form
drawStones state = reverse (map stone state)

-- Draws a stone 
stone : Play -> Form
stone play =
    let (x, y) = play.point
        p a = toFloat (-270 + 30*a)
        co =
          if | play.color == Black -> Color.black
             | play.color == White  -> Color.white
        fill = move (p x, p y) <| filled co <| circle 15
        border = move (p x, p y) <| outlined (solid Color.black) <| circle 15
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
main = display <~ (foldp click initialState input)

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
