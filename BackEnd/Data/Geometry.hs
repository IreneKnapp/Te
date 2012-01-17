module Data.Geometry
  (Coordinate(..),
   Point(..),
   Size(..),
   Rectangle(..),
   pointInRectangle)
  where

import Data.Int


type Coordinate = Int64
type Point = (Coordinate, Coordinate)
type Size = (Coordinate, Coordinate)
type Rectangle = (Point, Size)


pointInRectangle :: Point -> Rectangle -> Bool
pointInRectangle (x, y) ((left, top), (width, height)) =
  let inHorizontal = (x >= left) && ((x - left) < width)
      inVertical = (y >= top) && ((y - top) < height)
  in inHorizontal && inVertical
