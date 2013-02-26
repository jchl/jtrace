module Transformations (identity_t, trans_t, scale_t, uscale_t,
                        rotatex_t, rotatey_t, rotatez_t, applyTransform,
                        applyMatrixToPoint, applyMatrixToVector) where

import Types

identity :: Matrix
identity = ((1, 0, 0, 0),
            (0, 1, 0, 0),
            (0, 0, 1, 0),
            (0, 0, 0, 1))

identity_t :: Transform
identity_t = (identity, identity)

trans :: Point -> Matrix
trans (x, y, z) = ((1, 0, 0, x),
                   (0, 1, 0, y),
                   (0, 0, 1, z),
                   (0, 0, 0, 1))

trans_t :: Point -> Transform
trans_t p = (trans p, trans p')
   where
      p' = neg p
      neg (x, y, z) = (-x, -y, -z)

scale :: Point -> Matrix
scale (x, y, z) = ((x, 0, 0, 0),
                   (0, y, 0, 0),
                   (0, 0, z, 0),
                   (0, 0, 0, 1))

scale_t :: Point -> Transform
scale_t p = (scale p, scale p')
   where
      p' = inv p
      inv (x, y, z) = (1/x, 1/y, 1/z)

uscale :: Double -> Matrix
uscale r = scale (r, r, r)

uscale_t :: Double -> Transform
uscale_t r = (uscale r, uscale (1/r))

rotatex :: Double -> Matrix
rotatex r = (( 1,  0,  0,  0),
             ( 0,  c, -s,  0),
             ( 0,  s,  c,  0),
             ( 0,  0,  0,  1))
   where c = cos r
         s = sin r

rotatex_t :: Double -> Transform
rotatex_t r = (rotatex r, rotatex (-r))

rotatey :: Double -> Matrix
rotatey r = (( c,  0,  s,  0),
             ( 0,  1,  0,  0),
             (-s,  0,  c,  0),
             ( 0,  0,  0,  1))
   where c = cos r
         s = sin r

rotatey_t :: Double -> Transform
rotatey_t r = (rotatey r, rotatey (-r))

rotatez :: Double -> Matrix
rotatez r = (( c, -s,  0,  0),
             ( s,  c,  0,  0),
             ( 0,  0,  1,  0),
             ( 0,  0,  0,  1))
   where c = cos r
         s = sin r

rotatez_t :: Double -> Transform
rotatez_t r = (rotatez r, rotatez (-r))

rows :: Matrix -> (Vector, Vector, Vector, Vector)
rows = id

cols :: Matrix -> (Vector, Vector, Vector, Vector)
cols ((a1, a2, a3, a4),
      (b1, b2, b3, b4),
      (c1, c2, c3, c4),
      (d1, d2, d3, d4)) = ((a1, b1, c1, d1),
                           (a2, b2, c2, d2),
                           (a3, b3, c3, d3),
                           (a4, b4, c4, d4))

dotProduct :: Vector -> Vector -> Double
dotProduct (a1, a2, a3, a4) (b1, b2, b3, b4) =
   (a1 * b1 + a2 * b2 + a3 * b3 + a4 * b4)

mmult :: Matrix -> Matrix -> Matrix
mmult m1 m2 = ((a1, a2, a3, a4),
               (b1, b2, b3, b4),
               (c1, c2, c3, c4),
               (d1, d2, d3, d4))
   where
      a1 = dotProduct row1 col1
      a2 = dotProduct row1 col2
      a3 = dotProduct row1 col3
      a4 = dotProduct row1 col4
      b1 = dotProduct row2 col1
      b2 = dotProduct row2 col2
      b3 = dotProduct row2 col3
      b4 = dotProduct row2 col4
      c1 = dotProduct row3 col1
      c2 = dotProduct row3 col2
      c3 = dotProduct row3 col3
      c4 = dotProduct row3 col4
      d1 = dotProduct row4 col1
      d2 = dotProduct row4 col2
      d3 = dotProduct row4 col3
      d4 = dotProduct row4 col4
      (row1, row2, row3, row4) = rows m1
      (col1, col2, col3, col4) = cols m2

combineTransforms :: Transform -> Transform -> Transform
combineTransforms (m2, m2') (m1, m1') = (m2 `mmult` m1, m1' `mmult` m2')

applyTransform :: Transform -> Object -> Object
applyTransform t2 (Primitive k t1 s) = Primitive k (combineTransforms t2 t1) s
applyTransform t2 (Union o1 o2) = Union (applyTransform t2 o1) (applyTransform t2 o2)
applyTransform t2 (Intersection o1 o2) = Intersection (applyTransform t2 o1) (applyTransform t2 o2)
applyTransform t2 (Difference o1 o2) = Difference (applyTransform t2 o1) (applyTransform t2 o2)

applyMatrixToPoint :: Matrix -> Point -> Point
applyMatrixToPoint m (x, y, z) = (a1, a2, a3)
   where
      a1 = dotProduct row1 v
      a2 = dotProduct row2 v
      a3 = dotProduct row3 v
      (row1, row2, row3, row4) = rows m
      v = (x, y, z, 1)

applyMatrixToVector :: Matrix -> Point -> Point
applyMatrixToVector m (x, y, z) = (a1, a2, a3)
   where
      a1 = dotProduct row1 v
      a2 = dotProduct row2 v
      a3 = dotProduct row3 v
      (row1, row2, row3, row4) = rows m
      v = (x, y, z, 0)
