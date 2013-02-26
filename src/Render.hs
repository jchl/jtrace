module Render (render) where

import Types
import Utils
import Machine
import Control.Monad.Identity (Identity(..))
import Transformations (applyMatrixToPoint, applyMatrixToVector)
import Debug.Trace

type Ray = (Point, Point) -- origin, unit vector in direction of view
type SurfaceProperties = (Point, Double, Double, Double)
type TextureCoords = (Int, Double, Double)

render :: Int -> Int -> Double -> Int -> Object -> [Light] -> Point -> [[Point]]
render height width fov depth obj lights amb = [[
   illumination depth obj lights amb (ray i j)
      | j <- [0 .. width - 1]]
      | i <- [0 .. height - 1]]
   where
      imageWidth = 2 * tan (0.5 * degToRad fov)
      pixelSize = (imageWidth / fromIntegral width)
      imageHeight = pixelSize * fromIntegral height

      leftEdge = -0.5 * imageWidth
      topEdge = 0.5 * imageHeight

      viewVec :: Int -> Int -> Point
      viewVec i j = (leftEdge + (fromIntegral j + 0.5) * pixelSize,
                     topEdge - (fromIntegral i + 0.5) * pixelSize,
                     1)

      ray :: Int -> Int -> Ray
      ray i j = ((0, 0, -1), viewVec i j)

findIntRayPlane :: Ray -> (Point, Double) -> Double
findIntRayPlane (ro, rd) (pn, d) = -(dotProduct pn ro + d) / (dotProduct pn rd)

plane :: Transform -> (Point, Double)
plane (m, m') = (n, d)
   where
      n = applyMatrixToVector m (0, 1, 0)
      p = applyMatrixToPoint m (0, 0, 0)
      d = (dotProduct p n) / (dotProduct n n)
      -- XXX if n is a unit vector, then the dotProduct with itself is 1

{-
 Have the unit normal N, and a point P0 in the plane.  Want to answer:

 for which value of d is dN in the plane?

 Plane can be defined by P such that (P - P0) . N == 0.  i.e. want:
 (dN - P0) . N == 0
 if N = (nx, ny, nz)
    P0 = (px, py, yz)
 => (dnx - px) * nx + (dny - py) * ny + (dnz - pz) * nz == 0
 => d(N.N) == P0.N
 => d = P0.N / N.N
-}

findIntersectionPrimitive :: ObjKind -> Transform -> Ray -> Maybe (Double, Point, TextureCoords)
findIntersectionPrimitive Plane (m, m') (start, dirn) = result
   where
      (n, d) = plane (m, m')
      t = findIntRayPlane (start, dirn) (n, d)
      p = start +++ (dirn *** t)
      (x, y, z) = applyMatrixToPoint m' p
      -- y should be zero (or very close)... how to check?
      result = if (t > 0) then Just (t, n, (0, x, z)) else Nothing
findIntersectionPrimitive kind t ray = error "findIntersectionPrimitive"

findIntersection :: Object -> Ray -> Maybe (Double, Point, Closure, TextureCoords)
findIntersection (Primitive kind t surface) ray = result
   where
      maybeCoords = findIntersectionPrimitive kind t ray
      result = case maybeCoords of Just (t, normal, x) -> Just (t, normal, surface, x)
                                   Nothing -> Nothing
findIntersection (Union o1 o2) ray = firstIntersection (findIntersection o1 ray) (findIntersection o2 ray)
   where
      firstIntersection Nothing x = x
      firstIntersection x Nothing = x
      firstIntersection (Just (t1, n1, s1, x1)) (Just (t2, n2, s2, x2)) = if t1 < t2 then Just (t1, n1, s1, x1) else Just (t2, n2, s2, x2)
findIntersection (Intersection o1 o2) ray = error "intersection not supported"
findIntersection (Difference o1 o2) ray = error "difference not supported"

callSurfaceFunction :: Closure -> TextureCoords -> SurfaceProperties
callSurfaceFunction c (face, u, v) = (color, kd, ks, n)
   where
      Identity [VReal n, VReal ks, VReal kd, VPoint color] =
         execute renderNotAllowed c [VReal v, VReal u, VInt face]
      renderNotAllowed = error "Calling render within a surface function"

(***) :: Point -> Double -> Point
(a, b, c) *** k = (a * k, b * k, c * k)

(///) :: Point -> Double -> Point
(a, b, c) /// k = (a / k, b / k, c / k)

(+++) :: Point -> Point -> Point
(a1, b1, c1) +++ (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)

(-=-) :: Point -> Point -> Point
(a1, b1, c1) -=- (a2, b2, c2) = (a1 - a2, b1 - b2, c1 - c2)

(*#*) :: Point -> Point -> Point
(a1, b1, c1) *#* (a2, b2, c2) = (a1 * a2, b1 * b2, c1 * c2)

dotProduct :: Point -> Point -> Double
dotProduct (a1, b1, c1) (a2, b2, c2) = (a1 * a2 + b1 * b2 + c1 * c2)

reflectedRay :: Ray -> Double -> Point -> Ray
reflectedRay (p, v) t n = ((p +++ (v *** t)), ((n *** (2 * (v `dotProduct` n))) -=- v))

avoidSurfaceAcne :: Ray -> Ray
avoidSurfaceAcne (p, v) = (p +++ (v *** epsilon), v)
   where
      epsilon = 1e-5

unit :: Point -> Point
unit v = v /// (v `dotProduct` v)

lightIntensity :: SurfaceProperties -> Point -> Point -> Point -> Light -> Point
lightIntensity (_, kd, ks, n) normal p v (Directional dir color) = (traceShow color) color *** y
   where
      l = unit (neg dir)
      h = unit (l -=- unit v)
      neg (x, y, z) = (-x, -y, -z)
      dp = abs (normal `dotProduct` h)
      x = (kd * (normal `dotProduct` l)) + (if dp /= 0.0 then ks * (dp ** n) else 0.0)
      y = if x < 0.0 then (error $ "negative light" ++ show x) else x
lightIntensity (_, kd, ks, n) normal p v (Point pos color) = error "Point lights not supported"
lightIntensity (_, kd, ks, n) normal p v (Spot _ _ _ _ _) = error "Spot lights not supported"

illumination :: Int -> Object -> [Light] -> Point -> Ray -> Point
illumination 0 obj lights amb ray = (0, 0, 0)
illumination depth obj lights amb ray@(p, v) =
   case findIntersection obj ray of
      Just (t, normal, surface, coords) -> intensity *#* color
         where
            props@(color, kd, ks, n) = callSurfaceFunction surface coords
            recursive = illumination (depth - 1) obj lights amb reflect
               where
                  reflect = avoidSurfaceAcne (reflectedRay ray t normal)
            -- potential optimization: if ks == 0.0, don't bother computing the recursive component
            intensity = (amb *** kd) +++ (recursive *** ks) +++ (foldl (+++) (0, 0, 0) (map (lightIntensity props normal (p +++ (v *** t)) v) lights))
            --- also need specular and diffuse reflection
      Nothing -> (0, 0, 0)
