module Operators (operator) where

import Prelude hiding (sin, cos, asin, acos, sqrt, floor, length)
import qualified Prelude
import Types
import qualified Transformations as Trans
import Utils

badArgs :: String -> a
badArgs name = error $ "Invalid arguments for " ++ name

stackfn_i :: (Int -> Int) -> String -> Operator
stackfn_i f name ((VInt i):xs) = ((VInt (f i)):xs)
stackfn_i f name _ = badArgs name

stackfn_r :: (Double -> Double) -> String -> Operator
stackfn_r f name ((VReal r):xs) = ((VReal (f r)):xs)
stackfn_r f name _ = badArgs name

stackfn_ii :: (Int -> Int -> Int) -> String -> Operator
stackfn_ii f name ((VInt i2):(VInt i1):xs) = ((VInt (f i1 i2)):xs)
stackfn_ii f name _ = badArgs name

stackfn_rr :: (Double -> Double -> Double) -> String -> Operator
stackfn_rr f name ((VReal r2):(VReal r1):xs) = ((VReal (f r1 r2)):xs)
stackfn_rr f name _ = badArgs name

stackfn_oo :: (Object -> Object -> Object) -> String -> Operator
stackfn_oo f name ((VObject o2):(VObject o1):xs) = ((VObject (f o1 o2)):xs)
stackfn_oo f name _ = badArgs name


-- Arithmetic operators

addi = stackfn_ii (+) "addi"
addf = stackfn_rr (+) "addf"

subi = stackfn_ii (-) "subi"
subf = stackfn_rr (-) "subf"

muli = stackfn_ii (*) "muli"
mulf = stackfn_rr (*) "mulf"

divi = stackfn_ii quot "divi"
divf = stackfn_rr (/) "divf"

modi = stackfn_ii rem "modi"

negi = stackfn_i negate "negi"
negf = stackfn_r negate "negf"

-- XXX double-check div/mod implementation


-- Trigonometric and other mathematical operators

sin = stackfn_r (Prelude.sin . degToRad) "sin"
asin = stackfn_r (radToDeg . Prelude.asin) "asin"
cos = stackfn_r (Prelude.cos . degToRad) "cos"
acos = stackfn_r (radToDeg . Prelude.acos) "acos"

sqrt = stackfn_r Prelude.sqrt "sqrt"
frac = stackfn_r (snd . properFraction) "frac"
-- XXX double-check

clampf = stackfn_r clamp "clamp"


-- Conversion operators

floor :: Operator
floor ((VReal n1):xs) = ((VInt (Prelude.floor n1)):xs)
-- XXX check definition of floor

real :: Operator
real ((VInt n1):xs) = ((VReal (fromIntegral n1)):xs)


-- Relational operators

eqi, eqf :: Operator
eqi ((VInt n2):(VInt n1):xs) = ((VBool (n1 == n2)):xs)
eqf ((VReal n2):(VReal n1):xs) = ((VBool (n1 == n2)):xs)

lessi, lessf :: Operator
lessi ((VInt n2):(VInt n1):xs) = ((VBool (n1 < n2)):xs)
lessf ((VReal n2):(VReal n1):xs) = ((VBool (n1 < n2)):xs)


-- Operators for points

getx, gety, getz :: Operator
getx ((VPoint (x, y, z)):xs) = ((VReal x):xs)
gety ((VPoint (x, y, z)):xs) = ((VReal y):xs)
getz ((VPoint (x, y, z)):xs) = ((VReal z):xs)

point :: Operator
point ((VReal z):(VReal y):(VReal x):xs) = ((VPoint (x, y, z)):xs)


-- Operators for arrays

get :: Operator
get ((VInt i):(VArray a):xs) = ((a !! i):xs)

length :: Operator
length ((VArray a):xs) = ((VInt (Prelude.length a)):xs)


-- Operators for primitive objects

stackfn_o :: ObjKind -> Operator
stackfn_o k ((VClosure c):xs) = ((VObject (Primitive k Trans.identity_t c)):xs)

sphere = stackfn_o Sphere
cube = stackfn_o Cube
cylinder = stackfn_o Cylinder
cone = stackfn_o Cone
plane = stackfn_o Plane


-- Operators for object transformations 

stackfn_pt :: (Point -> Transform) -> String -> Operator
stackfn_pt f name ((VReal z):(VReal y):(VReal x):(VObject o):xs) = ((VObject (Trans.applyTransform (f (x, y, z)) o)):xs)
stackfn_pt f name _ = badArgs name

stackfn_dt :: (Double -> Transform) -> String -> Operator
stackfn_dt f name ((VReal r):(VObject o):xs) = ((VObject (Trans.applyTransform (f r) o)):xs)
stackfn_dt f name _ = badArgs name

translate = stackfn_pt Trans.trans_t "translate"
scale = stackfn_pt Trans.scale_t "scale"

uscale = stackfn_dt Trans.uscale_t "uscale"
rotatex = stackfn_dt (Trans.rotatex_t . degToRad) "rotatex"
rotatey = stackfn_dt (Trans.rotatey_t . degToRad) "rotatey"
rotatez = stackfn_dt (Trans.rotatez_t . degToRad) "rotatez"


-- Operators for compound objects

union = stackfn_oo Union "union"
intersect = stackfn_oo Intersection "intersection"
difference = stackfn_oo Difference "difference"


-- Operators for lights

light :: Operator
light ((VPoint color):(VPoint dir):xs) = ((VLight (Directional dir color)):xs)

pointlight :: Operator
pointlight ((VPoint color):(VPoint pos):xs) = ((VLight (Point pos color)):xs)

spotlight :: Operator
spotlight ((VReal exp):(VReal cutoff):(VPoint color):(VPoint at):(VPoint pos):xs) = ((VLight (Spot pos at color cutoff exp)):xs)


ops = [
   ("acos", acos),
   ("addi", addi),
   ("addf", addf),
   ("asin", asin),
   ("clampf", clampf),
   ("cone", cone),
   ("cos", cos),
   ("cube", cube),
   ("cylinder", cylinder),
   ("difference", difference),
   ("divi", divi),
   ("divf", divf),
   ("eqi", eqi),
   ("eqf", eqf),
   ("floor", floor),
   ("frac", frac),
   ("get", get),
   ("getx", getx),
   ("gety", gety),
   ("getz", getz),
   ("intersect", intersect),
   ("length", length),
   ("lessi", lessi),
   ("lessf", lessf),
   ("light", light),
   ("modi", modi),
   ("muli", muli),
   ("mulf", mulf),
   ("negi", negi),
   ("negf", negf),
   ("plane", plane),
   ("point", point),
   ("pointlight", pointlight),
   ("real", real),
   ("rotatex", rotatex),
   ("rotatey", rotatey),
   ("rotatez", rotatez),
   ("scale", scale),
   ("sin", sin),
   ("sphere", sphere),
   ("spotlight", spotlight),
   ("sqrt", sqrt),
   ("subi", subi),
   ("subf", subf),
   ("translate", translate),
   ("union", union),
   ("uscale", uscale)]

operator :: String -> Operator
operator name = maybe (error $ "Unknown operator: " ++ name) id (lookup name ops)
