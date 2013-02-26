module Types where

import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Code

type Code = [Instruction]
type Stack = [Value]
type Operator = Stack -> Stack

data Instruction = PushConstant Value
                 | PushClosure Code
                 | PushArray Code
                 | Lookup String
                 | Bind String
                 | Invoke String Operator
                 | Apply
                 | If
                 | Render

-- XXX Would like to use deriving Show, but doesn't work for Operator
instance Show Instruction where
   show (PushConstant v) = "PushConstant " ++ show v
   show (PushClosure c) = "PushClosure " ++ show c
   show (PushArray c) = "PushArray " ++ show c
   show (Lookup name) = "Lookup " ++ name
   show (Bind name) = "Bind " ++ name
   show (Invoke name op) = "Invoke " ++ name
   show Apply = "Apply"
   show If = "If"
   show Render = "Render"

-------------------------------------------------------------------------------
-- Closures

type Env = Map.Map String Value
type Closure = (Env, Code)

-------------------------------------------------------------------------------
-- Arrays

type Array = [Value]

-------------------------------------------------------------------------------
-- Points

type Point = (Double, Double, Double)

-------------------------------------------------------------------------------
-- Objects

type Vector = (Double, Double, Double, Double)
type Matrix = (Vector, Vector, Vector, Vector)
type Transform = (Matrix, Matrix) -- forward, inverse

data ObjKind = Sphere | Cube | Cylinder | Cone | Plane
               deriving (Show)

type Surface = Closure

data Object = Primitive ObjKind Transform Surface
            | Union Object Object
            | Intersection Object Object
            | Difference Object Object
              deriving (Show)

-------------------------------------------------------------------------------
-- Lights

type Color = Point
type Pos = Point
type Dir = Point

data Light = Directional Dir Color
           | Point Pos Color
           | Spot Pos Pos Color Double Double
             deriving (Show)

-------------------------------------------------------------------------------
-- Values

data Value = VBool Bool
           | VInt Int
           | VReal Double
           | VString String
           | VClosure Closure
           | VArray Array
           | VPoint Point
           | VObject Object
           | VLight Light
             deriving (Show)
