{-# LANGUAGE DeriveFunctor #-}

module DualMethodsTypes where

import Control.Applicative

---------------------------------------------------------------
--Positions and Directions in 3D Space (i.e. Vector3)

data Triple a = Triple  
                { x :: a
                , y :: a
                , z :: a
                } deriving Functor

instance Applicative Triple where
    pure a = Triple a a a
    Triple f g h <*> Triple a b c = Triple (f a) (g b) (h c)

infixl 6 #+
(#+) :: Num a => Triple a -> Triple a -> Triple a
(#+) = liftA2 (+)

infixl 6 #-
(#-) :: Num a => Triple a -> Triple a -> Triple a
(#-) = liftA2 (-)

infixl 7 #*
(#*) :: Num a => Triple a -> a -> Triple a
(#*) t = flip fmap t . (*) 

infixl 7 #/
(#/) :: Fractional a => Triple a -> a -> Triple a
(#/) t = flip fmap t . (/)

type Position = Triple Double
type Direction = Triple Double

---------------------------------------------------------------
--type synonyms

type Length = Double
type Size = Double
type Depth = Int
type Index = Int

---------------------------------------------------------------
--new types

newtype Surface = Surface (Position -> Length)
newtype VertexPlacer = VertexPlacer (Eituple Length -> Position)
newtype Simplifier = Simplifier (Eituple Position -> Maybe Position)

newtype Vertices = Vertices [Position]
newtype Normals = Normals [Direction]
newtype Colors = Colors [Color]
newtype Indices = Indices [Index]

---------------------------------------------------------------
--data types

data Color = Color 
                { r :: Double
                , g :: Double
                , b :: Double
                , a :: Double
                }

data Mesh = Mesh 
            { vertices :: Vertices
            , normals :: Normals
            --, colors :: Colors
            , indices :: Indices
            }

data Eituple s = Eituple 
                    { i000 :: s
                    , i001 :: s
                    , i010 :: s
                    , i011 :: s
                    , i100 :: s
                    , i101 :: s
                    , i110 :: s
                    , i111 :: s
                    } deriving Functor

data Octree a = Node (Eituple (Octree a))
              | Leaf a
              | Empty

data Axis = X | Y | Z



