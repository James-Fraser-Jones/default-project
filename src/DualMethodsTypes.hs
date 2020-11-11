{-# LANGUAGE DeriveFunctor #-}

module DualMethodsTypes where

import Data.Ratio

---------------------------------------------------------------
--type synonyms

type Vector3 = Triple Double
type Vector3I = Triple Rational

---------------------------------------------------------------
--new types

newtype Surface = Surface (Vector3 -> Double)
newtype VertexPlacer = VertexPlacer (Eituple Double -> Vector3)
newtype Simplifier = Simplifier (Eituple Vector3 -> Maybe Vector3)

newtype Vertices = Vertices [Vector3]
newtype Normals = Normals [Vector3]
newtype Colors = Colors [Color]
newtype Indices = Indices [Int]

---------------------------------------------------------------
--data types

data Triple a = Triple  
                { x :: a
                , y :: a
                , z :: a
                } deriving Functor

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



