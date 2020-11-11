module DualMethods where

import DualMethodsTypes
import Control.Applicative
import Data.Ratio

---------------------------------------------------------------
--utility functions

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
(#/) t = flip fmap t . (flip (/))

---------------------------------------------------------------
--heavy lifters

dualMethod :: Vector3 -> Double -> Int -> Surface -> VertexPlacer -> Simplifier -> Mesh
dualMethod origin size maxDepth surface vertexPlacer simplifier =
    let octree = buildOctree origin size maxDepth surface vertexPlacer 0 (Triple 0 0 0)
     in undefined

buildOctree :: Vector3 -> Double -> Int -> Surface -> VertexPlacer -> Int -> Vector3I -> Octree Vector3
buildOctree origin size maxDepth surface vertexPlacer@(VertexPlacer placerFn) curDepth curPosition =
    if maxDepth > 0
        then
            Node $ Eituple 
                (buildOctree origin size maxDepth surface vertexPlacer (curDepth + 1) (curPosition #* 2 #+ (Triple 1 1 1)))
                (buildOctree origin size maxDepth surface vertexPlacer (curDepth + 1) (curPosition #* 2 #+ (Triple 1 1 1)))
                (buildOctree origin size maxDepth surface vertexPlacer (curDepth + 1) (curPosition #* 2 #+ (Triple 1 1 1)))
                (buildOctree origin size maxDepth surface vertexPlacer (curDepth + 1) (curPosition #* 2 #+ (Triple 1 1 1)))
                (buildOctree origin size maxDepth surface vertexPlacer (curDepth + 1) (curPosition #* 2 #+ (Triple 1 1 1)))
                (buildOctree origin size maxDepth surface vertexPlacer (curDepth + 1) (curPosition #* 2 #+ (Triple 1 1 1)))
                (buildOctree origin size maxDepth surface vertexPlacer (curDepth + 1) (curPosition #* 2 #+ (Triple 1 1 1)))
                (buildOctree origin size maxDepth surface vertexPlacer (curDepth + 1) (curPosition #* 2 #+ (Triple 1 1 1)))
        else
            let relativePosition = (fromRational <$> curPosition) #/ fromIntegral (2 ^ curDepth)
                position = origin #+ relativePosition #* (size/2)
                corners = sampleCorners position size surface
             in Leaf $ placerFn corners

sampleCorners :: Vector3 -> Double -> Surface -> Eituple Double
sampleCorners position size (Surface surfaceFn) = (surfaceFn . (#+ position) . (#* (size/2))) <$> defaultPositions

---------------------------------------------------------------
--constants

defaultPositions :: Eituple Vector3
defaultPositions = 
    let o = -1 in
        Eituple
            (Triple o o o)
            (Triple 1 o o)
            (Triple o 1 o)
            (Triple 1 1 o)
            (Triple o o 1)
            (Triple 1 o 1)
            (Triple o 1 1)
            (Triple 1 1 1)

--write a function which can figure out the position of a cube x layers deep into an octree
--in an analytic way rather than relying on double precision and incrementally adding or removing from the center point each time