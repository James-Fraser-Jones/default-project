module DualMethods where

import DualMethodsTypes

dualMethod :: Position -> Size -> Depth -> Surface -> VertexPlacer -> Simplifier -> Mesh
dualMethod position size maxDepth surface vertexPlacer simplifier =
    let octree = buildOctree position size maxDepth surface vertexPlacer
     in undefined

buildOctree :: Position -> Size -> Depth -> Surface -> VertexPlacer -> Octree Position
buildOctree position size maxDepth surface vertexPlacer =
    if maxDepth > 0
        then
            undefined
        else
            undefined

sampleCorners :: Position -> Size -> Surface -> Eituple Length
sampleCorners position size (Surface surfaceFn) = (surfaceFn . (#+ position) . (#* size)) <$> defaultPositions

defaultPositions :: Eituple Position
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