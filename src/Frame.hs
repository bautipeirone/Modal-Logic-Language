module Frame 
        ( Graph (..)
        , neighbours
        , isReflexive
        , isSymmetric
        , isSerial
        , isFunctional
        , isLinear
        , isTransitive
        , isEuclidean
        ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

{-
-- Internal representation of a graph
-- Designed to work as an adjacency matrix without the need of an
-- explicit array in order to avoid the need of indexable types.
-}
data Graph v = Graph
      { vertices :: [v]
      , edges    :: M.Map v [v]
      } deriving Show

-- Properties over graphs are thought as predicates
-- over these types. If the predicate is valid on a
-- graph the property holds.
type GraphProperty v = Graph v -> Bool

{-
Funcion sobre listas extra
-}
cartesian :: [a] -> [b] -> [(a,b)]
cartesian xs ys = do  x <- xs
                      y <- ys
                      return (x,y)

neighbours :: Ord v => Graph v -> v -> [v]
neighbours g v = fromMaybe [] $ M.lookup v (edges g)

existsEdge :: Ord v => Graph v -> (v, v) -> Bool
existsEdge g (x,y) = elem y $ neighbours g x

isReflexive :: Ord v => GraphProperty v
isReflexive g = all hasLoop (vertices g)
          where
            hasLoop v = existsEdge g (v,v)

isSymmetric :: Ord v => GraphProperty v
isSymmetric g = checkSymmetry (vertices g)
    where
      checkSymmetry [] = True
      checkSymmetry (v:vs) = all (isSymmetricPair v) vs && checkSymmetry vs
      isSymmetricPair x y = existsEdge g (x,y) == existsEdge g (y,x)

isSerial :: Ord v => GraphProperty v
isSerial g = (not . any (null . neighbours g)) (vertices g)

isTransitive :: Ord v => GraphProperty v
isTransitive g = all vertexIsTransitive (vertices g)
      where
        vertexIsTransitive x = all (f x) (neighbours g x)
        f x y = all (\z -> existsEdge g (x,z)) (neighbours g y)

isEuclidean :: Ord v => GraphProperty v
isEuclidean g = all vertexIsEuclidean (vertices g)
      where
        vertexIsEuclidean x =
          let n = neighbours g x
              c = cartesian n n
          in all (existsEdge g) c

isFunctional :: Ord v => GraphProperty v
isFunctional g = all ((==) 1 . length . neighbours g) (vertices g)

isLinear :: Ord v => GraphProperty v
isLinear g = all vertexIsLinear (vertices g)
      where
        vertexIsLinear x =
          let n = neighbours g x
              c = cartesian n n
          in all (\(y,z) -> y == z || existsEdge g (y,z) || existsEdge g (z,y)) c

-- newtype Frame v = Frame (Graph v)
-- type Logic v = [GraphProperty v]
