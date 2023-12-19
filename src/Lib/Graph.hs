module Lib.Graph
  ( dijkstra,
    shortestPath,
    bellmanFord,
    floydWarshall,
  )
where

import Algebra.Graph.Label
import Algebra.Graph.Labelled.AdjacencyMap
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as LazyMap
import Data.Set qualified as Set
import Relude hiding (one)

-- If the edge type is 'Distance' 'Int':
-- @
-- '<+>'  == 'min'
-- '<.>'  == '+'
-- 'one'  == 0
-- 'zero' == 'distance' 'infinity'
dijkstra :: forall a e. (Ord a, Ord e, Dioid e) => AdjacencyMap e a -> a -> Map a e
dijkstra graph src = dijkstra' (Set.singleton (one, src)) mempty
  where
    adjMap = adjacencyMap graph
    o = one :: e
    z = zero :: e
    pop
      | o <+> z == o =
        if o < z
          then Set.minView
          else Set.maxView
      | o <+> z == z =
        if o < z
          then Set.maxView
          else Set.minView
      | otherwise = Set.minView
    dijkstra' :: Set (e, a) -> Map a e -> Map a e
    dijkstra' queue visited = case pop queue of
      Nothing -> visited
      Just ((e, v), queue') ->
        if v `LazyMap.member` visited
          then dijkstra' queue' visited
          else dijkstra' queue'' visited'
        where
          visited' = LazyMap.insert v e visited
          queue'' = foldr relax queue' (Map.toList $ adjMap ! v)
          relax (v', e') = Set.insert (e <.> e', v')

-- Returns shortest distance and path (e, [a]) for all reachable nodes
shortestPath :: forall a e. (Ord a, Ord e, Dioid e) => AdjacencyMap e a -> a -> Map a (e, [a])
shortestPath graph src = dijkstra' (Set.singleton (one, [src])) mempty
  where
    adjMap = adjacencyMap graph
    o = one :: e
    z = zero :: e
    pop
      | o <+> z == o =
        if o < z
          then Set.minView
          else Set.maxView
      | o <+> z == z =
        if o < z
          then Set.maxView
          else Set.minView
      | otherwise = Set.minView
    dijkstra' :: Set (e, [a]) -> Map a (e, [a]) -> Map a (e, [a])
    dijkstra' queue visited = case pop queue of
      Nothing -> visited
      Just ((e, v:path), queue') ->
        if v `LazyMap.member` visited
          then dijkstra' queue' visited
          else dijkstra' queue'' visited'
        where
          visited' = LazyMap.insert v (e, reverse $ v:path) visited -- 'reverse' isn't good for performance..
          queue'' = foldr relax queue' (Map.toList $ adjMap ! v)
          relax (v', e') = Set.insert (e <.> e', v':v:path)

bellmanFord :: (Ord a, Dioid e) => AdjacencyMap e a -> a -> Map a e
bellmanFord graph src = foldl' relax distances relaxEdgeList 
  where
    adjMap = adjacencyMap graph
    distances = Map.mapWithKey (\v _ -> if v == src then one else zero) adjMap
    relaxEdgeList = join $ replicate (length adjMap - 1) (edgeList graph)
    relax dists (w, u, v) = Map.insert v ((dists ! v) <+> (dists ! u <.> w)) dists

floydWarshall :: (Ord a, Dioid e) => AdjacencyMap e a -> Map a (Map a e)
floydWarshall graph = relax0 adjMap'
  where
    adjMap = adjacencyMap graph
    adjMap' = Map.mapWithKey (Map.adjust (const one)) adjMap
    vertices = vertexList graph
    relax0 m = foldl' relax1 m vertices
    relax1 m k = foldl' (relax2 k) m vertices
    relax2 k m i = foldl' (relax3 k i) m vertices
    relax3 k i m j =
      let n = (m ! i ! j) <+> ((m ! i ! k) <.> (m ! k ! j))
       in Map.adjust (Map.insert j n) i m
