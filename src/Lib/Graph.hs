module Lib.Graph
  ( dijkstra,
    shortestPath,
    bellmanFord,
    floydWarshall,
    Flow (..),
    edmondsKarp,
  )
where

import Algebra.Graph.Label
import Algebra.Graph.Labelled.AdjacencyMap
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as LazyMap
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)), (><))
import Relude hiding (one)
import qualified Text.Show as S

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

data Flow a = Num a => Flow
  { getFlow :: a,
    getCap :: a
  }
instance S.Show a => Show (Flow a) where
  show (Flow f c) = show f <> "/" <> show c
instance (Eq a, Num a) => Eq (Flow a) where
  Flow f1 c1 == Flow f2 c2 = f1 == f2 && c1 == c2
instance Num a => Semigroup (Flow a) where
  Flow f1 c1 <> Flow f2 c2 = Flow (f1 + f2) (c1 + c2)
instance Num a => Monoid (Flow a) where
  mempty = Flow 0 0

edmondsKarp :: forall a e. (Eq a, Ord a, Ord e, Num e) => AdjacencyMap (Capacity e) a -> a -> a -> AdjacencyMap (Flow e) a
edmondsKarp graph source sink = edmondsKarp' source sink initFlowGraph
  where
    initFlowGraph = emap (Flow 0 . readCapacity) graph :: AdjacencyMap (Flow e) a
    readCapacity = fromMaybe (error "Invalid capacity!") . getFinite . getCapacity

    edmondsKarp' :: a -> a -> AdjacencyMap (Flow e) a -> AdjacencyMap (Flow e) a
    edmondsKarp' s t flowGraph = case findAugmentingPath s t flowGraph of
      Nothing -> flowGraph
      Just path -> edmondsKarp' s t (updateFlowGraph path flowGraph)
    
    findAugmentingPath :: a -> a -> AdjacencyMap (Flow e) a -> Maybe [a]
    findAugmentingPath s t flowGraph = bfs (fromList [s]) mempty
      where
        bfs :: Seq a -> Map a a -> Maybe [a]
        bfs Seq.Empty _ = Nothing
        bfs (v :<| vs) parents
          | v == t = Just (getAugmentingPath parents t)
          | otherwise = bfs (vs >< nextVertices) parents'
          where
            nextVertices = fromList . filter filterFn . toList . postSet v $ flowGraph where
              filterFn u = let Flow flow cap = adjacencyMap flowGraph ! v ! u in
                u `LazyMap.notMember` parents && u /= s && flow < cap
            parents' = foldr (\u -> LazyMap.insert u v) parents nextVertices
            
            getAugmentingPath :: Map a a -> a -> [a]
            getAugmentingPath parents v = case parents Map.!? v of
              Nothing -> [s]
              Just parent -> v : getAugmentingPath parents parent

    updateFlowGraph :: [a] -> AdjacencyMap (Flow e) a -> AdjacencyMap (Flow e) a
    updateFlowGraph vs = updateFlowGraph' vs Nothing where
      updateFlowGraph' :: [a] -> Maybe e -> AdjacencyMap (Flow e) a -> AdjacencyMap (Flow e) a
      updateFlowGraph' (u:v:vs) maxFlow flowGraph = updateFlowGraph' (v:vs) (Just maxFlow') flowGraph' where
        edge = adjacencyMap flowGraph ! v ! u
        maxEdgeFlow = getCap edge - getFlow edge
        maxFlow' = case maxFlow of
          Nothing -> maxEdgeFlow
          Just mf -> min maxEdgeFlow mf

        flowGraph' = updateReverseEdge . updateEdge $ flowGraph
        updateEdge = replaceEdge (Flow (getFlow edge + maxFlow') (getCap edge)) v u
        updateReverseEdge = if u `Set.member` postSet v flowGraph
          then replaceEdge (Flow (getFlow edge - maxFlow') (getCap edge)) u v
          else id
      updateFlowGraph' _ _ flowGraph = flowGraph

