module Graph where

-- Label for vertex (coordinates of cell)
type Coords = (Int, Int)

-- Constant definition for non-existing predecessor in BFS
noPredecessor :: Coords
noPredecessor = (-1, -1)

-- Vertex description
data Vertex = Vertex {
                          vertexLabel :: Coords
                        , vertexNeighbors :: [Coords]
                        , vertexDistance :: Int
                        , vertexPredecessor :: Coords
                        , isObstacle :: Bool
                      } deriving (Show)

-- Graph description
newtype Graph = Graph [Vertex] deriving (Show)

-- Check whether vertex in list of vertices
vertexInVertices :: Vertex -> [Vertex] -> Bool
vertexInVertices _ [] = False
vertexInVertices vertex@(Vertex {vertexLabel = label'}) (x:y) = 
  (label' == vertexLabel x) || vertexInVertices vertex y

-- Get list of vertices for given labels
graphVertices :: Graph -> [Coords]-> [Vertex]
graphVertices (Graph []) _ = []
graphVertices (Graph (x:y)) [] = x : y
graphVertices (Graph (x:y)) keys = filter (\ z -> vertexLabel z `elem` keys) (x:y)

-- BFS
bfs :: Graph -> Graph -> [Vertex] -> [Vertex] -> Graph
bfs (Graph []) _ _ _ = Graph []
bfs _ outGraph [] _ = outGraph
bfs (Graph (a:b)) (Graph (c:d)) (e:f) (g:h) = bfs inGraph outGraph queue seen'
  where 
    inGraph = Graph (a:b)
    eLabel = vertexLabel e
    eNeighbors = vertexNeighbors e
    eVertexNeighbors = graphVertices inGraph eNeighbors
    dist = vertexDistance e + 1
    seen = g : h
    filteredNeighbors = filterVertexNeighbors seen eVertexNeighbors
    enqueue = updateDistPred filteredNeighbors dist eLabel
    outGraph = Graph $ (c:d) ++ enqueue
    queue = f ++ enqueue
    seen' = seen ++ enqueue

-- Omit improper vertices (and obstacles)
filterVertexNeighbors :: [Vertex] -> [Vertex] -> [Vertex]
filterVertexNeighbors _ [] = []
filterVertexNeighbors [] _ = []
filterVertexNeighbors s vn = filter (\ x -> not (vertexInVertices x s) && not (isObstacle x)) vn

-- Reset graph for BFS
resetGraph :: Graph -> Graph
resetGraph (Graph vertices) =
  Graph $ map (\ (Vertex lbl ngb _ _ obs) -> Vertex lbl ngb 0 noPredecessor obs) vertices

-- Change predecessors and distance
updateDistPred :: [Vertex] -> Int -> Coords -> [Vertex]
updateDistPred [] _ _ = []
updateDistPred (x:y) dist predLabel = map (\ (Vertex label n _ _ o) -> Vertex label n dist predLabel o) (x:y)

-- Get vertex from graph
getVertex :: Graph -> Coords -> Maybe Vertex
getVertex (Graph []) _ = Nothing
getVertex (Graph (x : xs)) c = 
  if vertexLabel x == c then Just x
  else getVertex (Graph xs) c

-- Extract vertices from graph
getVertices :: Graph -> [Vertex]
getVertices (Graph v) = v

-- Method to get path from first coords to second
findPath :: Graph -> Coords -> Coords -> Maybe [Coords]
findPath g f t = 
  if null (graphVertices tree [t])
    then Nothing
    else Just (f : buildPath t (graphVertices tree [t]))
  where
    queue = graphVertices cleanGraph [f]
    outGraph = Graph queue
    cleanGraph = resetGraph g
    tree = bfs cleanGraph outGraph queue queue

    find :: Coords -> [Vertex] -> Maybe Vertex
    find x [] = Nothing
    find x (y : ys) = if x == vertexLabel y then Just y else find x ys
    
    buildPath t p = case find t p of
      Nothing -> []
      Just v -> vertexLabel v : buildPath (vertexPredecessor v) p
