module Graph where
import Utils
-- Label for vertex (coordinates of cell)


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
newtype Graph = Graph {vertices :: [Vertex]} deriving (Show)

-- Check whether vertex in list of vertices
vertexInVertices :: Vertex -> [Vertex] -> Bool
vertexInVertices _ [] = False
vertexInVertices vertex@(Vertex {vertexLabel = label'}) (x:y) = 
  (label' == vertexLabel x) || vertexInVertices vertex y

-- Get list of vertices for given labels
takeVertices 
  :: Graph      -- Given graph
  -> [Coords]   -- Given labels (coordinates)
  -> [Vertex] 
takeVertices (Graph []) _ = []
takeVertices (Graph (x:y)) [] = x : y
takeVertices (Graph (x:y)) keys = filter (\ z -> vertexLabel z `elem` keys) (x:y)

-- BFS
bfs 
  :: Graph              -- Graph traverse to
  -> Graph              -- Traverse tree that will be returned
  -> [Vertex]           -- Vertices to visit
  -> [Vertex]           -- Seen vertices
  -> (Coords -> Bool)   -- Dynamic check are the coordinates of vertex impassable
  -> Graph
bfs (Graph []) _ _ _ _ = Graph []
bfs _ outGraph [] _ _ = outGraph
bfs (Graph (a:b)) (Graph (c:d)) (e:f) (g:h) isObstacle' 
  = bfs inGraph outGraph queue seen' isObstacle'
  where 
    inGraph = Graph (a:b)
    eLabel = vertexLabel e
    eNeighbors = vertexNeighbors e
    eVertexNeighbors = takeVertices inGraph eNeighbors
    dist = vertexDistance e + 1
    seen = g : h
    filteredNeighbors = filterVertexNeighbors seen eVertexNeighbors isObstacle'
    enqueue = updateDistPred filteredNeighbors dist eLabel
    outGraph = Graph $ (c:d) ++ enqueue
    queue = f ++ enqueue
    seen' = seen ++ enqueue
bfs g _ _ _ _ = g

-- Omit improper vertices (and obstacles)
filterVertexNeighbors 
  :: [Vertex]           -- Full list of vertices
  -> [Vertex]           -- List of vertices to find neighbors for
  -> (Coords -> Bool)   -- Dynamic check are the coordinates of vertex impassable
  -> [Vertex]
filterVertexNeighbors _ [] _ = []
filterVertexNeighbors [] _ _ = []
filterVertexNeighbors s vn isObstacle' = 
  filter (\ x -> not (vertexInVertices x s) && not (isObstacle x) && not (isObstacle' (vertexLabel x))) vn

-- Reset graph for BFS (set distance 0, set noPredecessor)
resetGraph 
  :: Graph  -- Given graph
  -> Graph
resetGraph (Graph vs) =
  Graph $ map (\ (Vertex lbl ngb _ _ obs) -> Vertex lbl ngb 0 noPredecessor obs) vs

-- Change predecessors and distance
updateDistPred 
  :: [Vertex]     -- Vertices to update predecessor and distance
  -> Int          -- Distance to set
  -> Coords       -- Predecessor label
  -> [Vertex]
updateDistPred [] _ _ = []
updateDistPred (x:y) dist predLabel = map (\ (Vertex label n _ _ o) -> Vertex label n dist predLabel o) (x:y)

-- Get vertex from graph
getVertex 
  :: Graph          -- Given graph
  -> Coords         -- Label (coords) of vertex
  -> Maybe Vertex
getVertex (Graph []) _ = Nothing
getVertex (Graph (x : xs)) c = 
  if vertexLabel x == c then Just x
  else getVertex (Graph xs) c

-- Extract vertices from graph
getVertices 
  :: Graph      -- Graph to extract vertices
  -> [Vertex]
getVertices (Graph v) = v

-- Method to get path from first coords to second
findPath 
  :: Graph            -- Graph to find path on int
  -> Coords           -- Vertex start on
  -> Coords           -- Vertex finish on
  -> (Coords -> Bool) -- Dynamic check are the coordinates of vertex impassable
  -> Maybe [Coords]
findPath g f t isObstacle' = 
  if null (takeVertices tree [t])
    then Nothing
    else Just (reverse (buildPath t (vertices tree)))
  where
    queue = takeVertices cleanGraph [f]
    outGraph = Graph queue
    cleanGraph = resetGraph g
    tree = bfs cleanGraph outGraph queue queue isObstacle'

    find :: Coords -> [Vertex] -> Maybe Vertex
    find _ [] = Nothing
    find x (y : ys) = if x == vertexLabel y then Just y else find x ys
    
    buildPath t' p = case find t' p of
      Nothing -> []
      Just v -> vertexLabel v : buildPath (vertexPredecessor v) p

-- Method to filter list of coords s.t. (0 <= x < w, 0 <= y < h)
filterNeighbours 
  :: Int          -- Width of field
  -> Int          -- Height of field
  -> [Coords]     -- Not filtered neighbours
  -> [Coords]
filterNeighbours w h = filter (\(x, y) -> (0 <= x && x < w) && (0 <= y && y < h))

-- Generate neighbours 
generateNeighbours 
  :: Coords     -- Label (coordinates) of vertex to find neighbours for
  -> [Coords]
generateNeighbours (x, y) = if odd y 
  then [
    (x - 1, y), (x - 1, y - 1), (x, y - 1), 
    (x + 1, y), (x, y + 1), (x - 1, y + 1)]
  else
    [(x, y - 1), (x + 1, y - 1), (x + 1, y),
      (x + 1, y + 1), (x, y + 1), (x - 1, y)]

-- Generate vertex on given field with WIDTH and HEIGHT
genVertex 
  :: Int        -- Width of field
  -> Int        -- Height of field
  -> Coords     -- Label (coordinates) of vertex
  -> Vertex
genVertex width height coords = 
  Vertex 
    coords 
    (filterNeighbours width height (generateNeighbours coords)) 
    0 
    noPredecessor 
    False

-- Method to generate field with given X and Y sizes (w/o obstacles)
generateGraph 
  :: Int      -- Width of field
  -> Int      -- Height of field
  -> Graph    
generateGraph width height = Graph gVertices
  where
    points = concatMap (\ x -> zip (repeat x) (take height [0 .. ])) (take width [0 .. ])
    gVertices = map (genVertex width height) points

-- Method to find distances from vertex with given label for reachable ones
findDistances
  :: Graph              -- Graph to find distances in it
  -> Coords             -- Label of vertex to start from
  -> (Coords -> Bool)   -- Dynamic check are the coordinates of vertex impassable
  -> [(Coords, Int)] 
findDistances graph fromCoords isObstacle' = distances
    where
      queue = takeVertices cleanGraph [fromCoords]
      outGraph = Graph queue
      cleanGraph = resetGraph graph
      tree = bfs cleanGraph outGraph queue queue isObstacle'
      distances = map (\v -> (vertexLabel v, vertexDistance v)) (vertices tree)