import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import System.IO

-- MID - Machine Identifier.
-- Name - a human readable identifier.
-- Distance - number of relationships between two MIDs.
type MID = String
type Name = String
type Distance = Int

-- Adjacency list representation.
-- The graph is undirected.
type Graph = Map.Map MID [MID]

type MIDToNameMap = Map.Map MID Name

-- Queue/Stack element: (current MID, distance, path).
data SearchNode = SearchNode MID Distance [MID] deriving (Show, Eq)

-- Result type for shortest path.
data PathResult = PathResult {
    distance :: Distance,
    path :: [MID],
    pathNames :: [Name]
} deriving (Show)

-- Parse TSV file contents (for mid2name.tsv).
-- 1. map applies parseLine to every entry in the list (lines content),
-- 2. lines is a Prelude function that creates a list of lines
-- from a String by separating it with '\n' character.
-- 3. T.strip is used to remove carriage return, because on Windows, lines end
-- with \r\n, and after splitting, they're parsed as '...\r', which causes trouble.
-- 4. parseLine, on its end, splits the line on '\t'. To that end, it must use
-- the Text library.
-- 4. Map.fromList takes a list of tuples in the [(k0, v0), (k1, v1), ...] format
-- and turns it into a Map.
parseMIDToNameFromText :: String -> MIDToNameMap
parseMIDToNameFromText content = Map.fromList $ map parseLine (lines $ (T.unpack . T.strip . T.pack) content)
  where
    parseLine line = case T.splitOn (T.pack "\t") (T.pack line) of
        (mid:name:_) -> (T.unpack mid, T.unpack name)
        _ -> error $ "Invalid line in mid2name.tsv: " ++ line

-- Parse TSV file contents (for freebase.tsv).
-- Basically, same idea as above, except use
-- a custom function instead of Map.fromList.
parseFreebaseFromText :: String -> Graph
parseFreebaseFromText content = buildGraph $ map parseLine (lines $ (T.unpack . T.strip . T.pack) content)
  where
    parseLine line = case T.splitOn (T.pack "\t") (T.pack line) of
        (mid1:_:mid2:_) -> (T.unpack mid1, T.unpack mid2)
        _ -> error $ "Invalid line in freebase.tsv: " ++ line
    buildGraph :: [(MID, MID)] -> Graph
    buildGraph edges = foldr addEdge Map.empty edges
    addEdge :: (MID, MID) -> Graph -> Graph
    addEdge (from, to) graph = 
        let graph' = Map.insertWith (++) from [to] graph
        in Map.insertWith (++) to [from] graph'

bfs_func :: Graph -> [SearchNode] -> Set.Set MID -> MID -> Maybe PathResult
bfs_func _ [] _ _ = Nothing
bfs_func graph (SearchNode currentMID dist currentPath : queue) visited target
    | currentMID == target           = Just $ PathResult dist currentPath []
    | Set.member currentMID visited  = bfs_func graph queue visited target
    | otherwise                      = 
        let visited' = Set.insert currentMID visited
            neighbors = fromMaybe [] (Map.lookup currentMID graph)
            newNodes = [SearchNode neighbor (dist + 1) (currentPath ++ [neighbor]) 
                        | neighbor <- neighbors, not (Set.member neighbor visited')]
        in bfs_func graph (queue ++ newNodes) visited' target

dfs_func :: Graph -> [SearchNode] -> Set.Set MID -> MID -> Maybe PathResult
dfs_func _ [] _ _ = Nothing
dfs_func graph (SearchNode currentMID dist currentPath : queue) visited target
        | currentMID == target            = Just $ PathResult dist (reverse currentPath) []
        | Set.member currentMID visited   = dfs_func graph queue visited target
        | otherwise                       = 
            let visited'   = Set.insert currentMID visited
                neighbors  = fromMaybe [] (Map.lookup currentMID graph)
                newNodes   = [SearchNode neighbor (dist + 1) (neighbor : currentPath) 
                             | neighbor <- neighbors, not (Set.member neighbor visited')]
            in dfs_func graph (newNodes ++ queue) visited' target

-- ! The actual magic starts here.
-- BFS implementation for shortest path.
searchForPath :: Graph -> MID -> MID -> (Graph -> [SearchNode] -> Set.Set MID -> MID -> Maybe PathResult) -> Maybe PathResult
searchForPath graph startMID endMID search_algo
    | startMID == endMID   = Just $ PathResult 0 [startMID] []
    | otherwise            = search_algo graph [SearchNode startMID 0 [startMID]] Set.empty endMID

-- ! This function is called once the path is finished (the MIDs
-- ! are filled) to convert all MIDs to human-readable names.
addNamesToPath :: MIDToNameMap -> PathResult -> PathResult
addNamesToPath midToName result = 
    result { pathNames = map (\mid -> fromMaybe mid (Map.lookup mid midToName)) (path result) }

-- Main function to find the shortest distance.
findPath :: MIDToNameMap -> Graph -> MID -> MID -> (Graph -> [SearchNode] -> Set.Set MID -> MID -> Maybe PathResult) -> IO ()
findPath midToNameMap graph startMID endMID search_algo = do
    case searchForPath graph startMID endMID search_algo of
        Nothing      -> error $ "No connection found between " ++ startMID ++ " and " ++ endMID
        Just result  -> do
            let resultWithNames = addNamesToPath midToNameMap result
            putStrLn $ "Shortest distance: " ++ show (distance resultWithNames)
            putStrLn $ "Full path:"
            mapM_ putStrLn $ zipWith formatPathStep (path resultWithNames) (pathNames resultWithNames)
    where
        formatPathStep mid name = "  " ++ mid ++ " -> " ++ name

-- Utility function to get name from MID.
getNameByMID :: MIDToNameMap -> MID -> String
getNameByMID midToNameMap mid = fromMaybe mid (Map.lookup mid midToNameMap)

-- debugGraph :: Graph -> MIDToNameMap -> MID -> Int -> IO ()
-- debugGraph graph midToNameMap startMID maxDepth = do
--     putStrLn $ "Exploring from: " ++ startMID ++ " (" ++ getNameByMID midToNameMap startMID ++ ")"
--     explore startMID 0 Set.empty
--   where
--     explore currentMID depth visited
--         | depth > maxDepth = return ()
--         | Set.member currentMID visited = return ()
--         | otherwise = do
--             let visited' = Set.insert currentMID visited
--             let neighbors = fromMaybe [] (Map.lookup currentMID graph)
--             let indent = replicate (depth * 2) ' '
--             putStrLn $ indent ++ currentMID ++ " -> " ++ getNameByMID midToNameMap currentMID
--             mapM_ (\neighbor -> explore neighbor (depth + 1) visited') neighbors

main :: IO ()
main = do
    -- ! The output might be weird on MacOS/Linux due to the
    -- ! carriage return characters in tsv files.
    -- mid2name.tsv
    -- Maps MID to Name.
    -- freebase.tsv
    -- Provides a list of relationships between MIDs.
    midToNameContent <- readFile "mid2name.tsv"
    freebaseContent <- readFile "freebase.tsv"
    let midToNameMap = parseMIDToNameFromText midToNameContent
    let graph = parseFreebaseFromText freebaseContent
    findPath midToNameMap graph "/m/0kfv9" "/m/01l1sq" bfs_func  -- The Sopranos to Steven Van Zandt
    findPath midToNameMap graph "/m/0h5k" "/m/07jq_"   dfs_func  -- Anthropology to Egypt