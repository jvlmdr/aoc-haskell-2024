import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

main :: IO ()
main = do
    input <- getLine
    let files = makeFileList $ map (toInteger . digitToInt) input
    -- print files
    let tree = makeTree $ makeSpaceList files
    -- putStrLn $ showGraph tree ++ "\n"
    -- putStrLn $ List.intercalate "\n\n" $
    --     zipWith (\f (f', t) -> show f ++ " -> " ++ show f' ++ "\n" ++ showGraph t)
    --         (reverse files) (compactScan (reverse files) tree)
    let (files', tree') = compact (reverse files) (makeTree $ makeSpaceList files)
    -- print $ List.sort files'
    -- putStrLn $ showGraph tree' ++ "\n"
    print $ checksum files'

type Pos = Integer
type Size = Integer
type Id = Integer
type Space = (Pos, Size)
type File = (Pos, Size, Id)

makeFileList :: [Size] -> [File]
makeFileList = go 0 0 where
    go :: Pos -> Id -> [Size] -> [File]
    go _ _ [] = []
    go pos id [n] = [(pos, n, id)]
    go pos id (n1 : n2 : xs) = (pos, n1, id) : go (pos + n1 + n2) (id + 1) xs

makeSpaceList :: [File] -> [Space]
makeSpaceList fs = catMaybes $ zipWith diff fs (tail fs) where
    diff :: File -> File -> Maybe Space
    diff (p1, s1, _) (p2, _, _)
        | p1 + s1 < p2 = Just (p1 + s1, p2 - p1 - s1)
        | otherwise = Nothing

-- Like Maybe but with a maximum value that implements Ord.
data WithTop a = Finite a | Top deriving (Eq, Show)

instance Ord a => Ord (WithTop a) where
    compare Top Top = EQ
    compare Top _ = GT
    compare _ Top = LT
    compare (Finite x) (Finite y) = compare x y

-- Tree structure that contains a list of (pos, size) intervals.
-- At each level of the tree, we have a list of intervals that is
-- increasing in size and position.
-- Below each interval is a subtree containing intervals that are
-- smaller or equal in size and occur later in the list
-- (but not after the next interval at the top level).
-- The critical abilities are to:
-- 1) insert a new interval
-- 2) pops the earliest occuring interval larger-or-equal than a size
--
-- For example, consider the intervals (input "3225434133402")
--   pos:   3,  7, 16, 21, 25
--   size:  2,  5,  1,  1,  3
-- This will yield the tree structure, using (size,pos):
--   + (2,3)
--   + (5,7)
--     + (1,16)
--     | + (1,21)
--     + (3,25)
-- When we come to pops an element from this structure,
-- we will either take (2,3) or (5,7).
-- These would be taken before any of the other elements.
--
-- It then remains to merge the subtree into the top-level table.
-- They may either go at the top level or below a preceding node.
-- For example, if we pops the size-5 element above, we obtain:
--   + (2,3)
--   | + (1,16)
--   |   + (1,21)
--   + (3,25)
-- We may need to recurse down the preceding node,
-- but we shouldn't need to recurse down the subtree.
--
-- This tree can be visualized as a sequence of intervals:
--    .   +====================··
--    .   ‖        ·        ·
--    .   ‖        ·        +==··
--    +===‖        ·        ‖
--    ‖   ‖        +====+===‖
-- 012345678901234567890123456789
--
-- We call it a StepTree because each level contains the
-- step increases in interval size.
newtype StepTree = StepTree { nodes :: Map Size (Pos, StepTree) }

-- One space dominates another if it occurs earlier and is larger-or-equal in size.
dominates :: Space -> Space -> Bool
dominates (pos, size) (p, s) = pos < p && size >= s

-- Returns the (space, subtree) pairs of the steps at this level.
assocs :: StepTree -> [(Space, StepTree)]
assocs = map (\(s, (p, t)) -> ((p, s), t)) . Map.assocs . nodes

fromAssocs :: [(Space, StepTree)] -> StepTree
fromAssocs = StepTree . Map.fromList . map (\((p, s), t) -> (s, (p, t)))

empty :: StepTree
empty = StepTree Map.empty

singleton :: Space -> StepTree
singleton x = fromAssocs [(x, empty)]

-- Assumes the input is strict monotonic increasing in position.
makeTree :: [Space] -> StepTree
makeTree = fromAssocs . fst . popNodesUnder Top where
    -- Pops nodes below a given size from a list of spaces.
    -- Terminates once max size is observed and returns remaining elements.
    -- Recursively creates nodes below nodes.
    popNodesUnder :: WithTop Size -> [Space] -> ([(Space, StepTree)], [Space])
    popNodesUnder _ [] = ([], [])
    popNodesUnder size (x@(_, s) : xs)
        -- The next element exceeds the bound. Stop.
        | Finite s > size = ([], x : xs)
        -- First read child nodes below size s, recursively.
        -- Then continue reading siblings from remainder.
        | otherwise = ((x, fromAssocs children) : siblings, rest') where
            (children, rest) = popNodesUnder (Finite s) xs
            (siblings, rest') = popNodesUnder size rest

headPos :: [(Space, StepTree)] -> WithTop Pos
headPos [] = Top
headPos (((pos, _), _) : _) = Finite pos

-- Splits a tree at the given position,
-- e.g. for the introduction of a new node.
-- To the left (before pos) remains a valid tree.
-- To the right (after pos) is a collection of valid subtrees.
-- Note that this splits the entire tree given,
-- however the caller could call it on a subtree.
split :: Pos -> StepTree -> (StepTree, [(Space, StepTree)])
split pos tree = (fromAssocs a1, a2) where
    (a1, a2) = go $ assocs tree
    go :: [(Space, StepTree)] -> ([(Space, StepTree)], [(Space, StepTree)])
    go [] = ([], [])
    go ((kA@(posA, _), treeA) : xs)
        -- The whole interval is to the left of pos.
        | posB <= Finite pos = ((kA, treeA) : left, right)
        -- The whole interval is to the right of pos.
        | pos <= posA = (left, (kA, treeA) : right)
        -- We have posA < pos < posB.
        -- Split the interval's subtree.
        -- The left subtree replaces the old subtree under (posA, sizeA).
        -- The right subtrees are returned directly.
        | otherwise = let (left', right') = split pos treeA in
            ((kA, left') : left, right' ++ right)
        where
            posB = headPos xs
            (left, right) = go xs

-- INTERNAL USE ONLY!
-- Inserts a whole subtree at the correct location.
-- Its topology must be consistent with the rest of the tree.
-- That is, no need to split or move any nodes.
-- TODO: Efficient version that reinserts all elements in an ordered list in one pass?
reinsert :: (Space, StepTree) -> StepTree -> StepTree
reinsert x@((pos, size), subtree) = fromAssocs . go . assocs where
    go :: [(Space, StepTree)] -> [(Space, StepTree)]
    go [] = [x]  -- Reinsert into an empty tree.
    go ((kA@(posA, sizeA), treeA) : xs)
        -- The whole interval is to the left (before) pos.
        -- Leave this node untouched and continue.
        | posB <= Finite pos = (kA, treeA) : go xs
        -- The whole interval and those after it are to the right (after) pos.
        -- Insert the node before the remaining elements.
        | pos < posA = x : (kA, treeA) : xs
        -- The position occurs within the interval; posA <= pos < posB.
        | otherwise =
            -- If its size is less than that of the interval, add below the node.
            if sizeA >= size then (kA, reinsert x treeA) : xs
            -- Otherwise, the node and its subtree go here.
            -- Insert directly, no recursion required.
            else (kA, treeA) : x : xs
        where
            posB = headPos xs

insert :: Space -> StepTree -> StepTree
insert x@(pos, size) = fromAssocs . go . assocs where
    go :: [(Space, StepTree)] -> [(Space, StepTree)]
    go [] = [(x, empty)]
    go ((kA@(posA, sizeA), treeA) : xs)
        -- The whole interval is to the left (before) pos.
        -- Leave this node untouched and continue.
        | posB <= Finite pos = (kA, treeA) : go xs
        -- The whole interval and those after it are to the right (after) pos.
        -- Add a new node at the start.
        -- Remaining nodes go below it or after it.
        -- TODO: Duplication here. Can this be re-used for the otherwise case?
        | pos < posA = let
            (xsBelow, xsAfter) = break (\((_, s), _) -> s > size) ((kA, treeA) : xs)
            in (x, fromAssocs xsBelow) : xsAfter
        -- The position occurs within the interval; posA <= pos < posB.
        | otherwise =
            -- If its size is less than that of the interval, recurse into this node.
            if sizeA >= size then (kA, insert x treeA) : xs
            -- Otherwise, insert a new node here.
            -- Split the current interval.
            -- The remaining nodes either go below or after.
            -- Create a tree with a single new node and reinsert
            -- subtrees from split and from rest of list into it.
            else let
                -- (treeA', restA) = split pos treeA
                -- treeRest = foldr reinsert (singleton x) (restA ++ xs)
                -- in (kA, treeA') : assocs treeRest
                (treeA', restA) = split pos treeA
                (xsBelow, xsAfter) = break (\((_, s), _) -> s > size) xs
                treeX = foldr reinsert (fromAssocs xsBelow) restA
                in (kA, treeA') : (x, treeX) : xsAfter
        where
            posB = headPos xs

popGE :: Size -> StepTree -> (Maybe Space, StepTree)
popGE minSize tree =
    -- TODO: Minor gain using Map. Replace completely with List?
    case Map.lookupGE minSize (nodes tree) of
        Nothing -> (Nothing, tree)
        Just (size, (pos, subtree)) -> (Just (pos, size), tree'') where
            -- Delete the node at this level; reinsert its subtrees.
            tree' = StepTree $ Map.delete size (nodes tree)
            tree'' = foldr reinsert tree' $ assocs subtree

-- Returns the resultant list of files and space tree.
compact :: [File] -> StepTree -> ([File], StepTree)
compact [] t = ([], t)
compact (f : fs) tree = (f' : fs', treeFinal) where
    (f', treeNext) = moveForward f tree
    (fs', treeFinal) = compact fs treeNext

moveForward :: File -> StepTree -> (File, StepTree)
moveForward (f_pos, f_size, f_id) t =
    -- Look for a space to put this file.
    case popGE f_size t of
    (Just (v_pos, v_size), t') | v_pos < f_pos ->
        -- Found large-enough space before file.
        ((v_pos, f_size, f_id), validate "after insert" t'') where
            d = v_size - f_size
            t'' = (if d > 0 then insert (v_pos + f_size, d) else id) (validate "after popGE" t')
    _ -> ((f_pos, f_size, f_id), t)

checksum :: [File] -> Integer
checksum = sum . map (\(pos, size, id) -> sum [pos..pos+size-1] * id)

-- BEYOND THIS POINT: OPTIONAL UTILS

-- Yields the series of steps involved in compacting.
compactScan :: [File] -> StepTree -> [(File, StepTree)]
compactScan fs t = List.unfoldr go (fs, t) where
    go :: ([File], StepTree) -> Maybe ((File, StepTree), ([File], StepTree))
    go ([], _) = Nothing
    go (f : fs, t) = Just ((f', t'), (fs, t')) where
        (f', t') = moveForward f t

showGraph :: StepTree -> String
showGraph t = List.intercalate "\n" $ go 0 t where
    indent :: Int -> String
    indent n = replicate (n * 2) ' '
    go :: Int -> StepTree -> [String]
    go depth (StepTree nodes) = concatMap showEntry (Map.assocs nodes) where
        showEntry :: (Size, (Pos, StepTree)) -> [String]
        showEntry (size, (pos, t)) =
            (indent depth ++ show (size, pos)) : go (depth + 1) t

nodeCount :: StepTree -> Int
nodeCount = sum . map (\((p, s), t) -> Map.size (nodes t) + nodeCount t) . assocs

volume :: StepTree -> Integer
volume = sum . map (\((p, s), t) -> s + volume t) . assocs

instance Show StepTree where
    show :: StepTree -> String
    show t = "StepTree{" ++ List.intercalate ", " (map showAssoc (assocs t)) ++ "}" where
        showAssoc :: (Space, StepTree) -> String
        showAssoc (k, t) = show k ++ "[" ++ show (volume t) ++ "/" ++ show (nodeCount t) ++ "]"

-- Check that a tree is valid.
-- In-order traversal must have increasing index.
-- Each node should dominate all of its children.
errcheck :: StepTree -> Maybe String
errcheck tree
    | not $ domIsOk tree = Just "violates domination"
    | not $ orderIsOk $ inOrder tree = Just "violates position order"
    | otherwise = Nothing
    where
        domIsOk :: StepTree -> Bool
        domIsOk t = all assocOk (assocs t) && all (domIsOk . snd) (assocs t)
        assocOk :: (Space, StepTree) -> Bool
        assocOk (x, t) = all ((\y -> x `dominates` y) . fst) (assocs t)
        orderIsOk :: [Space] -> Bool
        orderIsOk ((p, s) : xs@((p', s') : _)) = p <= p' && orderIsOk xs
        orderIsOk _ = True

validate :: String -> StepTree -> StepTree
validate _ = id
-- validate name t = case errcheck t of
--     Just err -> error $ "invalid tree: " ++ name ++ ": " ++ err
--     _ -> t

inOrder :: StepTree -> [Space]
inOrder (StepTree m) = concatMap inOrderAssoc $ Map.assocs m where
    inOrderAssoc :: (Size, (Pos, StepTree)) -> [Space]
    inOrderAssoc (size, (pos, t)) = (pos, size) : inOrder t
