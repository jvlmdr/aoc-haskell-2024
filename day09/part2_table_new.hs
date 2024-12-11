import Data.Char (digitToInt)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Control.Arrow as Arrow
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

main :: IO ()
main = do
    input <- getLine
    let files = makeFileList $ map (toInteger . digitToInt) input
    print $ checksum $ compact (reverse files) (makeTable $ makeSpaceList files)

type Pos = Integer
type Size = Integer
type Id = Integer
type File = (Pos, Size, Id)
type Space = (Pos, Size)

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
-- We call this a StepTree because each level contains the
-- step increases in interval size.
data StepTree = StepTree
    { nodes :: Map Size (Pos, StepTree) }

-- A space dominates another if it occurs earlier and is larger-or-equal in size.
dominates :: Space -> Space -> Bool
dominates (pos, size) (p, s) = pos < p && size >= s

fromAssoc :: (Size, (Pos, StepTree)) -> Space
fromAssoc (size, (pos, _)) = (pos, size)

-- Returns the (pos, size) pairs defining the steps at this level.
steps :: StepTree -> [Space]
steps (StepTree m) = map fromAssoc $ Map.assocs m

empty :: StepTree
empty = StepTree Map.empty

consec :: [a] -> [(a, a)]
consec xs = zip xs (tail xs)

-- TODO: Avoid linear search within node?
findStepBefore :: Pos -> StepTree -> Maybe Space
findStepBefore pos = Map.lookupLE pos . Map.fromList . steps

findAssocBefore :: Pos -> StepTree -> Maybe (Size, (Pos, StepTree))
findAssocBefore pos tree = do
    (_, s) <- findStepBefore pos tree
    (p, t) <- Map.lookup s (nodes tree)  -- Should never fail.
    return (s, (p, t))

peekGE :: Size -> StepTree -> Maybe Space
peekGE minSize (StepTree nodes) = fromAssoc <$> Map.lookupGE minSize nodes

makeTable :: [Space] -> StepTree
makeTable = foldr insert empty

insert :: Space -> StepTree -> StepTree
insert x@(pos, size) t =
    case findAssocBefore pos t of
        -- Preceding step dominates this one.
        -- Recurse below that node.
        Just (size', _) | size' >= size ->
            StepTree $ Map.adjust (Arrow.second (insert x)) size' (nodes t)
        -- Preceding step does not dominate this one (or no preceding step).
        -- Insert a new step at this level.
        -- The preceding step may need to be split.
        -- The following steps may be dominated by the new node.
        Nothing -> y1
        -- Preceding step must be split.
        Just (size', (pos', t')) ->
            -- Recursively remove nodes after pos.
            let y2 = validate "Map.insert removeAfter" $ StepTree $ Map.insert size' (pos', removeAfter pos t') (nodes y1) in
            -- Re-insert the nodes (with their subtrees) into the table.
            validate "foldr reinsert y2 findAfter" $ foldr reinsert y2 (findAfter pos t')
    where
        (dom, rest) = Map.partitionWithKey (\s (p, _) -> x `dominates` (p, s)) (nodes t)
        y1 = StepTree $ Map.insert size (pos, StepTree dom) rest

-- TODO: Easy way to combine `removeAfter` and `findAfter`?

-- Remove all nodes that start after a given position.
removeAfter :: Pos -> StepTree -> StepTree
removeAfter pos (StepTree nodes) =
    let (keep, remove) = Map.partition (\(p, _) -> p < pos) nodes in
    StepTree $ Map.map (Arrow.second (removeAfter pos)) keep

-- Remove all nodes that start after a given position.
findAfter :: Pos -> StepTree -> [(Size, (Pos, StepTree))]
findAfter pos (StepTree nodes) =
    let (keep, remove) = Map.partition (\(p, _) -> p < pos) nodes in
    Map.assocs remove ++ concatMap (findAfter pos . snd) (Map.elems keep)

popGE :: Size -> StepTree -> (Maybe Space, StepTree)
popGE minSize tree =
    case Map.lookupGE minSize (nodes tree) of
        -- No spaces that are sufficiently large.
        Nothing -> (Nothing, tree)
        -- Found the first node that satisfies the condition.
        -- Remove this from the table and re-insert its children.
        Just (size, (pos, subtree)) -> (Just (pos, size), tree') where
            tree' = foldr reinsert (StepTree $ Map.delete size (nodes tree)) (Map.assocs (nodes subtree))

-- Like insert but each element has a subtree.
-- Since it is a "reinsert", the subtrees do not need to be traversed.
reinsert :: (Size, (Pos, StepTree)) -> StepTree -> StepTree
reinsert a@(size, (pos, subt)) t =
    case findStepBefore pos t of
        -- Preceding step dominates this one.
        -- Recurse below that node.
        Just (pos', size') | size' >= size ->
            StepTree $ Map.adjust (Arrow.second (reinsert a)) size' (nodes t)
        -- Preceding step does not dominate this one (or no preceding step).
        -- It should not be necessary to split or move any surrounding steps.
        -- Simply insert the step at this level.
        _ -> StepTree $ Map.insert size (pos, subt) (nodes t)

-- Files must be provided in order to be moved.
compact :: [File] -> StepTree -> [File]
compact [] t = []
compact (f : fs) t = f' : compact fs t' where (f', t') = moveForward f t

moveForward :: File -> StepTree -> (File, StepTree)
moveForward (f_pos, f_size, f_id) t =
    -- Look for a space to put this file.
    case popGE f_size (validate "before popGE" t) of
    (Just (v_pos, v_size), t') | v_pos < f_pos ->
        -- Found large-enough space before file.
        ((v_pos, f_size, f_id), validate "after insert" t'') where
            d = v_size - f_size
            t'' = (if d > 0 then insert (v_pos + f_size, d) else id) (validate "after popGE" t')
    _ -> ((f_pos, f_size, f_id), t)

checksum :: [File] -> Integer
checksum = sum . map (\(pos, size, id) -> sum [pos..pos+size-1] * id)

-- -- BEYOND THIS POINT: OPTIONAL UTILS

instance Show StepTree where
    show t = List.intercalate "\n" $ go 0 t where
        indent :: Int -> String
        indent n = replicate (n * 2) ' '
        go :: Int -> StepTree -> [String]
        go depth (StepTree nodes) = concatMap showEntry (Map.assocs nodes) where
            showEntry :: (Size, (Pos, StepTree)) -> [String]
            showEntry (size, (pos, t)) =
                (indent depth ++ show (size, pos)) : go (depth + 1) t

assocs :: StepTree -> [(Space, StepTree)]
assocs (StepTree m) = [((p, s), t) | (s, (p, t)) <- Map.assocs m]

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
--     Just err -> error $ "not ok: " ++ name ++ ": " ++ err
--     _ -> t

inOrder :: StepTree -> [Space]
inOrder (StepTree m) = concatMap inOrderAssoc $ Map.assocs m where
    inOrderAssoc :: (Size, (Pos, StepTree)) -> [Space]
    inOrderAssoc (size, (pos, t)) = (pos, size) : inOrder t
