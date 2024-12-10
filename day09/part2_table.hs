import Data.Char (digitToInt)
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
    print $ makeSpaceList files
    print $ makeTable $ makeSpaceList files
    print files
    print $ makeTable $ makeSpaceList files
    print $ List.sort $ compact (reverse files) (makeTable $ makeSpaceList files)
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
-- 2) pop the earliest occuring interval larger-or-equal to a size
--
-- For example, consider the intervals (input "3225414133402")
--   pos:   3,  7, 16, 21, 25
--   size:  2,  5,  1,  1,  3
-- This will yield the tree structure, using (size,pos):
--   + (2,3)
--   + (5,7)
--     + (1,16)
--     | + (1,21)
--     + (3,25)
-- When we come to pop an element from this structure,
-- we will either take (2,3) or (5,7).
-- These would be taken before any of the other elements.
--
-- It then remains to merge the subtree into the top-level table.
-- They may either go at the top level or below a preceding node.
-- For example, if we pop the size-5 element above, we obtain:
--   + (2,3)
--   | + (1,16)
--   |   + (1,21)
--   + (3,25)
-- We may need to recurse down the preceding node,
-- but we shouldn't need to recurse down the subtree.
--
-- We call this a StepTree because each level contains the
-- step increases in interval size.
data StepTree = StepTree { nodes :: Map Size (Pos, StepTree) }

instance Show StepTree where
    show t = List.intercalate "\n" $ go 0 t where
        indent :: Int -> String
        indent n = replicate (n * 2) ' '
        go :: Int -> StepTree -> [String]
        go depth (StepTree nodes) = concatMap showEntry (Map.assocs nodes) where
            showEntry :: (Size, (Pos, StepTree)) -> [String]
            showEntry (size, (pos, t)) =
                (indent depth ++ show (size, pos)) : go (depth + 1) t

fromAssoc :: (Size, (Pos, StepTree)) -> Space
fromAssoc (size, (pos, _)) = (pos, size)

empty :: StepTree
empty = StepTree Map.empty

peekGE :: Size -> StepTree -> Maybe Space
peekGE size (StepTree nodes) = fromAssoc <$> Map.lookupGE size nodes

-- A space dominates another if it occurs earlier and is larger-or-equal in size.
dominates :: Space -> Space -> Bool
dominates (pos, size) (p, s) = pos < p && size >= s

insert :: Space -> StepTree -> StepTree
insert x@(pos, size) (StepTree nodes) =
    case Map.lookupGE size nodes of
        -- Found larger-or-equal space that comes before position.
        -- Recurse on that node and replace the entry at this level.
        Just (size', (pos', t')) | pos' < pos ->
            StepTree $ Map.insert size' (pos, insert x t') nodes
        -- No larger-or-equal space before this position.
        -- Insert a new node at this level.
        -- Split existing nodes into dominated (below) and siblings (beside).
        _ -> StepTree $ Map.insert size (pos, StepTree dom) sib where
            (dom, sib) = Map.partitionWithKey (\s (p, _) -> x `dominates` (p, s)) nodes

insertNode :: (Size, (Pos, StepTree)) -> StepTree -> StepTree
insertNode node@(size, (pos, StepTree subtable)) (StepTree nodes) =
    case Map.lookupGE size nodes of
        -- No larger-or-equal space exists; insert new node.
        Nothing -> insertHere
        -- Larger-or-equal space exists at pos'.
        Just (size', (pos', t')) ->
            -- Larger-or-equal space exists at earlier Pos.
            -- Recurse on insertion below this node.
            if pos' < pos then
                StepTree $ Map.adjust (Arrow.second (insertNode node)) size' nodes
            -- No larger-or-equal space before this Pos; insert new node.
            else insertHere
    where
        -- When inserting a new node, move the dominated nodes below it.
        -- TODO: Possible to avoid recursing more than necessary here?
        insertHere = StepTree $ Map.insert size (pos, children) rest where
            (below, rest) = Map.partitionWithKey (\s (p, _) -> p > pos && s <= size) nodes
            children
              | Map.null subtable = StepTree below
              | Map.null below = StepTree subtable
              | otherwise = foldr insertNode (StepTree below) (Map.assocs subtable)

-- TODO: Could implement O(n) instead of O(n log n) if list is sorted?
makeTable :: [Space] -> StepTree
makeTable = foldr insert empty

pop :: Space -> StepTree -> StepTree
pop (pos, size) (StepTree nodes) =
    case Map.lookupGE size nodes of
        Nothing -> error "element not found"
        Just (size', (pos', StepTree nodes')) ->
            if size == size' && pos == pos' then
                -- Remove this node from the table that contains it.
                -- Insert its children in that table.
                foldr insertNode (StepTree $ Map.delete size nodes) (Map.assocs nodes')
            else
                StepTree $ Map.adjust (Arrow.second (pop (pos, size))) size' nodes

-- Files must be provided in reverse order.
compact :: [File] -> StepTree -> [File]
compact [] table = []
compact ((f_pos, f_size, f_id) : fs) t =
    -- Look for a space to put this file.
    case Map.lookupGE f_size (nodes t) of
        -- No space; cannot move file.
        Nothing -> (f_pos, f_size, f_id) : compact fs t
        -- There is space; move the file if it's closer.
        Just (v_size, (v_pos, _)) ->
            if v_pos < f_pos then
                (v_pos, f_size, f_id) : compact fs t''
            else
                (f_pos, f_size, f_id) : compact fs t
            where
                gap = v_size - f_size
                t' = pop (v_pos, v_size) t
                t'' = (if gap > 0 then insert (v_pos + f_size, gap) else id) t'

checksum :: [File] -> Integer
checksum = sum . map (\(pos, size, id) -> sum [pos..pos+size-1] * id)