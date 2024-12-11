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
makeSpaceList fs = catMaybes $ zipWith spaceBetween fs (tail fs) where
    spaceBetween :: File -> File -> Maybe Space
    spaceBetween (p1, s1, _) (p2, _, _)
        | p1 + s1 < p2 = Just (p1 + s1, p2 - p1 - s1)
        | otherwise = Nothing

data StepTree = StepTree { sizeIndex :: Map Size (Pos, StepTree) }

-- A space dominates another if it occurs earlier and is larger-or-equal in size.
dominates :: Space -> Space -> Bool
dominates (pos, size) (p, s) = pos < p && size >= s

emptyTable :: StepTree
emptyTable = StepTree Map.empty

-- Assumes that none of the spaces overlap.
insert :: Space -> StepTree -> StepTree
insert (pos, size) (StepTree sizeIndex) =
    case Map.lookupGE size sizeIndex of
        -- No larger-or-equal space exists; insert new node.
        Nothing -> insertHere
        -- Larger-or-equal space exists at pos'.
        Just (size', (pos', t')) ->
            -- Larger-or-equal space exists at earlier Pos.
            -- Recurse on insertion below this node.
            if pos' < pos then
                StepTree $ Map.adjust (Arrow.second (insert (pos, size))) size' sizeIndex
            -- No larger-or-equal space before this Pos; insert new node.
            else insertHere
    where
        -- When inserting a new node, move the dominated nodes below it.
        insertHere = StepTree $ Map.insert size (pos, StepTree below) rest where
            (below, rest) = Map.partitionWithKey (\s (p, _) -> p > pos && s <= size) sizeIndex

-- Assumes that none of the spaces overlap.
insertNode :: (Size, (Pos, StepTree)) -> StepTree -> StepTree
insertNode node@(size, (pos, StepTree subtable)) (StepTree sizeIndex) =
    case Map.lookupGE size sizeIndex of
        -- No larger-or-equal space exists; insert new node.
        Nothing -> insertHere
        -- Larger-or-equal space exists at pos'.
        Just (size', (pos', t')) ->
            -- Larger-or-equal space exists at earlier Pos.
            -- Recurse on insertion below this node.
            if pos' < pos then
                StepTree $ Map.adjust (Arrow.second (insertNode node)) size' sizeIndex
            -- No larger-or-equal space before this Pos; insert new node.
            else insertHere
    where
        -- When inserting a new node, move the dominated nodes below it.
        -- TODO: Possible to avoid recursing more than necessary here?
        insertHere = StepTree $ Map.insert size (pos, children) rest where
            (below, rest) = Map.partitionWithKey (\s (p, _) -> p > pos && s <= size) sizeIndex
            children
              | Map.null subtable = StepTree below
              | Map.null below = StepTree subtable
              | otherwise = foldr insertNode (StepTree below) (Map.assocs subtable)

-- TODO: Could implement O(n) instead of O(n log n) if list is sorted?
makeTable :: [Space] -> StepTree
makeTable = foldr insert emptyTable

pop :: Space -> StepTree -> StepTree
pop (pos, size) (StepTree sizeIndex) =
    case Map.lookupGE size sizeIndex of
        Nothing -> error "element not found"
        Just (size', (pos', StepTree sizeIndex')) ->
            if size == size' && pos == pos' then
                -- Remove this node from the table that contains it.
                -- Insert its children in that table.
                foldr insertNode (StepTree $ Map.delete size sizeIndex) (Map.assocs sizeIndex')
            else
                StepTree $ Map.adjust (Arrow.second (pop (pos, size))) size' sizeIndex

-- Files must be provided in reverse order.
compact :: [File] -> StepTree -> [File]
compact [] table = []
compact ((f_pos, f_size, f_id) : fs) t =
    -- Look for a space to put this file.
    case Map.lookupGE f_size (sizeIndex t) of
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
                t' = validate "after pop" $ pop (v_pos, v_size) t
                t'' = (if gap > 0 then insert (v_pos + f_size, gap) else id) t'

checksum :: [File] -> Integer
checksum = sum . map (\(pos, size, id) -> sum [pos..pos+size-1] * id)


-- BEYOND THIS POINT: OPTIONAL UTILS

instance Show StepTree where
    show t = List.intercalate "\n" $ go 0 t where
        indent :: Int -> String
        indent n = replicate (n * 2) ' '
        go :: Int -> StepTree -> [String]
        go depth (StepTree sizeIndex) = concatMap showEntry (Map.assocs sizeIndex) where
            showEntry :: (Size, (Pos, StepTree)) -> [String]
            showEntry (size, (pos, t)) =
                (indent depth ++ show (size, pos)) : go (depth + 1) t

steps :: StepTree -> [(Space, StepTree)]
steps (StepTree m) = [((p, s), t) | (s, (p, t)) <- Map.assocs m]

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
        domIsOk t = all assocOk (steps t) && all (domIsOk . snd) (steps t)
        assocOk :: (Space, StepTree) -> Bool
        assocOk (x, t) = all ((\y -> x `dominates` y) . fst) (steps t)
        orderIsOk :: [Space] -> Bool
        orderIsOk ((p, s) : xs@((p', s') : _)) = p <= p' && orderIsOk xs
        orderIsOk _ = True

validate :: String -> StepTree -> StepTree
validate name t = case errcheck t of
    -- Just err -> error $ "invalid tree: " ++ name ++ ": " ++ err
    _ -> t

inOrder :: StepTree -> [Space]
inOrder (StepTree m) = concatMap inOrderAssoc $ Map.assocs m where
    inOrderAssoc :: (Size, (Pos, StepTree)) -> [Space]
    inOrderAssoc (size, (pos, t)) = (pos, size) : inOrder t

posRange :: StepTree -> Maybe (Pos, Pos)
posRange t
    | Map.null (sizeIndex t) = Nothing
    | otherwise = Just (minimum xs, maximum xs) where xs = map fst $ inOrder t
