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

data Table = Table { bySize :: Map Size (Pos, Table) }

instance Show Table where
    show t = List.intercalate "\n" $ go 0 t where
        indent :: Int -> String
        indent n = replicate (n * 2) ' '
        go :: Int -> Table -> [String]
        go depth (Table bySize) = concatMap showEntry (Map.assocs bySize) where
            showEntry :: (Size, (Pos, Table)) -> [String]
            showEntry (size, (pos, t)) =
                (indent depth ++ show (size, pos)) : go (depth + 1) t

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

emptyTable :: Table
emptyTable = Table Map.empty

-- Assumes that none of the spaces overlap.
insert :: Space -> Table -> Table
insert (pos, size) (Table bySize) =
    case Map.lookupGE size bySize of
        -- No larger-or-equal space exists; insert new node.
        Nothing -> insertHere
        -- Larger-or-equal space exists at pos'.
        Just (size', (pos', t')) ->
            -- Larger-or-equal space exists at earlier Pos.
            -- Recurse on insertion below this node.
            if pos' < pos then
                Table $ Map.adjust (Arrow.second (insert (pos, size))) size' bySize
            -- No larger-or-equal space before this Pos; insert new node.
            else insertHere
    where
        -- When inserting a new node, move the dominated nodes below it.
        insertHere = Table $ Map.insert size (pos, Table below) rest where
            (below, rest) = Map.partitionWithKey (\s (p, _) -> p > pos && s <= size) bySize

-- Assumes that none of the spaces overlap.
insertNode :: (Size, (Pos, Table)) -> Table -> Table
insertNode node@(size, (pos, Table subtable)) (Table bySize) =
    case Map.lookupGE size bySize of
        -- No larger-or-equal space exists; insert new node.
        Nothing -> insertHere
        -- Larger-or-equal space exists at pos'.
        Just (size', (pos', t')) ->
            -- Larger-or-equal space exists at earlier Pos.
            -- Recurse on insertion below this node.
            if pos' < pos then
                Table $ Map.adjust (Arrow.second (insertNode node)) size' bySize
            -- No larger-or-equal space before this Pos; insert new node.
            else insertHere
    where
        -- When inserting a new node, move the dominated nodes below it.
        -- TODO: Possible to avoid recursing more than necessary here?
        insertHere = Table $ Map.insert size (pos, children) rest where
            (below, rest) = Map.partitionWithKey (\s (p, _) -> p > pos && s <= size) bySize
            children = foldr insertNode (Table below) (Map.assocs subtable)

-- TODO: Could implement O(n) instead of O(n log n) if list is sorted?
makeTable :: [Space] -> Table
makeTable = foldr insert emptyTable

pop :: Space -> Table -> Table
pop (pos, size) (Table bySize) =
    case Map.lookupGE size bySize of
        Nothing -> error "element not found"
        Just (size', (pos', Table bySize')) ->
            if size == size' && pos == pos' then
                -- Remove this node from the table that contains it.
                -- Insert its children in that table.
                foldr insertNode (Table $ Map.delete size bySize) (Map.assocs bySize')
            else
                Table $ Map.adjust (Arrow.second (pop (pos, size))) size' bySize

-- Files must be provided in reverse order.
compact :: [File] -> Table -> [File]
compact [] table = []
compact ((f_pos, f_size, f_id) : fs) t =
    -- Look for a space to put this file.
    case Map.lookupGE f_size (bySize t) of
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