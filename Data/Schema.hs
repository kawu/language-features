module Data.Schema
( Schema
, runSchema
) where

import qualified Data.Text.Lazy as L
import qualified Data.Vector as V

import qualified Data.Morphosyntax.Class as M
import qualified Data.Feature as F

type Schema w = V.Vector w -> Int -> [[L.Text]]

runSchema :: Schema w -> V.Vector w -> [[L.Text]]
runSchema schema sent =
    [ concat 
        [ map (tag i) xs
        | (xs, i) <- zip (runOn k) [1..] ]
    | k <- [0 .. n - 1] ]
  where
    n = V.length sent
    runOn :: Int -> [[L.Text]]
    runOn = schema sent
    tag i = (L.pack (show i) `L.append`)

-- | Here is an example of how you can define your schema.  Note, that
-- you can ensure explicit subexpression elimination using standard haskell
-- functions, as well as memoize some of transitional results.
--
-- schema :: M.Morph w => Schema w
-- schema sent = \k ->
--     [ orth (k-1), orth k, orth (k+1)
--     , F.substr 3 $ orth k, F.substr 2 $ orth k
--     , shape (k-1), shape k
--     , packedShape (k-1), packedShape k ]
--   where
--     n = V.length sent
--     orth = F.orth sent
--     shape = F.memo n (F.shape . orth)
--     packedShape = F.pack . shape
