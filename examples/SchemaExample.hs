{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.Vector as V
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Data.Morphosyntax.Canonical (Word)
import Data.Morphosyntax.Raw (toCanoData)
import Text.Morphosyntax.Tagset (parseTagset)
import Text.Morphosyntax.Plain (parsePlain)

import qualified Data.Feature as F
import Data.Schema

-- | Here is an example of how you can define your schema.  Note, that
-- you can ensure explicit subexpression elimination using plain haskell,
-- as well as memoize some of transitional results.
schema :: Schema Word
schema sent = \k ->
    [ orth (k-1), orth k, orth (k+1)
    , F.substr 3 $ orth k, F.substr 2 $ orth k
    , shape (k-1), shape k
    , packedShape (k-1), packedShape k ]
  where
    n = V.length sent
    orth = F.orth sent
    shape = F.memo n (F.shape . orth)
    packedShape = F.pack . shape

main = do
    [tagsetPath, inPath] <- getArgs
    tagset <- parseTagset tagsetPath <$> readFile tagsetPath
    cano   <- map V.fromList . toCanoData tagset
            . map (map fst) . parsePlain
          <$> L.readFile inPath
    forM_ cano $ \sent -> do
        forM_ (runSchema schema sent) $ \word ->
            L.putStrLn $ L.intercalate " " word
        putStrLn ""
