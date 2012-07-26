{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Data.Feature
( Feature
, memo
, isBeg
, known
, orth
, lowerOrth
, withUpperOrth
, prefix
, suffix
, substr
, shape
, pack
, join
) where

import Control.Applicative ((<$>))

import qualified Data.Text.Lazy as L
import qualified Data.Vector as V
import qualified Data.Char as C
import qualified Data.Set as S

import qualified Data.Morphosyntax.Class as M

type Feature w  = V.Vector w -> Int -> [L.Text]
type Trafo      = [L.Text] -> [L.Text]

memo :: Int -> (Int -> [a]) -> Int -> [a]
memo n f = doIt 
  where
    v = V.fromList [f k | k <- [0..n-1]]
    doIt k
        | k < 0 || k >= n = []
        | otherwise = v V.! k

isBeg :: Feature w
isBeg s k
    | k < 0 || k >= n = []
    | k == 0    = ["B"] -- beginning 
    | otherwise = ["I"] -- inside
  where
    n = V.length s

known :: M.Morph w => Feature w
known s k
    | M.known (s V.! k) = ["1"]
    | otherwise         = ["0"]

orth :: M.Morph w => Feature w
orth s k
    | k < 0 || k >= n = []
    | otherwise = [M.orth (s V.! k)]
  where
    n = V.length s

lowerOrth :: M.Morph w => Feature w
lowerOrth s = map L.toLower . orth s

withUpperOrth :: M.Morph w => Feature w
withUpperOrth s k = do
    x <- orth s k
    case L.find C.isUpper x of
        Just _  -> [x]
        Nothing -> []

-- | Prefix(es) of given observation feature values.
prefix :: Int -> Trafo
prefix prefOf xs = do
    x <- xs 
    let xLen = L.length x
    let prefOf' = if prefOf <= 0
        then xLen + fromIntegral prefOf
        else fromIntegral prefOf
    if 0 < prefOf' && prefOf' <= xLen
        then [L.take prefOf' x]
        else []

-- | Suffix(es) of given observation feature values.
suffix :: Int -> Trafo
suffix suffOf xs = do
    x <- xs 
    let xLen = L.length x
    let suffOf' = if suffOf <= 0
        then xLen + fromIntegral suffOf
        else fromIntegral suffOf
    if 0 < suffOf' && suffOf' <= xLen
        then [L.drop (xLen - suffOf') x]
        else []

-- | All substrings of size sn and given observation feature values.
-- TODO: Should `removeDups' be really used ?
substr :: Int -> Trafo
substr sn xs = removeDups $ do
    x <- xs
    i <- [0 .. fromIntegral (L.length x) - sn]
    return $ takeSub x i sn
  where
    -- removeDups = id
    removeDups = S.toList . S.fromList
    takeSub x start n = L.take (fromIntegral n)
                      $ L.drop (fromIntegral start) x

shape :: Trafo
shape = map (L.map translate)
  where
    translate char
        | C.isLower char = 'l'
        | C.isUpper char = 'u'
        | C.isDigit char = 'd'
        | otherwise      = 'x'

pack :: Trafo
pack = map (L.pack . map L.head . L.group)

join :: L.Text -> [L.Text] -> [L.Text] -> [L.Text]
join with xs ys = do
    x <- xs
    y <- ys
    return $ x `L.append` with `L.append` y
