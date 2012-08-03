{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Data.Feature
( Feature
, tag
, iff
, group
, memo

, atG
, atB
, at

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

-- | We do not want to be dependent on any specific token
-- representation, but rather allow any token type.

type Feature w  = V.Vector w -> Int -> [L.Text]
type Trafo      = [L.Text] -> [L.Text]

-- | TODO: To many atX combinators, find better solution.
atG :: V.Vector w -> (w -> a) -> Int -> Maybe a
atG s f k
    | k < 0 || k >= n = Nothing
    | otherwise = Just $ f $ s V.! k
  where
    n = V.length s

atB :: V.Vector w -> (w -> Bool) -> Int -> Bool
atB s f k = case atG s f k of
    Just x  -> x
    Nothing -> False

at :: V.Vector w -> (w -> [a]) -> Int -> [a]
at s f k = case atG s f k of
    Just x  -> x
    Nothing -> []

{-# INLINE tag #-}
tag :: Int -> L.Text -> L.Text
tag i = (L.pack (show i ++ ".") `L.append`)

iff :: Bool -> [[L.Text]] -> [L.Text]
iff False xss = []
iff True  xss = concat
    [ map (tag i) xs
    | (i, xs) <- zip [0..] xss ]

group :: [[L.Text]] -> [L.Text]
group = concat

memo :: Int -> (Int -> [a]) -> Int -> [a]
memo n f = doIt 
  where
    v = V.fromList [f k | k <- [0..n-1]]
    doIt k
        | k < 0 || k >= n = []
        | otherwise = v V.! k

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
