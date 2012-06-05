{-# LANGUAGE TypeSynonymInstances
           , RankNTypes #-} 

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Monoid
import Data.Maybe (isJust, fromJust)
import Data.List (partition, mapAccumL, sortBy, delete, elemIndex)
import Data.Tree

type ID = Int
type Shift = Int
type IsUnique = Bool

-- | Podczas definiowania funkcji (pos, substring) chcemy każdej takiej
-- funkcji nadać identyfikator.  Ponadto, niektóre z tych funkcji (np.
-- substring) przyjmują dodatkowy argument, który tak na prawdę dopiero
-- określa o jaką nam chodzi funkcję.  Tak więc (substring 3) i (substring 4)
-- mają być dwiema różnymi funkcjami w naszym drzewie wyrażeń, podczas
-- gdy (substring 3) i (substring 3) są oczywiście tymi samymi funkcjami.
--
-- Chcielibyśmy więc najpierw "zarejestrować" funkcje pos i substring
-- w ramach aplikacji (monady) Ox:
-- pos <- newFun getPos
-- substring <- newFun getSubstring 
--
-- Ale funkcja (Fun a b) powinna powstać dopiero podczas definiowania schematu:
-- [ ..., substring 3 # orth 1, ... ]
-- Ostateczny identyfikator powinien więc powstać właśnie na typ etapie.
--
-- Propozycja: identyfikator powinien być złożeniem głównego identyfikatora
-- otrzymanego podczas rejestracji funkcji oraz dodatkowych artumentów
-- podawanych w trakcie definiowania schematu (np. 3 w wyrażeniu substring 3).
--
-- Jak to zrealizować? Wydaje się jasne, że typ ID = Int nie jest już odpowiedni.
-- Jedyną "cechą", której wymagamy od ID, jest to aby był elementem klasy Eq. 
--
-- Rozważmy kilka przykładów definicji funkcji:
-- newFun2 :: (a -> b) -> Ox (Fun a b)
-- newFun3 :: (a -> b -> c) -> Ox (a -> Fun b c)
-- newFun4 :: (a -> b -> c -> d) -> Ox (a -> b -> Fun c d)
-- Raczej nie da się tej definicji połączyć w jedną, zostawmy ten wątek
-- na później.
--
-- Weźmy pod lupę newFun3.  W trakcie jej wywołania generujemy nowy
-- identyfikator "podstawowy", a identyfikator "końcowy" dopiero gdy
-- otrzymamy kolejny argument typu 'a'.  Zakładając, że identyfikator
-- podstawowy jest typu ID, chcielibyśmy dopuszczać wszystkie typy
-- 'a' względem których można sensownie zdefiniować *różnowartościową*
-- (przy zadanych pierwszych dwóch argumentach) funkcję ID -> a -> ID.
--
-- UWAGA UWAGA! Banał! ID = [String], przy czym zakładamy, że argumenty
-- są elementami klasy Show! Przyjmujemy przy tym ciche (czy na pewno ciche?
-- przecież musi istnieć jednoznacznośc(?) Show<->Read) założenie, że
-- dla dwóch różnych x i x' tego samego typu (show x /= show x').  Włuala!

-- | Function a -> b with identifier.
data Fun a b = Fun
    { funId   :: ID
    , funArgs :: [String]
    , funBody :: a -> b }

instance Show (Fun a b c) where
    show (Fun id args _) = "Fun id=" ++ show id ++ " args=" ++ show args
instance Eq (Fun a b c) where
    (Fun id args _) == (Fun id' args' _) = (id, args) == (id', args')

-- | Expression operation for token type t and monoidal value m.
data Op t m = Base IsUnique (Fun t m) Shift
            | App  IsUnique (Fun [m] m)
              deriving (Show, Eq)

data Ptr = Ptr Shift ID deriving (Show, Eq)

-- type Expr op    = Tree op
-- type ExprID op  = Tree (op, ID)
-- type ExprPtr op = Tree (Either (op, ID) Ptr)
-- 
-- getId :: ExprID op -> ID
-- getId (Node (_, id) _) = id

-- type Schema t a    = [Expr t a]
-- type SchemaID t a  = [ExprID t a]
-- type SchemaPtr t a = [ExprPtr t a]

-- (.$.) :: Op t m -> Expr t m -> Expr t m
-- (.$.) f e = Node f [e]
-- infixr 1 .$.
-- 
-- (#) :: Maybe Shift -> Maybe Shift -> Maybe Shift
-- Just x # Just y
--     | x == y    = Just x
--     | otherwise = Nothing
-- _ # _ = Nothing
-- 
-- shiftAll = foldl1 (#)
-- 
-- class Sim t where
--     (~>) :: t -> t -> Maybe Shift
-- 
-- instance Sim (Expr t a) where
--     Node (Base False t s) _ ~> Node (Base False t' s') _
--         | t == t'   = Just (s - s')
--         | otherwise = Nothing
--     Node (App False f) [e] ~> Node (App False f') [e']
--         | funId f == funId f' = e ~> e'
--         | otherwise = Nothing
--     _ ~> _  = Nothing
-- 
-- instance Sim (ExprID t a) where
--     e ~> e' = fmap fst e ~> fmap fst e'
-- 
-- (~~) :: Sim t => t -> t -> Bool
-- x ~~ x' = isJust $ x ~> x'
-- 
-- -- | Function for comparing two expressions with IDs from the
-- -- same ~> equivalence class.  It represents evaluation order.
-- (<>) :: ExprID t a -> ExprID t a -> Ordering 
-- e <> e'
--     | k < 0  = GT
--     | k > 0  = LT
--     | k == 0 = compare (getId e) (getId e')
--   where
--     k = case e ~> e' of
--             Just x  -> x
--             Nothing -> error "<>: arguments not ~> similar" 
-- 
-- subTrees :: Tree a -> [Tree a]
-- subTrees x@(Node _ xs) = x : concatMap subTrees xs
-- 
-- unWind :: [Tree a] -> [Tree a]
-- unWind = concatMap subTrees
-- 
-- -- | Find equivalence classes for given relation.
-- findEqs :: (a -> a -> Bool) -> [a] -> [[a]]
-- findEqs _ [] = []
-- findEqs r (e:es) =
--     let (cls, rst) = partition (r e) es
--     in  (e:cls) : findEqs r rst
-- 
-- identExpr :: ID -> Expr t a -> (ID, ExprID t a)
-- identExpr id (Node op xs) =
--     let (id', xs') = mapAccumL identExpr (id + 1) xs
--     in  (id', Node (op, id) xs')
-- 
-- identSchema :: Schema t a -> SchemaID t a
-- identSchema = snd . mapAccumL identExpr 0
-- 
-- type EqClass t a = [ExprID t a]
-- 
-- -- | Add pointers.  We assume, that equivalence classes are sorted
-- -- with respect to (<>) relation.
-- ptrExpr :: [EqClass t a] -> ExprID t a -> ExprPtr t a
-- ptrExpr cs e@(Node x xs)
--     | e == e'   = Node (Left x) (map (ptrExpr cs) xs)
--     | otherwise = Node (Right $ Ptr shift id) []
--   where
--     e' = head [head cls | cls <- cs, e ~~ head cls]
--     id = getId e'
--     shift = fromJust $ e ~> e'
-- 
-- ptrSchema :: SchemaID t a -> SchemaPtr t a
-- ptrSchema schema =
--     let cs = map (sortBy (<>)) $ findEqs (~~) $ unWind schema
--     in  map (ptrExpr cs) schema

-- schema :: SchemaID
-- schema = identSchema 



-- | Monad counting function and expression identifiers.
-- type Ox t a = State (Int, Int) a
-- 
-- newFunIdent :: State (Int, Int) Int
-- newFunIdent = do
--     (x, y) <- get
--     put (x + 1, y)
--     return x
-- 
-- newExprIdent :: State (Int, Int) Int
-- newExprIdent = do
--     (x, y) <- get
--     put (x, y + 1)
--     return y
-- 
-- data Tok = Tok
--     { tokOrth  :: String
--     , tokMorph :: String }
-- 
-- mkFun :: (t -> a) -> Ox t (Fun t a)
-- mkFun f = Fun <$> newFunIdent <*> pure f
-- 
-- mkBase :: (t -> a) -> Ox t (Int -> Expr t a)
-- mkBase f = do
--     ff <- mkFun f
--     return $ \k -> Node (Base False ff k) []
-- 
-- mkApp :: ([m] -> m) -> Ox t (Op t m)
-- mkApp f = App False <$> mkFun f
-- 
-- ident :: Ox t (Op t a) -> Ox t (Op t a, ID)
-- ident e = (,) <$> e <*> newExprIdent
-- 
-- -- (#) :: Ox t (Op t m) -> Ox t (Op t m) -> Ox t ?? 
-- (#) pOx cOx = do
--     c <- cOx
--     p <- pOx
--     x <- newExprIdent
--     return $ Node (p, x) [c]

-- newFun :: Regable f => f -> Ox t (Fun t a)
-- newFun f = 
-- 
-- mkSchema :: Ox Tok (SchemaID Tok String)
-- mkSchema = do
--     orth  <- newBase tokOrth
--     morph <- newBase tokMorph
-- 
--     posF <- newFun (undefined :: String -> String)
--     substringF <- newFun (undefined :: Int -> String -> String)
-- 
--     let pos k = posA #. orth k
--     let substring k = mkApp undefined
--     let prefix k = mkApp undefined
-- 
--     let schema = identSchema
--             [ pos 0
--             , pos 1
--             , orth 1
--             , orth 0
--             , substring 3 #. orth 2
--             , prefix 2 # substring 3 #. orth 2 ]
--     return schema

-- runOx :: Ox t a -> a
-- runOx = undefined



-- main = do
--     let mkStrTree = fmap show
--         mkStrForest = map mkStrTree
--     putStrLn $ drawForest $ mkStrForest $ ptrSchema schema


main = print 1
