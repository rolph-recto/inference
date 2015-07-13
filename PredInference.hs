-- PredInference.hs

module PredInference (
  Pred(..)
, unify
) where

import Data.List (intercalate)

data Pred = F String [Pred] -- predicates or function names
          | V String        -- variable
          | C String        -- constant name
          deriving (Eq)

instance Show Pred where
  show (F p args) = p ++ "(" ++ (intercalate "," $ map show args) ++ ")"
  show (V vname)  = vname
  show (C cname)  = cname

type Sub = [(String,Pred)]

unify :: Pred -> Pred -> Maybe Sub
unify x' y' = unify_ x' y' (Just [])
  where unify_ x y s
          | Nothing <- s = Nothing
          | x == y       = s
          | V vx <- x  = s >>= unifyVar x y
          | V vy <- y  = s >>= unifyVar y x
          -- unify preds / function names
          | F px argx <- x, F py argy <- y, px == py, length argx == length argy
            = foldr (\(xa,ya) acc -> unify_ xa ya acc) s $ zip argx argy
          | otherwise = Nothing
        unifyVar var@(V vname) x s
          | Just val <- lookup vname s               = unify_ val x (Just s)
          | V xname <- x, Just val <- lookup xname s = unify_ var val (Just s)
          | vname `occurs` x                         = Nothing
          | otherwise                                = Just $ (vname,x):s
        -- occurs check
        occurs vname x
          | V xname <- x, vname == xname             = True
          | C _ <- x                                 = False
          | F _ args <- x                            = any (vname `occurs`) args

main = do
  let x = F "Knows" [C "john", V "x"]
  let y = F "Knows" [C "john", F "mother" [C "jane"]]
  print $ unify x y
