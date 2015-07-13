-- PredInference.hs

module PredInference (
  Pred(..)
, unify
) where

import Data.List (intercalate,sortBy)

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
unify x' y' = unify_ x' y' (Just []) >>= replVars
  where unify_ x y s
          | Nothing <- s = Nothing
          | x == y       = s
          | V vx <- x    = s >>= unifyVar x y
          | V vy <- y    = s >>= unifyVar y x
          -- unify preds / function names
          | F px argx <- x, F py argy <- y, px == py, length argx == length argy
            = foldr (\(xa,ya) acc -> unify_ xa ya acc) s $ zip argx argy
          | otherwise    = Nothing
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
          | otherwise                                = False

        -- topologically sort subs based on the occurences
        -- of vars in the substitution
        toposortSubs s = toposortSubs_ subGraph s
          where toposortSubs_ g [] = []
                toposortSubs_ g s  =
                  let (x:xs) = sortBy (cmpInEdges g) s in
                  x:(toposortSubs_ (filter (\(n,_) -> n /= fst x) g) xs)
                makeEdges (name, F _ args) = map (curry makeEdges name) args >>= id
                makeEdges (name, V vname)  = [(name, vname)]
                makeEdges (name, C _)      = []
                subGraph = map makeEdges s >>= id
                cmpInEdges g (n1,_) (n2,_) = inEdges g n2 `compare` inEdges g n1
                inEdges g n = length $ filter (\(_,n') -> n == n') g

        -- replace vars in substitutions
        replVars s = Just $ replVars_ $ toposortSubs s
        replVars_ [] = []
        replVars_ (x@(name,val):xs) =
          x:(replVars_ $ zip (map fst xs) $ map (replVar name val) $ map snd xs)
        replVar name sub val
          | F vname args <- val           = F vname $ map (replVar name sub) args
          | V vname <- val, name == vname = sub
          | otherwise                     = val

main = do
  let x = F "Knows" [V "y", C "john", V "x"]
  let y = F "Knows" [F "father" [F "father" [V "x"]], C "john", F "mother" [C "jane"]]
  print $ unify x y
