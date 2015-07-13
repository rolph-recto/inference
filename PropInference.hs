-- PropInference.hs
-- inference in propositional logic
module PropInference(
  Prop(..)
, Lit(..)
, cnfToProp
-- , resolution
-- , DPLL
) where

import Data.List (intercalate, nub)

-- proositions
data Prop = Or [Prop]
          | And [Prop]
          | Not Prop
          | If Prop Prop
          | Iff Prop Prop
          | Sym String
          deriving (Eq)

instance Show Prop where
  show (Or [d]) = show d
  show (Or disjuncts) = "(" ++ (intercalate " || " $ map show disjuncts) ++ ")"
  show (And [c]) = show c
  show (And conjuncts) = "(" ++ (intercalate " && " $ map show conjuncts) ++ ")"
  show (Not (Sym s)) = "~" ++ s
  show (Not p) = "~(" ++ (show p) ++ ")"
  show (If p q) = (show p) ++ " => " ++ (show q)
  show (Iff p q) = (show p) ++ " <=> " ++ (show q)
  show (Sym s) = s

-- literals
data Lit = LSym String
         | LNot String
         deriving (Eq)

litToSym :: Lit -> Prop
litToSym (LSym s) = Sym s
litToSym (LNot s) = Not (Sym s)

instance Show Lit where
  show = show . litToSym

type CNF = [[Lit]]

cnfToStr :: CNF -> String
cnfToStr cnf =
  intercalate " " $ map (\c -> "[" ++ (intercalate "," $ map show c) ++ "]") cnf

cnfToProp :: CNF -> Prop
cnfToProp cnf = And $ map (Or . map litToSym) cnf

contraLitIn :: Lit -> [Lit] -> Bool
contraLitIn (LSym s) xs = (LNot s) `elem` xs
contraLitIn (LNot s) xs = (LSym s) `elem` xs

-- converts arbitrary prop logic sentences to CNF form
propToCNF :: Prop -> CNF
propToCNF = filterClauses . propToCNF_
  where propToCNF_ (Sym s) = [[LSym s]]
        propToCNF_ (And conjuncts) = conjuncts >>= propToCNF
        propToCNF_ (Or disjuncts) = map (>>= id) $ sequence $ map propToCNF disjuncts
        -- negation elim
        propToCNF_ (Not (Not (Sym s))) = [[LSym s]]
        propToCNF_ (Not (Sym s)) = [[LNot s]]
        propToCNF_ (Not (Not p)) = propToCNF p
        -- demorgan laws
        propToCNF_ (Not (And conjuncts)) = propToCNF $ Or $ map Not conjuncts
        propToCNF_ (Not (Or disjuncts)) = propToCNF $ And $ map Not disjuncts
        propToCNF_ (Not (If p q)) = propToCNF (And [p, Not q])
        propToCNF_ (Not (Iff p q)) = propToCNF (Or [Not $ If p q, Not $ If q p])
        -- implication elim
        propToCNF_ (If p q) = propToCNF (Or [Not p, q])
        propToCNF_ (Iff p q) = propToCNF (And [Or [Not p, q], Or [p, Not q]])
        
        -- remove clauses with contradictory literals 
        -- then remove duplicate clauses
        filterClauses = removeContraClauses
        removeContraClauses = filter (\c -> not $ any (flip contraLitIn c) c)

type Model = [(String, Bool)]

resolution :: CNF -> Maybe Model
resolution cnf
  -- empty clause = unsat
  | any (\c -> length c == 0) cnf' = Nothing
  -- no new clauses = sat
  | all (flip elem cnf) cnf' = Just $ extractModel $ nub $ cnf' >>= id
  | otherwise = resolution cnf'

  where cnf' = if length cnf > 1 then map (uncurry resolve) $ pairs cnf else cnf
        pairs []         = []
        pairs (x:xs)     = map ((,) x) xs ++ pairs xs
        resolve x y      = nub $ removeContra $ x ++ y
        removeContra cnf = filter (\c -> not $ c `contraLitIn` cnf) cnf
        -- all literals are guaranteed to be pure since we stop resolution
        extractModel []            = []
        extractModel ((LSym s):xs) = (s,True):(extractModel xs)
        extractModel ((LNot s):xs) = (s,False):(extractModel xs)


main = do
  let p = propToCNF $ ((Sym "P") `Iff` (Sym "Q")) `If` (Or [Sym "R", Sym "S"])
  let p2 = propToCNF $ And [Sym "P", Not $ Sym "P"]
  let p3 = propToCNF $ Sym "P"
  putStrLn $ cnfToStr p
  print $ resolution p


