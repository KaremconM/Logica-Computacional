--Práctica 2.
--Karem Ramos Calpulalpan - 314068583. 

module Practica2

where

import Data.List as S

data PL  = Bot               -- Constructor para bot
         | Top               -- Constructor para top
         | Var Variables     -- Constructor de variables
         | Imp PL PL         -- Constructor de la implicación
         | Dis PL PL         -- Constructor de la disyunción
         | Con PL PL         -- Constructor de la conjunción
         | Neg PL            -- Constructor de la negación
          deriving (Eq,Show)

 
type Variables = String 

-- Regresa una lista de variables de fórmulas en la LP.
-- Ejemplos: *Practica2> listVar Bot ---> []
--           *Practica2> listVar (Var "x") ---> ["x"]
listVar :: PL -> [Variables]
listVar phi = case phi of
            Bot             -> []
            Top             -> []
            Var x           -> [x]
            Imp alpha beta  -> (listVar alpha) `S.union` listVar(beta)
            Dis alpha beta  -> (listVar alpha) `S.union` listVar(beta)
            Con alpha beta  -> (listVar alpha) `S.union` listVar(beta)
            Neg alpha       -> (listVar alpha)


-- Si phi en PL es una conjunción de literales,
-- entonces conLit2ListLit transforma phi en una lista de literales.
-- Ejemplos: *Practica2> conLit2ListLit ((Var "x") `Con` (Var "y")) ---> [Var "x",Var "y"]
--           *Practica2> conLit2ListLit ((Var "x") `Dis` (Var "y")) ---> *** Exception: conLit2ListLit: phi no es una conjunción 
--                                                                       de literales, phi = Dis (Var "x") (Var "y")
conLit2ListLit :: PL -> [PL]
conLit2ListLit phi = case phi of
            Top               -> []
            Bot               -> []
            Var x             -> [Var x]
            Neg (Var x)       -> [Neg (Var x)]
            (Con alpha beta)  -> (conLit2ListLit alpha) ++ (conLit2ListLit beta)
            _                 -> error $ "conLit2ListLit: phi no es una conjunción de literales, phi = "++(show phi)


-- Dado un literal l en PL, litComp calcula el literal complementario de l.
-- Ejemplos: *Practica2> litComp (Neg(Var "x")) ---> Var "x"
--           *Practica2> litComp (Var "x") ---> Neg (Var "x") 
--           *Practica2> litComp (Neg(Neg(Var "x"))) ---> *** Exception: litComp: phi no es literal, phi = Neg (Neg (Var "x"))
litComp :: PL -> PL
litComp phi = case phi of
            Var x       -> Neg (Var x)
            Neg (Var x) -> Var x
            _           -> error $ "litComp: phi no es literal, phi = "++(show phi)


-- Dada una término de PL, representada por una lista de literales ll,
-- terminoEnSAT determina si phi es una termino satisfactible.
-- Ejemplos: *Practica2> terminoEnSAT [Var "z"] ---> True
--           *Practica2> terminoEnSAT [] ---> True
--           *Practica2> terminoEnSAT [Var "l", (Neg(Var "l"))] ---> False

terminoEnSAT :: [PL] -> Bool
terminoEnSAT phi = case phi of
            []     -> True
            (l:ls) -> (litComp l) `notElem` phi && terminoEnSAT ls


-- Dada phi en PL, dnf2LListLit transforma phi a una fórmula phi' en DNF,
-- donde phi' está representada como una lista de listas de literales.
-- Ejemplos: *Practica2> dnf2LListLit ((Var "k") `Dis` (Var "j")) ---> [[Var "k"],[Var "j"]] 
--           *Practica2> dnf2LListLit (Neg Top) ---> *** Exception: dnf2LListLit: phi no esta en DNF, phi = Neg Top
dnf2LListLit :: PL -> [[PL]]
dnf2LListLit phi = case phi of
            Bot              -> [[]]
            Top              -> [[]]    
            Var x            -> [[Var x]]
            Neg (Var x)      -> [[Neg (Var x)]]
            (Con _ _)        -> [conLit2ListLit phi]
            (Dis alpha beta) -> (dnf2LListLit alpha) ++ (dnf2LListLit beta)
            _                -> error $ "dnf2LListLit: phi no esta en DNF, phi = "++(show phi)


-- Dada phi en DNF, representada como una lista de listas de literales phi,
-- terminoListEnSAT determina si los términos de phi son satisfactibles.
-- Ejemplos: *Practica2> terminoListEnSAT [[Var "w"]] ---> True
--           *Practica2> terminoListEnSAT [[(Neg(Var"d"))]] ---> True
--           *Practica2> terminoListEnSAT [[Bot]] ---> *** Exception: litComp: phi no es literal, phi = Bot
terminoListEnSAT :: [[PL]] -> Bool
terminoListEnSAT phi = case phi of
            []     -> False
            (l:ls) -> terminoEnSAT l || terminoListEnSAT ls


-- Dada phi en PL, decide si phi pertenece, o no, a SAT := {phi in PL | Existe m : m |= phi}.
-- Esto se hace transformando primero phi a una fórmula en DNF representada mediante una lista de listas de literales,
-- y luego aplicando terminoListEnSAT a dicha lista. 
-- Ejemplos: *Practica2> decideDNFenSAT ((Var "x") `Dis` (Neg(Var "f"))) ---> True
--           *Practica2> decideDNFenSAT ((Var "x") `Con` (Neg(Var "x"))) ---> False
--           *Practica2> decideDNFenSAT Bot ---> True
decideDNFenSAT :: PL -> Bool
decideDNFenSAT phi = terminoListEnSAT (dnf2LListLit phi)