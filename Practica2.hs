
--Práctica 2.
--Karem Ramos Calpulalpan - 314068583. 

module Practica2

where

import Data.List as L

----Fórmulas de la PL ----
data PL = Bot             --Constructor para bottom.
        | Top             --Constructor para top.
        | Var PL          --Constructor de variables.
        | Imp PL PL       --Constructor de implicaciones.
        | Dis PL PL       --Constructor de disyunciones.
        | Con PL PL       --Constructor de conjunciones.
        | Neg PL          --Constructor de negaciones.
        deriving (Eq,Show)


-- Si phi en PL es una conjunción de literales,
-- entonces conLit2ListLit transforma phi en una lista de literales.
conLit2ListLit :: PL -> [PL]
conLit2ListLit phi = case phi of
            Top               -> []
            Bot               -> []
            Var x             -> [Var x]
            Neg (Var x)       -> [Neg (Var x)]
            (Con alpha beta)  -> (conLit2ListLit alpha) ++ (conLit2ListLit beta)
            _                 -> error $ "conLit2ListLit: phi no es una conjunción de literales, phi = "++(show phi)


-- Dado un literal l en PL, litComp calcula el literal complementario de l.
litComp :: PL -> PL
litComp phi = case phi of
            Var x       -> Neg (Var x)
            Neg (Var x) -> Var x
            _           -> error $ "litComp: phi no es literal, phi = "++(show phi)


-- Dada una término de PL, representada por una lista de literales ll,
-- terminoEnSAT determina si phi es una termino satisfactible.
terminoEnSAT :: [PL] -> Bool
terminoEnSAT phi = case phi of
            []     -> True
            (l:ls) -> (litComp l) `elem` phi && terminoEnSAT ls


-- Dada phi en PL, dnf2LListLit transforma phi a una fórmula phi' en DNF,
-- donde phi' está representada como una lista de listas de literales.
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
terminoListEnSAT :: [[PL]] -> Bool
terminoListEnSAT phi = case phi of
            []     -> False
            (l:ls) -> terminoEnSAT l || terminoListEnSAT ls

-- Dada phi en PL, decide si phi pertenece, o no, a SAT := {phi in PL | Existe m : m |= phi}.
-- Esto se hace transformando primero phi a una fórmula en DNF representada mediante una lista de listas de literales,
-- y luego aplicando terminoListEnSAT a dicha lista. 
decideDNFenSAT :: PL -> Bool
decideDNFenSAT phi = terminoListEnSAT (dnf2LListLit phi)