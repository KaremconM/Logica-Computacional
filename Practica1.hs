-- Pŕactica 1. 
-- Karem Ramos Calpulalpan - 314068583.

module Practica1

where

import Data.Set as S 
import Data.List as L

-- Ejercicio 1: Variables representadas como cadenas. --
type Variable = String
-- Ejemplos: *Practica1> "Hola Mundo." ---> "Hola Mundo."
--           *Practica1> "Adiós." ---> "Adiós."
--           *Practica1> "Karem." ---> "Karem."


-- Ejercicio 2: Definicion de valuaciones como funciones. --
type Valuacion = Variable -> Int
funcion :: Valuacion
funcion x = case x of
            "variable_1"    -> 1
            "variable_2"    -> 0
            _               -> error $ "La función no está definida para la variable "++(show x)
-- Ejemplos: *Practica1> funcion "variable_1" ---> 1
--           *Practica1> funcion "variable_2" ---> 0
--           *Practica1> funcion "variable_5" ---> *** Exception: La función no está definida para la variable "variable_5"


-- Ejercicio 3: Implementa una función que reciba una fórmula y regrese el conjunto o lista, 
-- de las variables usadas en la formula.--
varList :: PL -> [Variable]
varList (formula) = quita (auxvarList formula)

auxvarList :: PL  ->[Variable]
auxvarList (Var v) = [v]
auxvarList (Neg formula) = (auxvarList formula)
auxvarList (Con alfa beta) = (auxvarList alfa) ++ (auxvarList beta)
auxvarList (Dis alfa beta) = (auxvarList alfa) ++ (auxvarList beta)
auxvarList (Imp alfa beta) = (auxvarList alfa) ++ (auxvarList beta)
auxvarList _ = []
 
quita ::(Eq a) => [a] -> [a]
quita [] = []
quita (x:xs) = x:quita (L.filter(/= x) xs)
-- Ejemplos: *Practica1> varList (Con (Var "x") (Dis (Var "d") (Var "d"))) ---> ["x","d"]
--           *Practica1> varList (Var "l") ---> ["l"]
--           *Practica1> varList (Neg (Var "w")) ---> ["w"]


-- Ejercicio 4: Una función que devuelva el número de apariciones de conjunciones en la fórmula. --
-- Fórmulas de la PL. --
data PL = Bot           --Constructor para bottom
        | Top           --Constructor para top
        | Var Variable  --Constructor de variables
        | Imp PL PL     --Constructor de implicaciones
        | Dis PL PL     --Constructor de disyunciones
        | Con PL PL     --Constructor de conjunciones
        | Neg PL        --Constructor de negaciones
        deriving (Eq,Show)

cuentaCon :: PL -> Int
cuentaCon phi = case phi of
            Bot             -> 0
            Top             -> 0
            Var _           -> 0
            Imp alpha beta  -> (cuentaCon alpha) + cuentaCon(beta)
            Dis alpha beta  -> (cuentaCon alpha) + cuentaCon(beta)
            Con alpha beta  -> (cuentaCon alpha) + cuentaCon(beta) + 1
            Neg alpha       -> cuentaCon alpha
-- Ejemplos: *Practica1> cuentaCon (Con (Con (Var "x") (Var "y")) (Con (Var "k") (Var "j"))) ---> 3
--           *Practica1> cuentaCon Bot ---> 0
--           *Practica1> cuentaCon (Dis (Var "f") (Var "o")) ---> 0


-- Ejercicio 5: Implementa el algoritmo quitaImp, que recibe una fórmula de la LP y regresa una fórmula  
-- de la LP sin apariciones del operador implicación. --
quitaImp :: PL -> PL
quitaImp phi = case phi of 
            Bot             -> Bot   
            Top             -> Top   
            Var x           -> Var x 
            Imp alpha beta  -> (Neg(quitaImp alpha)) `Dis` (quitaImp(beta))  
            Dis alpha beta  -> (quitaImp alpha)      `Dis` (quitaImp(beta))       
            Con alpha beta  -> (quitaImp alpha)      `Con` quitaImp(beta)         
            Neg alpha       -> (Neg(quitaImp alpha))
-- Ejemplos: *Practica1> quitaImp Bot ---> Bot
--           *Practica1> quitaImp ((Var "k") `Imp` (Var "j")) ---> Dis (Neg (Var "k")) (Var "j")
--           *Practica1> quitaImp ((Var "c") `Con` (Var "m")) ---> Con (Var "c") (Var "m")


-- Ejercicio 6: Implementa el algoritmo lNor que recibe una fórmula de la LP y regresa una fórmula en 
-- donde sólo aparece el operador nor. --
lNor :: PL -> PL
lNor phi = case phi of
            Bot            -> Bot
            Top            -> Top
            Var x          -> Var x
            Imp alpha beta -> ((Neg(lNor alpha)) `Dis` (lNor beta))
            Dis alpha beta -> ((lNor alpha)      `Dis` (lNor beta))
            Con alpha beta -> (Neg(lNor alpha)   `Dis` Neg(lNor beta))
            Neg alpha      -> Neg(lNor alpha)
-- Ejemplos: *Practica1> lNor Bot ---> Bot
-- 			 *Practica1> lNor ((Var "a") `Con` (Var "e")) ---> Dis (Neg (Var "a")) (Neg (Var "e"))
-- 			 *Practica1> lNor ((Var "o") `Imp` (Var "r")) ---> Dis (Neg (Var "o")) (Var "r")


-- Ejercicio 7: Define la función mSatisface :: Modelo-> PL -> Bool, que reciba un modelo y una fórmula
-- de la LP y regrese si el modelo satisface o no a la fórmula. --
type Modelo = [Variable]
mSatisface :: Modelo -> PL -> Bool 
mSatisface m phi = case phi of 
    Bot               -> False     
    Top               -> True
    Var x             -> x `elem` m    
    Imp alpha beta    -> not(mSatisface m alpha) || (mSatisface m beta) 
    Dis alpha beta    -> (mSatisface m alpha)    || (mSatisface m beta) 
    Con alpha beta    -> (mSatisface m alpha)    && (mSatisface m beta) 
    Neg alpha         -> not (mSatisface m alpha) 
-- Ejemplos: *Practica1> mSatisface ["l"] Top ---> True
--           *Practica1> mSatisface ["k"] ((Var "g") `Dis` (Var "y")) ---> False
--           *Practica1> mSatisface [] Top ---> True


-- Extra --
--Función cY, que cuente el numero de operadores "and" en phi.
--Por ejemplo, cY (Dis Top Bot)= 0, cY (Con Bot Top)=1 
cY :: PL -> Int
cY phi = case phi of
            Bot             -> 0
            Top             -> 0
            Var _           -> 0
            Imp alpha beta  -> (cY alpha)+ cY(beta)
            Dis alpha beta  -> (cY alpha)+ cY(beta)
            Con alpha beta  -> (cY alpha)+ cY(beta) + 1
            Neg alpha       -> cY alpha