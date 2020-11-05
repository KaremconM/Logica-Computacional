module PL

--Karem Ramos Calpulalpan - 314068583
--Logica Computacional.

where
import Data.Set as S 
import Data.List as L

----Formulas de la PL ----
data PL = Bot           --Constructor para bottom
        | Top           --Constructor para top
        | Var Variable  --Constructor de variables
        | Imp PL PL     --Constructor de implicaciones
        | Dis PL PL     --Constructor de disyunciones
        | Con PL PL     --Constructor de conjunciones
        | Neg PL        --Constructor de negaciones
        deriving (Eq,Show)

--- Ejercicio 1 ---
--Cadenas: 
type Variable = String
--Números:
type Variable2 = Int
data Variable3 = X | V Variable3 deriving (Eq,Show)

--- Ejercicio 2 ---
--Valuaciones:
type Valuacion = Variable -> Int
type Valuacion2 = Variable -> Bool
type Valuacion3 = [(Variable,Bool)]
type Valuacion4 = [Variable]     -- Los elementos de sigma son las variables verdaderas
type Valuacion5 = S.Set Variable -- Los elementos de sigma son las variables verdaderas
type Valuacion6 a= a -> Int
-- Ejemplos de valuaciones:
-- sigma1 :: Valuacion
-- sigma1 x= case x of
--            "x0"    -> 1
--             "x1"    -> 0
--             _       -> error $ "sigma1 no esta definida para la variable "++(show x)
--
-- sigma2 :: Valuacion
-- sigma2 _ = 0
--
-- sigma3 :: Valuacion
-- sigma3 x= case x of
--             "x0"    -> 0
--             _       -> 1

--- Ejercicio 3 ---
varList :: PL  ->[Variable]
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

--- Ejercicio 4 ---
cuentaCon :: PL -> Int
cuentaCon phi = case phi of
            Bot             -> 0
            Top             -> 0
            Var _           -> 0
            Imp alpha beta  -> (cuentaCon alpha)+ cuentaCon(beta)
            Dis alpha beta  -> (cuentaCon alpha)+ cuentaCon(beta)
            Con alpha beta  -> (cuentaCon alpha)+ cuentaCon(beta) + 1
            Neg alpha       -> cuentaCon alpha

--instance Show PL where
--	show Bot = "False"
--	show Top = "True"
--	show (Var x) = x
--	show (Neg alpha) = "¬(" ++  show alpha ++ ")"
--	show (Dis alpha beta) = "(" ++  show alpha ++ ") v (" ++  show beta ++ ")"
--	show (Con alpha beta) = "(" ++  show alpha ++ ") ∧ (" ++  show beta ++ ")"
--	show (Imp alpha beta) = "(" ++  show alpha ++ ") -> (" ++  show beta ++ ")"

--- Ejercicio 5 ---
--quitaAnd :: PL -> PL
--quitaAnd phi = case phi of
--            Bot             -> Bot
--            Top             -> Top
--            Var _           -> phi
--            Imp alpha beta  -> not(quitaAnd alpha) v quitaAnd(beta)
--            Dis alpha beta  -> not(not(quitaAnd alpha) v not(quitaAnd(beta))
--            Con alpha beta  -> (quitaAnd alpha) v quitaAnd(beta) 
--            Neg alpha       -> not quitaAnd(alpha)

--- Ejercicio 6 ---
--lNor :: PL -> PL
--lNor phi = case phi of
--            Bot             -> Bot
--            Top             -> Top
--            Var _           -> phi
--            Imp alpha beta  -> lNor(alpha) + lNor(beta)
--            Dis alpha beta  -> (lNor alpha) v lNor(beta)
--            Con alpha beta  -> (lNor alpha) ∧ lNor(beta) 
--            Neg alpha       -> lNor alpha

--- Ejercicio 7 ---
data Modelo = Boton           --Constructor para bottom
        | Topp           --Constructor para top
        | Variables Variable  --Constructor de variables
        | Implicacion PL PL     --Constructor de implicaciones
        | Disyuncion PL PL     --Constructor de disyunciones
        | Conguncion PL PL     --Constructor de conjunciones
        | Negacion PL        --Constructor de negaciones
        deriving (Eq,Show)

mSatisface :: Modelo -> PL -> Bool 
mSatisface m phi = case phi of 
    -- Casos base:
    Bot               -> False     
    Top               -> True      
    --Var x             -> x `elem` m 
    Imp alpha beta    -> not(mSatisface m alpha) || (mSatisface m beta) 
    Dis alpha beta    -> (mSatisface m alpha) || (mSatisface m beta) 
    Con alpha beta    -> (mSatisface m alpha) && (mSatisface m beta) 
    Neg alpha         -> not (mSatisface m alpha) 

--Extra
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