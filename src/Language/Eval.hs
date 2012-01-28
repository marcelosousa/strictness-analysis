module Language.Eval where

import Analysis.Inferencing.ATm.HMTy (Var)
import Language.Tm.Printing
import Language.Tm.Syntax

import Data.Map as Map

-- Wrapper datatype for evaluating a value
type Value  = Tm
type FunEnv = Map Var Tm

{--
Lots of stuff functions to deal with built-in operations
--}
                        
opTm :: Operation -> Tm
opTm op = Fun "x" $ Fun "y" $ Op op (Var "x") (Var "y") 
   
strToOp :: String -> Operation
strToOp "+"     = Add
strToOp "-"     = Sub
strToOp "*"     = Mul
strToOp "`div`" = Div
strToOp "`mod`" = Mod 
strToOp "=="    = Eq
strToOp "<"     = Lt
strToOp ">"     = Gt
strToOp _       = error "Undefined Operation"

opToFun :: Operation -> Integer -> Integer -> Value
opToFun Add x y = Num (x+y)
opToFun Sub x y = Num (x-y)
opToFun Mul x y = Num (x*y)
opToFun Div x y = Num (x `div` y)
opToFun Mod x y = Num (x `mod` y)
opToFun Eq  x y = Bool (x == y)
opToFun Lt  x y = Bool (x < y)
opToFun Gt  x y = Bool (x > y)
                  
lOp :: [String]
lOp = ["+","-","*","`div`","`mod`", "==", "<", ">"]

-- start with a dictionary of basic arith functions
initFunEnv :: FunEnv 
initFunEnv = foldr (\s m -> Map.insert s (opTm (strToOp s)) m) Map.empty lOp   

{--
Eval function     -- seems to be working but i have not defined with Andres-Algebra way. Quite easy to transform now and get ride of those _ for the TySch
let name partial = fun f => fun x => fun y => x + y in partial _|_ 1 2 end
*Main> :main "../test/simple.fun"
Program: let name: partial = (\f -> (\x -> (\y -> (x + y)))) in (((partial ⊥) 1) 2) ni
Ͱ let name: partial = (\f::a -> (\x::Int -> (\y::Int -> ((+::(Int->(Int->Int)) x::Int)::(Int->Int) y::Int)::Int)::(Int->Int))::(Int->(Int->Int)))::Ɐa.((a->(Int->(Int->Int)))) in (((partial::(Int->(Int->(Int->Int))) ⊥::(Int->Int))::(Int->(Int->Int)) 1::Int)::(Int->Int) 2::Int)::Int ni :: Int
3::Int

--}
evalProg :: Prog -> FunEnv -> (Value, FunEnv)
evalProg (Prog tm) = eval tm

eval :: Tm -> FunEnv -> (Value, FunEnv)
eval t@ Bot              env = (error "⊥",env)
eval t@(Num i      )     env = (t,env)
eval t@(Bool b     )     env = (t,env)
eval t@(Fun afi atm)     env = (t,env)
eval t@(NFun f i tm)     env = (t, Map.insert f (Fun i tm) env)
eval t@(Var var    )     env = case Map.lookup var env of
                                    Nothing -> error $ "Eval.Not in scope: " ++ var
                                    Just t' -> eval t' env
eval t@(App       t1 t2) env = (evalApp t1 t2 env, env)
eval t@(App_      t1 t2) env = let (at2, nenv) = eval t2 env
                               in  (evalApp t1 at2 nenv, nenv)
eval t@(Op  op    t1 t2) env = let (at1, env') = eval t1 env
                                   (at2, nenv) = eval t2 env'
                               in  (evalOp op at1 at2, nenv)
eval t@(Let decl t1)     env = let nop  = did decl
                                   (opTm, env') = eval (dtm decl) env
                                   nenv = Map.insert nop opTm env'
                               in  eval t1 nenv
eval t@(If c th e)       env = let (b, env') = eval c env
                               in case b of
                                       Bool True  -> eval th env'
                                       Bool False -> eval e  env'
                                       x          -> error ("EvalIf.error Should not happen: " ++ show x) 

evalOp :: Operation -> Value -> Value -> Value
evalOp op (Num x) (Num y) = opToFun op x y
evalOp op _       _       = error "EvalOp.error Should not happen"


evalApp :: Tm -> Tm -> FunEnv -> Value
evalApp  Bot            t env = error "⊥"
evalApp (Num  _)        t env = error "EvalApp.error Num Application"
evalApp (Bool _)        t env = error "EvalApp.error Bool Application"
evalApp (Op  op  t1 t2) t env = error "EvalApp.error Op Application"                                
evalApp (Var x)         t env = case Map.lookup x env of
                                     Nothing -> error $ "EvalApp.error Not in scope: " ++ x
                                     Just f  -> evalApp f t env                                 
evalApp (App     t1 t2) t env = evalApp (evalApp t1 t2 env) t env                                   
evalApp (App_    t1 t2) t env = let (at2, nenv) = eval t2 env 
                                in evalApp ((evalApp t1 $! at2) nenv) t nenv   -- because of the pair at2 must be really evaluated
evalApp (If  c t1 t2)   t env = let nif = If c (App t1 t) (App t2 t)
                                in  fst $ eval nif env
evalApp (Fun i t1)      t env = fst $ eval (reduce i t1 t) env
evalApp (NFun f i t1)   t env = fst $ eval (reduce i t1 t) env
evalApp (Let d t1)      t env = let (t1',nenv) = eval (Let d t1) env
                                in  fst $ eval (App t1' t) nenv


reduce :: Ident -> Tm -> Tm -> Tm
reduce s t@ Bot           _  = t
reduce s t@(Num  i)       _  = t
reduce s t@(Bool b)       _  = t
reduce s t@(App    t1 t2) t3 = App   (reduce s t1 t3) (reduce s t2 t3)                              
reduce s t@(App_   t1 t2) t3 = App_  (reduce s t1 t3) (reduce s t2 t3)
reduce s t@(Op  op t1 t2) t3 = Op op (reduce s t1 t3) (reduce s t2 t3)
reduce s t@(If  g  t1 t2) t3 = If (reduce s g t3) (reduce s t1 t3) (reduce s t2 t3)
reduce s t@(Var x )       t2 | s == x    = t2
                             | otherwise = t 
reduce s t@(Fun i     t1) t2 | s == i    = t
                             | otherwise = Fun i $ reduce s t1 t2
reduce s t@(NFun f i  t1) t2 | s == i    = t
                             | otherwise = NFun f i $ reduce s t1 t2
reduce s t@(Let decl  t1) t2 = let aid = did decl
                                   atm = dtm decl
                                   nad = decl { dtm = preventPoisoning s atm t2 }  -- watch out with poisoning
                               in  if s == aid
                                      then t
                                      else Let nad $ reduce s t1 t2
reduce _ _ _ = undefined

{--
Without this function poisoning in the let def could happen  (variable x)
let name t = fun f => fun x => let name id = fun x => x in id (f x) end in let name k = fun a => a + 3 in t k 3 end end
*Main> :main "../test/simple.fun"
Program: let name: t = (\x -> let name: id = (\x -> x) in (id x) ni) in (t 3) ni
Ͱ let name: t = (\x::c -> (let name: id = (\x::a -> x::a)::Ɐa.((a->a)) in (id::(c->c) x::c)::c ni) :: c)::Ɐc.((c->c)) in (t::(Int->Int) 3::Int)::Int ni :: Int
*** Exception: EvalApp.error Num Application

-- now it works
*Main> :main "../test/simple.fun"
Program: let name: t = (\f -> (\x -> let name: id = (\x -> x) in (id (f x)) ni)) in let name: k = (\a -> (a + 3)) in ((t k) 3) ni ni
Ͱ let name: t = (\f::(b->d) -> (\x::b -> (let name: id = (\x::a -> x::a)::Ɐa.((a->a)) in (id::(d->d) (f::(d->d) x::d)::d)::d ni) :: d)::(b->d))::Ɐb.(Ɐd.(((b->d)->(b->d)))) in (let name: k = (\a::Int -> ((+::(Int->(Int->Int)) a::Int)::(Int->Int) 3::Int)::Int)::(Int->Int) in ((t::(Int->(Int->Int)) k::Int)::(Int->Int) 3::Int)::Int ni) :: Int ni :: Int
6::Int
--}                                         
preventPoisoning :: Ident -> Tm -> Tm -> Tm
preventPoisoning s t@(Fun i t1) t2 | s == i = t
                                   | otherwise = Fun i $ reduce s t1 t2
preventPoisoning s t            t2 = reduce s t t2                               
 
