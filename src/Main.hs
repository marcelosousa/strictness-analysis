{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Data     ( Data     )  
import Data.Typeable ( Typeable )  

------

import System.Console.CmdArgs ( cmdArgsMode , summary , def , help , typ , (&=) , cmdArgsRun , args , Default(..))
import Control.Monad.Fix (fix)
------

import Language.Parser   ( parser )
import Language.Parsing  ( parse  ) 
import Language.Scanner  ( scanner )
import Language.Scanning ( scan    )

import Language.Tm.Syntax
import Language.Tm.Printing
import Language.ATm.Syntax
import Language.ATm.Printing

--import Analysis.Conversion.ATm.Catas
import Language.Eval                                  
import Analysis.Inferencing.ATm.HM
import Analysis.Inferencing.ATm.HMTy
import Analysis.Conversion.ATm.Catas as ATm ( pFold )

import Analysis.Inferencing.HTm.HH as HTm   ( pFold )

import qualified CCO.Printing as PP
  
data ProgramOptions = SoftType {
    input  :: FilePath,
    mode   :: ProgramMode
  }
  deriving (Show, Data, Typeable)

data ProgramMode = Interpreter | PrettyPrint | Transform | Debug
  deriving (Show, Data, Typeable)
  
instance Default ProgramMode where
  def = Debug

standard = cmdArgsMode $ SoftType 
           { 
             -- output     = (def &= help "Output file") &= typFile
             input      = def &= args,
             mode       = (def &= help "Program Mode") &= typ "Mode"
           -- , typeoutput = (def &= help "Graph|Plain|Debug" &= typ "Graph")
           } &= summary usage


-- interpreter = fix $ \loop env -> do
--               expr <- getLine
--               case expr of
--                    ":q"   -> print "Bye"
--                    source -> let res = (parse parser . scan scanner) source 
--                              in case res of Right r -> let pfr = ATm.pFold r
--                                                            (val, nenv) = evalProg pfr env
--                                                        in  do print r
--                                                               print pfr
--                                                               print val
--                                                               loop env                                                    
--                                             Left  e -> error $ show e

{--
--main = interpreter initFunEnv
--}

main = do args <- cmdArgsRun standard
          case mode args of
            PrettyPrint -> undefined
            Transform   -> undefined
            Interpreter -> interpreter initFunEnv
            _           -> debug args
            

-- | Do everything and test everything.            
debug args = do source <- (readFile . input) args
                let res = (parse parser . scan scanner) source 
                case res of Right r -> do print r
                                          let (constraints, original, term, term2, nt) = HTm.pFold r
                                          putStrLn "Pretty printed term:"
                                          putStrLn $ PP.render_ 80 $ PP.pp term
                                          putStrLn "Pretty printed term after applying transformation twice (should be the same):"
                                          putStrLn $ PP.render_ 80 $ PP.pp term2
                                          if term == term2 then putStrLn "Equal, yay! ☺" else putStrLn "Not equal ☹"

                                          putStrLn $ "Eval original: " ++ show (fst (evalProg r initFunEnv))
                                          putStrLn $ "Eval transformed: " ++ show (fst (evalProg nt initFunEnv))                                          
                                          if fst (evalProg r initFunEnv) == fst (evalProg nt initFunEnv) then putStrLn "Equal eval, yay! ☺" else putStrLn "Not equal eval ☹" 
                            Left  e -> error $ show e
                            
-- | Only do the transformation, print out the nice version of the code :-)
prettyprint args = do source <- (readFile . input) args
                      let res = (parse parser . scan scanner) source
                      case res of Right r -> do print r
                                                let (constraints, original, term, term2, nt) = HTm.pFold r
                                                putStrLn "Pretty printed term:"
                                                putStrLn $ PP.render_ 80 $ PP.pp term
                                                putStrLn "Pretty printed term after applying transformation twice (should be the same):"
                                                putStrLn $ PP.render_ 80 $ PP.pp term2
                                                putStrLn $ "Eval original: " ++ show (fst (evalProg r initFunEnv))
                                                putStrLn $ "Eval transformed: " ++ show (fst (evalProg nt initFunEnv))
                                                if fst (evalProg r initFunEnv) == fst (evalProg nt initFunEnv) then putStrLn "Equal eval, yay! ☺" else putStrLn "Not equal eval ☹" 
                                  Left  e -> error $ show e
                      
interpreter args = undefined

--------------------------------------------------------------------------------
-- Helper function for testing parses
tryParse f = do source <- readFile f
                return $ (parse parser . scan scanner) source

usage :: String
usage = unlines ["The soft-typing team"]

ift = If (Op Eq (Var "x") (Num 0)) (Num 1) (App (Var "fact") (Op Sub (Var "x") (Num 1)))
tt = Let (Name "factz" (NFun "fact" "x" ift)) (App (Var "factz") (Num 1))