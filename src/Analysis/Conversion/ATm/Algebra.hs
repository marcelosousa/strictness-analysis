-------------------------------------------------------------------------------
-- Module    :  Analysis.Conversion.ATm.Algebra
-- Copyright :  (c) 2011 Marcelo Sousa, Alessandro Vermeulen, Thijs Alkemade

module Analysis.Conversion.ATm.Algebra where
                           
import Language.Tm.Syntax (Operation(..), Ident) 
import Language.ATm.Syntax
import Analysis.Inferencing.ATm.HMTy

type ATmAlgebra r = ( AFunI     -> r      -> TySch -> r -- AFun
                    , ADecl     -> r      -> TySch -> r -- ALet
                    , Integer             -> TySch -> r -- ANum
                    , Ident               -> TySch -> r -- AVar
                    ,                        TySch -> r -- ABot
                    , Operation -> r -> r -> TySch -> r -- AOp
                    , r         -> r      -> TySch -> r -- AApp
                    , r         -> r      -> TySch -> r -- AApp_
                    )
foldATm :: ATmAlgebra r -> ATm -> r
foldATm (fun , flet , num , var , bot , op, app , app_) = f
  where f ( AFun i a   ty) = fun i (f a)      ty
        f ( ALet d a   ty) = flet d (f a)     ty
        f ( ANum i     ty) = num i            ty
        f ( AVar i     ty) = var i            ty
        f ( ABot       ty) = bot              ty
        f ( AOp  o l r ty) = op o (f l) (f r) ty
        f ( AApp   l r ty) = app  (f l) (f r) ty
        f ( AApp_  l r ty) = app_ (f l) (f r) ty