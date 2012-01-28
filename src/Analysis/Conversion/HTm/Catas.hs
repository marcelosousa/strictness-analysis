-------------------------------------------------------------------------------
-- Module    :  Analysis.Conversion.HTm.Catas
-- Copyright :  (c) 2011 Marcelo Sousa, Alessandro Vermeulen, Thijs Alkemade

module Analysis.Conversion.HTm.Catas where

import Language.HTm.Syntax  
import Language.Tm.Syntax  
                                                                                   
-- | Conversion function from UTm to Tm
unUTm :: UTm -> Tm
unUTm (UTm UBot _)            = Bot
unUTm (UTm (UNum i) _)        = Num  i
unUTm (UTm (UBool b) _)       = Bool b
unUTm (UTm (UVar iden) _)     = Var  iden
unUTm (UTm (UFun iden utm) _) = Fun  iden           (unUTm utm)
unUTm (UTm (UNFun f i utm) _) = NFun f i            (unUTm utm)
unUTm (UTm (UApp t1 t2) _)    = App  (unUTm t1)     (unUTm t2)
unUTm (UTm (UApp_ t1 t2) _)   = App_ (unUTm t1)     (unUTm t2)
unUTm (UTm (ULet udec utm) _) = Let  (unUDecl udec) (unUTm utm)
unUTm (UTm (UOp op t1 t2) _)  = Op   op             (unUTm t1)  (unUTm t2)
unUTm (UTm (UIf g  t1 t2) _)  = If   (unUTm g)      (unUTm t1)  (unUTm t2)

unUDecl :: UDecl -> Decl
unUDecl (UName name utm) = Name name (unUTm utm)
unUDecl (UVal name utm)  = Val name (unUTm utm)
