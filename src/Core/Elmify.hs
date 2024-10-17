{-# LANGUAGE OverloadedStrings #-}

module Core.Elmify
  ( elmify,
  )
where

import Control.Exception (bracket)
import Core.Substitution
import Core.Syntax
import Fun.Syntax (BinOp (..), Ctor (..), Dtor (..))
import Prettyprinter
import Prettyprinter.Render.String

elmify :: Program () -> Doc ann
elmify (MkProg defs) = vcat (showDef <$> defs)

-- params :: (Pretty a) => [a] -> Doc ann
-- params :: [a] -> (a -> Doc ann1) -> Doc ann2
-- params :: [a] -> (a -> Doc ann) -> Doc ann
params :: (a -> Doc ann) -> [a] -> Doc ann
params f x =
  --   brackets (punctuate comma (map f x))
  brackets (hsep (punctuate comma (map f x)))

showDef :: Def b -> Doc ann
showDef (Def name pargs cargs body) =
  let args x =
        hsep (punctuate comma (pretty . fst <$> x))
   in -- pretty name <+> "= " <>
      "Def "
        <> dquotes (pretty name)
          <+> brackets (args pargs)
          <+> brackets (args cargs)
          <+> "<|"
          <+> softline
          <+> showStatement body

showStatement :: Statement -> Doc ann
showStatement body =
  case body of
    Cut p c -> "Cut" <+> (showP p) <+> showC c
    Fun nm pargs cargs -> "Call" <+> dquotes (pretty nm) <+> (params showP pargs) <+> (params showC cargs)
    IfZ p1 s1 s2 -> "IfZ" <+> showP p1 <+> showS s1 <+> showS s2
    Op p1 op p2 c -> "BinOp" <+> dquotes (showOp op) <+> showP p1 <+> showP p2 <+> showC c
    Done -> "Halt"

-- _ ->
--   pretty (show body)

showOp :: BinOp -> Doc ann
showOp Prod = "*"
showOp Sum = "+"
showOp Sub = "-"

showP :: Producer -> Doc ann
showP p = parens $
  case p of
    Var v -> "PVar" <+> pretty v
    Lit n -> "Literal (IntNum " <> pretty n <> ")"
    MuDyn v s -> "Mu" <+> pretty v <+> showS s
    Mu v s -> "Mu" <+> pretty v <+> showS s
    Constructor ct pargs cargs -> "Constructor" <+> (showCtor ct) <+> params showP pargs <+> params showC cargs
    Cocase patterns -> "CoCase" <+> params (showPattern showCtor) patterns

-- _ -> pretty (show p)

showS :: Statement -> Doc ann
showS s = parens $ (showStatement s)

showC :: Consumer -> Doc ann
showC c =
  parens $
    case c of
      Covar v -> "CVar" <+> pretty v
      MuTilde v s -> "CMu" <+> pretty v <+> showS s
      MuTildeDyn v s -> "CMu" <+> pretty v <+> showS s
      Case patterns -> "Case" <+> params (showPattern showCtor) patterns
      Destructor ct pargs cargs -> "Destructor" <+> (showCtor ct) <+> params showP pargs <+> params showC cargs

--   _ ->
--     pretty (show c)

showPattern :: (a -> Doc ann) -> Pattern a -> Doc ann
showPattern f (MkPattern {xtor = nm, patv = vars, patcv = covars, patst = st}) =
  parens $ f nm <+> comma <+> params pretty vars <+> comma <+> params pretty covars <+> comma <+> (showS st)

showCtor c = dquotes $ pretty (show c)