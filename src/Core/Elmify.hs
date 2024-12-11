{-# LANGUAGE OverloadedStrings #-}

module Core.Elmify
  ( elmify,
    elmifys,
    writeElmModule,
    showStatement,
  )
where

import Core.Pretty
import Core.Syntax
import Fun.Syntax (BinOp (..))
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    brackets,
    comma,
    dquotes,
    hsep,
    indent,
    parens,
    punctuate,
    softline,
    vcat,
    vsep,
    (<+>),
  )

writeElmModule :: FilePath -> [Doc ann] -> [Doc ann] -> IO ()
writeElmModule filePath defs unfocused = do
  -- Define the preamble and the declaration
  let preambles = ["module Elmified exposing (..)", "import Literal exposing (..)", "import MuMu exposing (..)", "import Unified exposing (..)"]
      declaration = "code ="
      code = vsep (punctuate comma defs)
      unfocusedDec = "unfocused = "
      unfocusedCode = vsep (punctuate comma unfocused)
  -- Indent the core Elm code under the "code =" declaration

  let fullContent = vsep $ preambles ++ [declaration, indent 2 $ brackets code, unfocusedDec, indent 2 $ brackets unfocusedCode]
  -- Render `Doc` to `Text` and write to file
  writeFile filePath (renderDoc fullContent)

  putStrLn $ "Elm code has been written to " ++ filePath

elmifys :: Program () -> [Doc ann]
elmifys (MkProg defs) = (showDef <$> defs)

elmify :: Program () -> Doc ann
elmify (MkProg defs) = vcat (showDef <$> defs)

-- params :: (Pretty a) => [a] -> Doc ann
-- params :: [a] -> (a -> Doc ann1) -> Doc ann2
-- params :: [a] -> (a -> Doc ann) -> Doc ann
params :: (a -> Doc ann) -> [a] -> Doc ann
params f x =
  --   brackets (punctuate comma (map f x))
  brackets (hsep (punctuate comma (map (f) x)))

paramsq :: (a -> Doc ann) -> [a] -> Doc ann
paramsq f x =
  --   brackets (punctuate comma (map f x))
  brackets (hsep (punctuate comma (map (dquotes . f) x)))

showDef :: Def b -> Doc ann
showDef (Def name pargs cargs body) =
  let args x =
        hsep (punctuate comma (dquotes . pretty . fst <$> x))
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

qpretty :: (Pretty a) => a -> Doc ann
qpretty v = dquotes (pretty v)

showP :: Producer -> Doc ann
showP p = parens $
  case p of
    Var v -> "PVar" <+> dquotes (pretty v)
    Lit n -> "Literal (IntNum " <> pretty n <> ")"
    MuDyn v s -> "PMu" <+> qpretty v <+> showS s
    Mu v s -> "PMu" <+> qpretty v <+> showS s
    Constructor ct pargs cargs -> "Constructor" <+> (showCtor ct) <+> params showP pargs <+> params showC cargs
    Cocase patterns -> "CoCase" <+> params (showPattern showCtor) patterns

-- _ -> pretty (show p)

showS :: Statement -> Doc ann
showS s = parens $ (showStatement s)

showC :: Consumer -> Doc ann
showC c =
  parens $
    case c of
      Covar v -> "CVar" <+> dquotes (pretty v)
      MuTilde v s -> "CMu" <+> qpretty v <+> showS s
      MuTildeDyn v s -> "CMu" <+> qpretty v <+> showS s
      Case patterns -> "Case" <+> params (showPattern showCtor) patterns
      Destructor ct pargs cargs -> "Destructor" <+> (showCtor ct) <+> params showP pargs <+> params showC cargs

--   _ ->
--     pretty (show c)

showPattern :: (a -> Doc ann) -> Pattern a -> Doc ann
showPattern f (MkPattern {xtor = nm, patv = vars, patcv = covars, patst = st}) =
  parens $ f nm <+> comma <+> paramsq pretty vars <+> comma <+> paramsq pretty covars <+> comma <+> (showS st)

showCtor :: (Show a) => a -> Doc ann
showCtor c = dquotes $ pretty (show c)