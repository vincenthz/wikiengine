{-# LANGUAGE OverloadedStrings #-}
module Text.WikiEngine.HTML
	( render
	) where

import Text.WikiEngine.Type
import Text.WikiEngine.Configuration
import qualified Text.WikiEngine.Enlightened as E

import Text.Blaze.Html4.Strict hiding (map, string, stringValue)
import qualified Text.Blaze.Html4.Strict.Attributes as A

import Control.Monad.Reader
import Prelude hiding (span)

mapColorType :: String -> String
mapColorType t = maybe "black" id $ lookup t
	[ ("pre_keyword" , "purple")
	, ("pre_operator", "yellow")
	, ("pre_number"  , "red")
	, ("pre_string"  , "red")
	, ("pre_char"    , "red")
	, ("pre_comment" , "blue")
	]

render :: RenderCfg -> [Block] -> Html
render cfg blk = mapM_ (renderBlockNewline cfg) blk

renderAtom :: RenderCfg -> Either String [E.Atom] -> Html
renderAtom _   (Left c)      = toHtml c
renderAtom cfg (Right atoms) = forM_ atoms (uncurry withTy)
	where
		spanF = case rcfgCodeRenderType cfg of
			CodeRenderNone   -> \_ s -> toHtml s
			CodeRenderCSS    -> spanClass
			CodeRenderSimple -> \t s -> spanColor (mapColorType t) s
		withTy E.Symbol   s = toHtml s
		withTy E.Keyword  s = spanF "pre_keyword" s
		withTy E.Operator s = spanF "pre_operator" s
		withTy E.Number   s = spanF "pre_number" s
		withTy E.String   s = spanF "pre_string" s
		withTy E.Char     s = spanF "pre_char" s
		withTy E.Comment  s = spanF "pre_comment" s
		withTy E.Other    s = toHtml s

renderBlockNewline :: RenderCfg -> Block -> Html
renderBlockNewline cfg blk = renderBlock cfg blk >> newline
	where
		newline :: Html
		newline = "\n"

{-- render block structure --}
renderBlock :: RenderCfg -> Block -> Html

renderBlock cfg (Paragraph l)  = p $ forM_ l (renderInline cfg)
renderBlock cfg (Quote l)      = blockquote $ p $ forM_ l (mapM_ (renderInline cfg))
renderBlock cfg (UList l)      = ul $ forM_ l (li . mapM_ (renderInline cfg))
renderBlock cfg (OList l)      = ol $ forM_ l (li . mapM_ (renderInline cfg))
renderBlock _   HRule          = hr
renderBlock cfg (Header 1 l)   = h1 $ forM_ l (renderInline cfg)
renderBlock cfg (Header 2 l)   = h2 $ forM_ l (renderInline cfg)
renderBlock cfg (Header 3 l)   = h3 $ forM_ l (renderInline cfg)
renderBlock cfg (Header 4 l)   = h4 $ forM_ l (renderInline cfg)
renderBlock cfg (Header 5 l)   = h5 $ forM_ l (renderInline cfg)
renderBlock cfg (Header _ l)   = h6 $ forM_ l (renderInline cfg)

renderBlock cfg (Table rows)   =
	let z = zip rows (if rcfgTableRenderAlt cfg then cycle [False,True] else cycle [False]) in
	table $ forM_ z (\(r, alt) -> renderTableRow cfg alt r)

renderBlock _   (Code Nothing content) = pre $ toHtml content
renderBlock cfg (Code (Just ty) content)
	| ty == "C"       = pre $ renderAtom cfg $ E.atomizeC content
	| ty == "haskell" = pre $ renderAtom cfg $ E.atomizeHaskell content
	| ty == "sh"      = pre $ renderAtom cfg $ E.atomizeShell content
	| otherwise       = pre $ toHtml content
	

{-- render block helper --}
renderTableRow :: RenderCfg -> Bool -> [[Inline]] -> Html
renderTableRow cfg True  row = tr ! A.class_ "alt" $ forM_ row (\x -> td $ forM_ x (renderInline cfg))
renderTableRow cfg False row = tr $ forM_ row (\x -> td $ forM_ x (renderInline cfg))

{-- render inline structure --}

renderInline :: RenderCfg -> Inline -> Html

renderInline _   (Str s)         = toHtml s
renderInline _   (White s)       = toHtml s
renderInline cfg (Strong l)      = b $ renderInline cfg l
renderInline _   (Quoted s)      = toHtml $ concat [ "\"", s, "\""]
renderInline _   (Symbol c)      = toHtml [c] 
renderInline _   (Escape x)      = maybe (toHtml x) (preEscapedString) $ lookup x textEscape
renderInline _   (EscapeNum x)   = preEscapedString ("&#" ++ show x ++ ";")
renderInline _   (Punctuation c) = toHtml [c]
renderInline cfg (Link s)        = aref cfg s
renderInline _   Apostrophe      = toHtml ("'" :: String)

textEscape :: [(String, String)]
textEscape =
	[ ("forall", "∀")
	, ("exists", "∃")
	, ("nexists", "∄")
	, ("empty", "∅")
	, ("not", "¬")
	, ("and", "∧")
	, ("or", "∨")
	, ("xor", "⊻")
	, ("nand", "⊼")
	, ("nor", "⊽")
	, ("intersection", "∩")
	, ("union", "∪")
	, ("elementOf", "∈")
	, ("notElementOf", "∉")
	, ("subset", "⊂")
	, ("superset", "⊃")
	, ("notSubset", "⊄")
	, ("notSuperset", "⊅")
	, ("subsetOrEqual", "⊆")
	, ("supersetOrEqual", "⊇")
	, ("alpha", "&#945;")
	, ("beta", "&#946;")
	, ("lambda", "&#955;")
	, ("pi", "π")
	, ("setR", "ℝ")
	, ("setN", "ℕ")
	]

{-- HTML helpers to escape and create tags --}

spanClass :: String -> String -> Html
spanClass cl dat = span ! A.class_ (toValue cl) $ toHtml dat

spanColor :: String -> String -> Html
spanColor cl dat = span ! A.style (toValue ("color: '" ++ cl ++ "'")) $ toHtml dat

aref :: RenderCfg -> [String] -> Html
aref cfg refs =
	case rcfgLinkRendering cfg refs of
		LinkTypeNormal lnk dat   -> a ! A.href (toValue lnk) $ toHtml dat
		LinkTypeImage s (Just x) -> img ! A.src (toValue s) ! A.alt (toValue x)
		LinkTypeImage s Nothing  -> img ! A.src (toValue s)
		LinkTypeCustom s         -> preEscapedString s
