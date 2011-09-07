{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.ByteString.Lazy.Char8 as B
import System.Console.CmdArgs
import System.Exit

import Text.WikiEngine

import qualified Text.Blaze.Renderer.Utf8 as RenderUtf8 (renderHtml)
import qualified Text.Blaze.Renderer.Pretty as RenderPretty (renderHtml)

import qualified Data.ByteString.Lazy as L

renderCfg = defaultRenderCfg { rcfgCodeRenderType = CodeRenderSimple }

doMain (Render input pretty) = do
	content <- readFile input
	let wikiblocks =
		case parseDocument content of
			Right blocks -> blocks
			Left  errors -> error ("error parsing wiki content: " ++ show errors)
	if pretty
		then putStrLn $ RenderPretty.renderHtml $ renderAsHtml renderCfg wikiblocks
		else L.putStrLn $ RenderUtf8.renderHtml $ renderAsHtml renderCfg wikiblocks

doMain (Raw input) = do
	content <- readFile input
	let wikiblocks =
		case parseDocument content of
			Right blocks -> blocks
			Left  errors -> error ("error parsing wiki content: " ++ show errors)
	mapM_ (putStrLn . show) wikiblocks

doMain (Validate input) = do
	content <- readFile input
	case parseDocument content of
		Right _ -> exitSuccess
		Left  _ -> exitFailure

data Opts =
	  Render   { input :: FilePath, pretty :: Bool }
	| Raw      { input :: FilePath }
	| Validate { input :: FilePath }
	deriving (Show,Data,Typeable)

render   = Render { input = def &= typFile, pretty = def }
raw      = Raw { input = def &= typFile }
validate = Validate { input = def &= typFile }

mode = cmdArgsMode $ modes [raw,validate,render]

main = cmdArgsRun mode >>= doMain
