module Text.WikiEngine
	( RenderCfg (..)
	, LinkType(..)
	, CodeRenderType(..)
	, defaultRenderCfg
	-- * parsed types
	, Block(..)
	, Inline(..)
	, parseDocument

	-- * renderer
	, renderAsHtml
	) where

import Text.WikiEngine.Configuration
import Text.WikiEngine.HTML
import Text.WikiEngine.Parse
import Text.WikiEngine.Type

renderAsHtml = render
