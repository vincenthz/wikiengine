module Text.WikiEngine.Configuration
	( RenderCfg(..)
	, LinkType(..)
	, CodeRenderType(..)
	, defaultRenderCfg
	) where

data LinkType =
	  LinkTypeNormal String String -- ^ url content
	| LinkTypeImage String (Maybe String)
	| LinkTypeCustom String
	deriving (Show,Eq)

data CodeRenderType =
	  CodeRenderNone
	| CodeRenderCSS
	| CodeRenderSimple
	deriving (Show,Eq)

data RenderCfg = RenderCfg
	{ rcfgLinkRendering  :: [String] -> LinkType
	, rcfgCodeRenderType :: CodeRenderType
	, rcfgTableRenderAlt :: Bool
	}

defaultLinkRendering :: [String] -> LinkType
defaultLinkRendering (title:url:_) = LinkTypeNormal url title
defaultLinkRendering (r:[])  = LinkTypeNormal r r
defaultLinkRendering []      = LinkTypeCustom ""

defaultRenderCfg :: RenderCfg
defaultRenderCfg = RenderCfg
	{ rcfgLinkRendering  = defaultLinkRendering
	, rcfgCodeRenderType = CodeRenderNone
	, rcfgTableRenderAlt = True
	}
