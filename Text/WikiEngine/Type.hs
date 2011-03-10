module Text.WikiEngine.Type
	( Block(..)
	, Inline(..)
	) where

type TableRow = [[Inline]]

data Block =
	  Header Int [Inline]
	| HRule
	| Code (Maybe String) String
	| Table [TableRow]
	| OList [[Inline]]
	| UList [[Inline]]
	| Quote [[Inline]]
	| Paragraph [Inline]
	deriving (Show,Eq)

data Inline =
	  Str String
	| White String
	| Strong Inline
	| Quoted String
	| Apostrophe
	| Punctuation Char
	| Escape String
	| EscapeNum Int
	| Symbol Char
	| Link [String]
	deriving (Show,Eq)
