module Text.WikiEngine.Parse
	( parseDocument
	) where

import Control.Applicative ((<$>))
import Text.WikiEngine.Type
import Text.Parsec hiding (space, spaces)

type P a = Parsec String () a

parseDocument :: String -> Either ParseError [Block]
parseDocument s = parse document "source" (s ++ "\n\n")

document :: P [Block]
document = many (oneOf "\r\n\t ") >> many block

block :: P Block
block = choice
	[ header
	, hrule
	, table
	, code
	, blockquote
	, ulist
	, olist
	, paragraph
	] <?> "block"

--------------- blocks parsers
header, header1, header2, code, blockquote, table, hrule, ulist, olist, paragraph :: P Block

header = header1 <|> header2 <?> "header"

header1 = try $ do
	level <- length <$> many1 (char '=')
	space
	hdr <- manyTill inline (eol >> optional (many blankline))
	return (Header level hdr)

header2 = try $ do
	text <- manyTill inline newline
	ruletype <- (char '=' <|> char '-')
	_ <- many (char ruletype)
	eob
	return (Header (if ruletype == '=' then 1 else 2) text)

code = try $ do
	_ <- string "%%%"
	ty <- optionMaybe (char '-' >> many1 (noneOf strChar))
	eol
	manyTill anyChar (try (eol >> string "%%%" >> eob)) >>= return . Code ty

blockquote = try $ do
	quotes <- many1 $ do
		char '>' >> optional spaces
		manyTill inline newline
	eob
	return $ Quote quotes

table = try $ do
	tableSep
	rows <- manyTill tableRow (blankline <|> try tableSep)
	return $ Table rows
	where
		tableSep = char '+' >> many1 (char '-') >> char '+' >> eol
		tableRow = do
			row <- char '|' >> manyTill inline eol
			return $ splitOn ((==) (Symbol '|')) row

		splitOn :: (a -> Bool) -> [a] -> [[a]]
		splitOn _ [] = []
		splitOn f l@(x:xs)
			| f x = splitOn f xs
			| otherwise = let (h,t) = break f l in h:(splitOn f t)

hrule = try $ do
	many1 (char '-') >> return HRule

ulist = try $ do
	many1 ulistitem >>= return . UList

ulistitem :: P [Inline]
ulistitem = ulistbegin >> manyTill inline ((try (blankline >> lookAhead ulistbegin)) <|> eob)
	where
		ulistbegin = char '*' >> spaces

olist = try $ do
	items <- many1 olistitem 
	return $ OList items

olistitem :: P [Inline]
olistitem = olistbegin >> manyTill inline eob
	where
		olistbegin = number >> (char '.' <|> char ')') >> spaces

paragraph = do
	manyTill inline eob >>= return . Paragraph

--------------- inline parsers
inline :: P Inline
inline = choice
	[ str
	, whitespace
	, strong
	, quoted
	, apostrophe
	, escape
	, escapeNum
	, punctuation
	, symbol
	, link
	] <?> "inline"

str, whitespace, strong, apostrophe, punctuation, escape, escapeNum, symbol, link, quoted :: P Inline

str         = many1 (noneOf strChar) >>= return . Str
whitespace  = many1 (oneOf whiteChars) >>= return . White
strong      = try (between (string "**") (try $ string "**") inline >>= return . Strong)
apostrophe  = char '\'' >> return Apostrophe
punctuation = oneOf ".,:;!?" >>= return . Punctuation
escape      = try (char '\\' >> many1 (oneOf alphaChars)) >>= return . Escape
escapeNum   = try (char '\\' >> char '#' >> many1 (oneOf digitChars)) >>= return . EscapeNum . read
symbol      = oneOf "<>_-^&+*|/\\()@#$%" >>= return . Symbol
link        = try (between (char '[') (char ']') linkdata >>= return . Link)
quoted      = try (between (char '"') (char '"') (many (noneOf "\"")) >>= return . Quoted)

linkdata :: P [String]
linkdata    = many (noneOf "|]") `sepBy` (char '|')
-------------- utility parsers
specialChars, whiteChars, digitChars, strChar :: String
specialChars = "\\[]|*_~`<>$!^-,.&\"';"
whiteChars   = " \t\r\n"
digitChars   = ['0'..'9']
alphaChars   = ['a'..'z'] ++ ['A'..'Z']
strChar      = specialChars ++ whiteChars

space :: P ()
space = char ' ' >> return ()

spaces :: P ()
spaces = many1 (char ' ') >> return ()

blankline :: P ()
blankline = try (optional spaces >> eol) >> return ()

number :: P String
number = many1 (oneOf digitChars)

eob :: P ()
eob = try $ do
	blankline
	eof <|> (many1 blankline >> return ())
	notFollowedBy (space >> noneOf "\n")

eolR :: P String
eolR = try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|> string "\r" <?> "EOL"

eol :: P ()
eol = eolR >> return ()
