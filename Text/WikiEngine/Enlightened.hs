{- |
   Module      : Enlightened
   Copyright   : Copyright (C) 2009-2010 Vincent Hanquez
   License     : BSD
   Maintainer  : Vincent Hanquez <vincent@snarc.org>
   Stability   : alpha
   Portabily   : haven't tested
-}

module Text.WikiEngine.Enlightened
	( AtomType(..)
	, Atom
	, atomizeHaskell
	, atomizeC
	, atomizeShell
	) where

import Prelude
import Text.Parsec hiding (space, spaces)

type P a = Parsec String () a

data AtomType =
	  Symbol
	| Keyword
	| Operator
	| Number
	| String
	| Char
	| Comment
	| Other
	deriving (Show,Eq)

type Atom = (AtomType, String)

mapError :: String -> Either ParseError [Atom] -> Either String [Atom]
mapError _       (Right x) = Right x
mapError content (Left pe) = Left (show pe ++ "\n" ++ content)

{-
 - Common parser to common stuff : symbol, operators, digits, char, string ...
 -}
symbolFirstChar, symbolChar :: [Char]
symbolFirstChar = [ 'a'..'z' ] ++ [ 'A'..'Z' ] ++ "_"
symbolChar      = symbolFirstChar ++ [ '0'..'9' ]

symbol :: [String] -> P Atom
symbol kws = try $ do
	f <- oneOf symbolFirstChar
	ending <- many (oneOf symbolChar)
	let s = f:ending
	if s `elem` kws
		then return (Keyword, s)
		else return (Symbol, s)

operators :: [String] -> P Atom
operators kws = try $ do
	s <- many1 (oneOf "+-*/<>=()[]&$|@!:~")
	if s `elem` kws
		then return (Keyword, s)
		else return (Operator, s)

number, hexnumber, str, chr, other :: P Atom
number = try $ many1 digit >>= \x -> return (Number, x)
hexnumber = try $ do
	string "0x" >> many1 hexDigit >>= \x -> return (Number, "0x" ++ x)

str    = try $ between (char '"') (char '"') (many (noneOf "\"")) >>= \x -> return (String, '"' : x ++ ['"'])
chr    = try $ between (char '\'') (char '\'') (oneOf symbolChar) >>= \x -> return (Char, '\'' : x : ['\''])
other  = anyToken >>= \x -> return (Other, [x])

{-
 - C specific parsing and functions exported
 -}
cKeywords :: [String]
cKeywords =
	["auto","break","case","char","const","continue","default","do"
	,"double","else","enum","extern","float","for","goto","if"
	,"int","long","register","return","short","signed","sizeof"
	,"static","struct","switch","typedef","union","unsigned"
	,"void","volatile","while"
	]

parseC :: P [Atom]
parseC = manyTill (choice
	[ symbol cKeywords
	, str
	, chr
	, operators []
	, hexnumber, number
	, other
	]) (try eof)

atomizeC :: String -> Either String [Atom]
atomizeC content = mapError content $ parse parseC "" content

{-
 - haskell specific parsing and functions exported
 -}
haskellKeywords :: [String]
haskellKeywords =
	[ "|","->","<-","@","!","::","_","~","--",">"
	,"as","case","of","class","data","default"
	,"deriving","do","forall","foreign","hiding"
	,"if","then","else","import","infix","infixl"
	,"infixr","instance","let","in","mdo","module"
	,"newtype","qualified","type","where"
	]

hcomment :: P Atom
hcomment = try $ string "--" >> manyTill anyChar (try $ string "\n") >>= \x -> return (Comment, "--" ++ x ++ "\n")
{-
hcomment2 = try $ string "{-" >> manyTill anyChar (try $ string "-}") >>= return . Comment True
-}

parseHaskell :: P [Atom]
parseHaskell = manyTill (choice
	[ symbol haskellKeywords
	, hcomment
	--, hcomment2
	, operators haskellKeywords
	, str
	, chr
	, hexnumber, number
	, other
	]) (try eof)

atomizeHaskell :: String -> Either String [Atom]
atomizeHaskell content = mapError content $ parse parseHaskell "" content

{-
 - shell specific parsing
 -}
shcomment :: P Atom
shcomment = try $ string "#" >> manyTill anyChar (try $ string "\n") >>= \x -> return (Comment, "#" ++ x ++ "\n")

shellKeywords :: [String]
shellKeywords =
	[ "if", "fi", "then", "else", "for", "in"
	, "case", "esac", "export"
	, "echo", "exit"
	, ";", "!", "[", "]", "="
	, "-eq", "-ne", "-lt", "-gt", "-ge", "-le"
	]

parseShell :: P [Atom]
parseShell = manyTill (choice
	[ symbol shellKeywords
	, shcomment
	, operators shellKeywords
	, str
	, chr
	, number
	, other
	]) (try eof)

atomizeShell:: String -> Either String [Atom]
atomizeShell content = mapError content $ parse parseShell "" content
