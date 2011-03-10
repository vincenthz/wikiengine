wikiengine: wiki syntax and renderer
====================================

Introduction
------------

This little library provides all the necessary stuff to render a wiki-looking
syntax to html, and support rendering configurability for different use cases.

Usage
-----

You only need to import the main module as the following:

  'import Text.WikiEngine'

Then the main function to use are parseDocument and renderAsHTML:
  
  import Text.Blaze.Renderer.Utf8
  main = do
    html <- (renderAsHtml defaultRenderCfg . parseDocument) `fmap` readFile "syntaxFile"
    L.putStrLn $ renderHtml html

A example program that does the same thing is available as the root of the project as Wikihtml.hs

Syntax
------

The syntax is an hybrid of the MarkDown and what i found convenient to use as syntax.
this is made to be mostly readable without rendering.

TODO 'add more description of the syntax'

a list:

  * item1
  * item2
  * item3
