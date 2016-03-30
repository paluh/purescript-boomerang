### purescript-boomerang - typesafe bidirectional routing

This library implements invertible parsers (it is clone of haskell boomerang library). With these (semi)isomorphic stuctures and provided combinators it's quite easy to define bidirectional web routes (example.

I'm still experimenting with the implementation, so proper error handling still doesn't exist (you get Nothing when parsing/serialization fails) etc.

Look into `test/Main.purs` for some simple examples - they are really verbose, but I'm going to experiment with Generics to produce more compact and generic utilities...
