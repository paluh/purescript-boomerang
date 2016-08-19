### purescript-boomerang - typesafe bidirectional routing

This library implements invertible parsers (it is clone of haskell boomerang library). With these (semi)isomorphic stuctures and provided combinators it's quite easy to define bidirectional web routes. You can check __purescript-routing-bob__ for example routing library build upon boomerangs.

I'm still experimenting with the implementation, so proper error handling doesn't exist (you get Nothing when parsing/serialization fails) and docs are missing too ;-)

Look into `test/Main.purs` for some simple examples - they are really verbose, but I'm going to experiment with `Generics` soon to produce more compact and user friendly API...


## Internals

Caution: I'm not mathematician (I'm trying to be practical programmer) and my derivations and arguments are done with hand waving - if you have spotted any inconsistency or mistake please let me know!


### Parsers / Serializers

Let's start with proposition for parser and serializer type. We want to create parser which parses part of input and leaves the rest. It also carries part of result and converts with every step.

    * parser:

        `(tok, b) -> (a, tok)`

    * serializer:

        `(a, tok) -> (tok, b)`


We can transform this types a bit (using these transformations: `(a, b) -> c` ~ `a -> b -> c` and `(a -> b, c)` ~ `a -> (b, c)`):

    * parser:

        `(tok, b) -> (a, tok)`


        `tok -> b -> (a, tok)`

        `tok -> (b -> a, tok)`


    * serializer:

        `(a, tok) -> (tok, b)`

        `a -> tok -> (tok, b)`

        `a -> (tok -> tok, b)`


Above types (plus error handling through `Either` and `Maybe`) are base building blocks for this library. The one important element which is missing is composition of this base combinators...



### Parsers composition

Let's try to compose parsers (using String as token and avoiding `Either` type for simplification):

    data BoomerangParser a b = BoomerangParser (String -> ((a -> b), String))

    (<<<) :: BoomerangParser b c -> BoomerangParser a b -> BoomerangParser a c

    BoomerangParser pb2c <<< BoomerangParser pa2b =
        BoomerangParser (\s -> let (b2c, s') = pb2c s
                                   (a2b, s'') = pa2b s'
                               in (b2c <<< a2b, s''))

There is one design decision worth noting here - these composition...


