### purescript-boomerang - typesafe bidirectional routing

This library implements invertible parsers (it is clone of haskell boomerang library). With these (semi)isomorphic stuctures and provided combinators it's quite easy to define bidirectional web routes. You can check __purescript-routing-bob__ for example routing library build upon boomerangs.

I'm still experimenting with the implementation, so proper error handling doesn't exist (you get Nothing when parsing/serialization fails) and docs are missing too ;-)

Look into `test/Main.purs` for some simple examples - they are really verbose, but I'm going to experiment with `Generics` soon to produce more compact and user friendly API...


## Internals

Caution: I'm not mathematician (I'm trying to be practical programmer) and my derivations and arguments are done with hand waving - if you have spotted any inconsistency or mistake please let me know!


### Parsers / Serializers

Let's try to analyze main type from this library which is called `Boomerang`. I'm going to try to construct this type bottom up. `Boomerang` consists of two isomorphic functions: serializer and parser.

Let's start with trivial parser and serializer types:

    * parser:

        `tok -> a`

    * serializer:

        `a -> tok`

These are clearly inverse functions and they can be used to serialize and parse any value, but... it is not easy to compose them! Such a parser and serializer are not really practical as we can't chain them and they suppose to consume all it's input.

At first let's try to add possibility to a parser to parse only some part of input and leave the rest for next parser:

    * parser:

        `tok -> (a, tok)`

    * serializer:

        `(a, tok) -> tok`


Above parser definition is nothing new in design space ;-), but it allows as to clearly derive serialzier structure.

Now we have the ability to chain parsers and to apply serializer some initial output state. We can imagine that this also allows us to partially chain serializers by hand, but we want to construct isomorphic structure for parsers and serializers which could be used to compose them both.

Let's try to add possibility to leave some input in case of serializer (imagine that we are serializing some part of structure and leave the rest of serialization for another serializers, like for example serializer for first element of a tuple etc.):

    * parser:

        `(tok, b) -> (a, tok)`

    * serializer:

        `(a, tok) -> (tok, b)`

It is strange but we have (nearly) the same type for serializer and parser... the main difference lays behind this types. As we will finally see parser uses `tok` to generate function which converts one value (of type `b`) into another (of type `a`). Additionally it returns some remaining tokens stream (of type `tok`). On the other hand serializer uses input value (of type `a`) to generate function which "appends" to token stream serialized version of input value. Additionally it returns remaining, unserialized value (of type `b`). I hope that it all will be clear in a moment.

Let's continue our deriviation. We can curry both functions:

    * parser:

        `tok -> b -> (a, tok)`

    * serializer:

        `a -> tok -> (tok, b)`

Let's add parenthesis for clarity:

    * parser:

        `tok -> (b -> (a, tok))`

    * serializer:

        `a -> (tok -> (tok, b))`

Now, if we assume that our parser does rely only on input token when doing parsing step then we can move `b` argument to a result of parsing (resulting token will be the same for all `b`'s):

    * parser:

        `tok -> (b -> a, tok)`


Similarly we can assume that in serializer function, remainig `b` value doesn't depend on initial token and extract it from all results:

    * serializer:

        `a -> (tok -> tok, b)`

Above types plus `Either/Maybe` types' wrappers are two parts of our isomorphic Boomerang structure. What's really important these structures can be composed!



### Parsers composition

Let's try to compose parsers (using String as token and avoiding `Either` type for simplification):

    data BoomerangParser a b = BoomerangParser (String -> ((a -> b), String))

    (<<<) :: BoomerangParser b c -> BoomerangParser a b -> BoomerangParser a c

    BoomerangParser pb2c <<< BoomerangParser pa2b =
        BoomerangParser (\s -> let (b2c, s') = pb2c s
                                   (a2b, s'') = pa2b s'
                               in (b2c <<< a2b, s''))

There is one design decision worth noting here - these composition...


### Different parser derivation

Of course our parser derivation and further simplifications are based on arbitrary order of parameters in argument's tuple:

    `(tok, b) -> (a, tok)`

We can easily imagine that our parsing function has type:

    `(b, tok) -> (a, tok)`

And now we are going to derive quite different type through curring:

    `b -> (tok -> (a, tok))`

Then we can use standard type for `Parser`:

    `Parser tok a = Parser (tok -> (a, tok)`

Above type is basic parser type for monadic parsers and it's monad/applicative/functor implementations can be found in various books/libraries (like `purescript-parsing`). Substituting this type to our second equation we get:

    `b -> Parser tok a`

If we assume that type `Parser tok` has monad instance we have category instance for "free" for this type (we have to only use `purescript-arrows` and `Kleisly (Parser tok)` is our category).


### Serializers composition

In this library (as in the original haskell library) parser results are stacked on the `heterogeneous` stack. This heterogeneous stack is build by stacking type `data HCons a s = HCons a s` - for example stack of three values (Int, String, Char) is represented by type:

