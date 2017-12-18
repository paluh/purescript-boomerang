## purescript-boomerang - typesafe bidirectional routing

This library implements invertible parsers (it is clone of haskell boomerang library). With these (semi)isomorphic stuctures and provided combinators it's quite easy to define bidirectional web routes. You can check __purescript-routing-bob__ for example routing library build upon boomerangs.


## Design

This library is build upon really simple approach to parsers/serializers. I'm using Haskell tuple notation in following psudocode snippets to simplify reading.

We can think of parser as a function which takes an input and result from provious step of parsing and gives back result and rest of the input:

  ```purescript
  prs :: (a, tok) -> (b, tok)
  ```

So `tok` on the left hand side of function type represents input stream (for example `String` or list of `String`s) but on the right hand side it represents unconsumed part of the input.
`a` is a result from previous steps of parsing and `b` is a result of current parsing step build upon `a` value.

This type is really general and composable - it composes just by function composition:

  ```purescript
  ((b, tok) -> (c, tok)) <<< ((a, tok) -> (b, tok)):: (a, tok) -> (c, tok)
  ```
What we really have here is a Kleisly Arrow of the well known and beloved monadic parser of type: `tok -> (a, tok)`.

What about serializer. With above approach to parsing we can easily reverse function arrow and get:

  ```purescript
  (b, tok) -> (a, tok)
  ```

It's funny that these types are really isomorphic, but the main mechanical difference is that "`tok` should grow" from left to right and `a` should be subpart of our staring `b`. What is even more important is that this type composes as easily as our parsers ;-)

From these two types we can build our Boomerang type - let's write some real Purescript:

  ```purescript
  newtype Boomerang tok a b =
    Boomerang
      { prs :: { tok :: tok, val :: a } -> { tok :: tok, val :: b }
      , ser :: { tok :: tok, val :: b } -> { tok :: tok, val :: a }
      }
  ```

And we can compose them by composing `prs` and `ser` but in "different directions" (this is just "Product Category" AFAIK). Here is `Semigroupoid` instance:

  ```purescript
  instance semigroupoidBoomerang :: Semigroupoid (Boomerang tok) where
    compose (Boomerang b1) (Boomerang b2) =
      Boomerang
        { prs: b1.prs <<< b2.prs
        , ser: b2.ser <<< b1.ser
        }
  ```
To be honest this library uses specialized version (so less powerfull) of parser type:

  ```purescript
  prs' :: tok -> ((a -> b), tok)
  ```

Such a parser can be easily converted into previous form (but not other way around):

  ```
  prs :: (tok, a) -> (tok, b)
  prs (tok, a) =
    let (a2b, tok') = prs' tok
    in (a2b a, tok')
  ```

This representation allows us to use standard `Parser` type (from `purescript-parsing`) - all we have is `type Parser tok a b = Parser tok (a -> b)`.


This library also uses different serializer type internally:

  ```purescript
  ser' :: b -> ((tok -> tok), a)
  ```

It also can be converted to our previous representation:

  ```
  ser :: (tok, b) -> (tok, a)
  ser (tok, b) =
    let (tok2tok, a) = ser' b
    in (tok2tok tok, a)
  ```
