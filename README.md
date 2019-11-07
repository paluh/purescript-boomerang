## purescript-boomerang - typesafe bidirectional routing

This library implements invertible parsers (it is clone of haskell boomerang library). With these (semi)isomorphic stuctures and provided combinators it's quite easy to define bidirectional web routes. You can check __purescript-routing-bob__ for example routing library build upon boomerangs.


## Design

This library is build upon a really simple approach to parsers/serializers. I'm using Haskell tuple notation in the following psudocode snippets to simplify reading.

We can think of parsers as a functions which take an input and result from the provious steps of parsing and give back the result and the rest of input:

  ```purescript
  prs :: (a, tok) -> (b, tok)
  ```

So `tok` on the left hand side of the function represents input stream (for example `String` or list of `String`s) but on the right hand side it represents unconsumed part of the input.
`a` is a result from previous steps of parsing and `b` is a result of current parsing step build upon `a` value.

This type is really general and composable - it composes just by function composition:

  ```purescript
  ((b, tok) -> (c, tok)) <<< ((a, tok) -> (b, tok)):: (a, tok) -> (c, tok)
  ```
What we really have here is a Kleisly Arrow of the well known and beloved monadic parser of type: `tok -> (a, tok)`.

What about serializer. With the above approach to parsing we can easily reverse the function arrow and get:

  ```purescript
  (b, tok) -> (a, tok)
  ```

These types are really isomorphic but the main "mechanical" difference is that "`tok` should grow" from left to right and `a` should be subpart of our staring `b` in the case of a serializer. What is even more important is that this type composes as easily as our parser type because it is also a simple function ;-)

From these two types we can build our Boomerang type - it is just a product of them. Let's write some real Purescript:

  ```purescript
  newtype Boomerang tok a b =
    Boomerang
      { prs :: { tok :: tok, val :: a } -> { tok :: tok, val :: b }
      , ser :: { tok :: tok, val :: b } -> { tok :: tok, val :: a }
      }
  ```

And we can compose them by composing `prs` and `ser` but in "different directions" (this is "Product Category" AFAIK). Here is `Semigroupoid` instance:

  ```purescript
  instance semigroupoidBoomerang :: Semigroupoid (Boomerang tok) where
    compose (Boomerang b1) (Boomerang b2) =
      Boomerang
        { prs: b1.prs <<< b2.prs
        , ser: b2.ser <<< b1.ser
        }
  ```
To be honest this library uses specialized version (so less powerfull) of a parser type:

  ```purescript
  prs' :: tok -> ((a -> b), tok)
  ```

Such a parser can be easily converted into previous form (but not the other way around):

  ```purescript
  prs :: (tok -> ((a -> b), tok)) -> ((tok, a) -> (tok, b))
  prs p = \(tok, a) ->
    let (a2b, tok') = p tok
    in (a2b a, tok')
  ```

This representation allows us to use standard `Parser` type (from `purescript-parsing`) - all we have is:

`type Parser tok a b = Parser tok (a -> b)`.


This library also uses different serializer type internally:

  ```purescript
  ser' :: b -> ((tok -> tok), a)
  ```

It can also be converted to our previous representation:

  ```purescript
  ser :: (b -> ((tok -> tok), a)) -> ((tok, b) -> (tok, a))
  ser s = \(tok, b) ->
    let (tok2tok, a) = s b
    in (tok2tok tok, a)
  ```
