module Text.Boomerang.Generic where

import Prelude
import Data.Generic.Rep (class Generic, Argument(Argument), Constructor(Constructor), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy)
import Text.Boomerang.Combinators (pureBmgPrs, pureBmgSer)
import Text.Boomerang.HStack (type (:-), (:-))
import Text.Boomerang.Prim (Boomerang(Boomerang), Serializer)
import Text.Parsing.Parser (Parser)

class ConstructorArgs rep (s :: Symbol) args | rep s -> args where
  repFromArgs :: SProxy s -> args -> rep
  repToArgs :: SProxy s -> rep -> Maybe args

instance constructorArgs :: ConstructorArgs (Constructor s args) s args where
  repFromArgs _ args = Constructor args :: (Constructor s args)
  repToArgs _ (Constructor args) = Just args

instance leftConstructorArgs :: ConstructorArgs (Sum (Constructor s args) b) s args where
  repFromArgs p args = Inl (Constructor args)

  repToArgs _ (Inl (Constructor args)) = Just args
  repToArgs _ _ = Nothing

instance rightRecurseConstructorArgs :: (ConstructorArgs b s args) => ConstructorArgs (Sum a b) s args where
  repFromArgs p args = Inr (repFromArgs p args)

  repToArgs s (Inr rep') = repToArgs s rep'
  repToArgs _ _ = Nothing

class ArgsStack args stack r | stack -> r where
  fromStack :: stack -> { args :: args, stack :: r}
  toStack :: r -> args -> stack

instance
  recurseArgsStack ::
    (ArgsStack args bottom r) =>
    ArgsStack (Product (Argument a) args) (a :- bottom) r where
  fromStack (a :- stack) =
      let res = fromStack stack
      in
        { args: Product (Argument a) (res.args)
        , stack: res.stack
        }
  toStack stack (Product (Argument a) args) = a :- (toStack stack args)

instance noargsArgsStack :: ArgsStack NoArguments r r where
  fromStack stack = { args: NoArguments, stack: stack }
  toStack stack NoArguments = stack

instance singleArgsStack :: ArgsStack (Argument a) (a :- r) r where
  fromStack (a :- r) = { args: Argument a, stack: r }
  toStack stack (Argument a) = a :- stack

constructorSerializer ::
  forall args n r rep stack t tok
    .  Generic t rep
    => ArgsStack args stack r
    => ConstructorArgs rep n args
    => SProxy n
    -> Serializer tok (t :- r) stack
constructorSerializer s =
  pureBmgSer ser
 where
  ser (t :- r) =
    case repToArgs s (from t) of
      Just args -> Just (toStack r args)
      Nothing -> Nothing

constructorParser ::
  forall args n r rep stack t tok
    . Generic t rep
    => ArgsStack args stack r
    => ConstructorArgs rep n args
    => SProxy n
    -> (Parser tok (stack -> t :- r))
constructorParser s =
  pureBmgPrs f
 where
  f stack =
    let res = fromStack stack
    in
      (to $ repFromArgs s res.args) :- res.stack

constructorBoomerang ::
  forall args n r rep stack t tok
  . Generic t rep
  => ArgsStack args stack r
  => ConstructorArgs rep n args
  => SProxy n
  -> Boomerang tok stack (t :- r)
constructorBoomerang s = Boomerang
  { prs: constructorParser s
  , ser: constructorSerializer s
  }
