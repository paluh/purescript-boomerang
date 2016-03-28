module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console
import Control.Error.Util (hush)
import Data.Generic (class Generic, gShow)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (bind, class Show, compose, return, show, Unit, unit, (<>), (==))
import Text.Boomerang.HStack (class HList, hArg, hCons, HCons(..), hHead, HNil, hNil, hSingleton, hTop2, HTop2)
import Text.Boomerang.Combinators (cons, list, nil, pure)
import Text.Boomerang.Prim (Boomerang(..), runSerializer)
import Text.Boomerang.Routing ((</>))
import Text.Boomerang.String (int, string, StringBoomerang)
import Text.Parsing.Parser (runParser)
import Unsafe.Coerce (unsafeCoerce)

parse :: forall a. StringBoomerang HNil (HCons a HNil) -> String -> Maybe a
parse (Boomerang b) s = do
  f <- hush (runParser s b.prs)
  return (hHead (f hNil))

todo :: forall a. a
todo = unsafeCoerce unit

serialize :: forall a t. StringBoomerang HNil (HCons a t) -> (HCons a t) -> Maybe String
serialize (Boomerang b) s = do
  (Tuple f _) <- runSerializer b.ser s
  return (f "")

data ProfileViewMode = Compact | Extended
derive instance genericProfileViewMode :: Generic ProfileViewMode

data Profile =
  Profile {
      id :: Int
    , view :: ProfileViewMode
  }
derive instance genericProfile :: Generic Profile

instance showProfile :: Show Profile where
  show = gShow

-- this route definition is not very... concise ;-)
profileR :: forall r. (HList r) => StringBoomerang r (HCons Profile r)
profileR =
  pr `compose` int </> vr
 where
  pr :: forall t. StringBoomerang (HTop2 Int ProfileViewMode t) (HCons Profile t)
  pr = pure (hArg (hArg hCons) (\i v -> Profile { id : i, view : v})) (\(HCons (Profile p) t) -> Just (hTop2 p.id p.view t))

  vr :: forall t. StringBoomerang t (HCons ProfileViewMode t)
  vr =
    vb `compose` (string "compact" <> string "extended")
   where
    vb = pure (hArg (hCons) (\s -> if s == "compact" then Compact else Extended))
              ser
    ser (HCons Compact t) = Just (hCons "compact" t)
    ser (HCons Extended t) = Just (hCons "extended" t)

main :: forall a. Eff ( console :: CONSOLE | a) Unit
main = do
  let foo = (string "foo" :: forall r. StringBoomerang r (HCons String r))
      bar = (string "bar" :: forall r. StringBoomerang r (HCons String r))
      fooBar = (cons `compose` foo `compose` cons `compose` bar `compose` nil)
      fooOrBar = (string "foo" <> string "bar")
      fooOrBarList = list (string "foo" <> string "bar")
      -- fooOrBarList = list fooBar

  log (show (parse foo "foo"))
  log (show (parse bar "foo"))
  log (show (parse fooBar "foobar"))
  log (show (parse fooBar "barfoo"))
  log (show (parse fooOrBar "bar"))
  log (show (parse fooOrBar "foo"))
  log (show (serialize fooBar (hSingleton ("foo" : "bar" : Nil))))

  log (show (parse fooOrBarList "foofoofoobarfoobar"))
  log (show (serialize fooOrBarList (hSingleton ("bar":"foo":"foo":"bar":Nil))))

  log (show (parse profileR "10/compact"))
  log (show (serialize profileR (hSingleton (Profile {id : 20, view : Extended}))))
