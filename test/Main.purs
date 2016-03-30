module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Error.Util (hush)
import Data.Generic (class Generic, gEq, gShow)
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (bind, class Eq, class Show, compose, return, show, Unit, unit, (<>), (==))
import Test.Unit (test, runTest, TIMER)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Assert (assert)
import Text.Boomerang.HStack (class HList, hArg, hCons, HCons(..), hHead, HNil, hNil, hSingleton, hTop2, HTop2)
import Text.Boomerang.Combinators (cons, list, nil, opt, pure)
import Text.Boomerang.Prim (Boomerang(..), runSerializer)
import Text.Boomerang.Routing ((</>))
import Text.Boomerang.String (int, lit, string, StringBoomerang)
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

instance eqProfile :: Eq Profile where
  eq = gEq

profileR :: forall r. (HList r) => StringBoomerang r (HCons Profile r)
profileR =
  lit "profile" </> pR `compose` int </> pvR `compose` opt (lit "/")
 where
  -- Profile route
  pR :: forall t. StringBoomerang (HTop2 Int ProfileViewMode t) (HCons Profile t)
  pR = pure (hArg (hArg hCons) (\i v -> Profile { id : i, view : v})) (\(HCons (Profile p) t) -> Just (hTop2 p.id p.view t))

  -- ProfileView route
  pvR :: forall t. StringBoomerang t (HCons ProfileViewMode t)
  pvR =
    vb `compose` (string "compact" <> string "extended")
   where
    vb = pure (hArg (hCons) (\s -> if s == "compact" then Compact else Extended))
              ser
    ser (HCons Compact t) = Just (hCons "compact" t)
    ser (HCons Extended t) = Just (hCons "extended" t)

main :: forall e. Eff ( timer :: TIMER
                      , avar :: AVAR
                      , testOutput :: TESTOUTPUT | e
                      ) Unit
main = runTest do
  test "Profile routes" do
    let profile = Profile {id: 20, view: Compact}
    assert "profile route parsing"
      (parse profileR "profile/20/compact/" == Just profile)
    assert "profile route parsing without trailing slash"
      (parse profileR "profile/20/compact/" == Just profile)
    assert "profile route serialization"
      ((serialize profileR (hSingleton profile)) == Just "profile/20/compact/")

  -- turn these into basic tests
  -- let foo = (string "foo" :: forall r. StringBoomerang r (HCons String r))
  --     bar = (string "bar" :: forall r. StringBoomerang r (HCons String r))
  --     fooBar = (cons `compose` foo `compose` cons `compose` bar `compose` nil)
  --     fooOrBar = (string "foo" <> string "bar")
  --     fooOrBarList = list (string "foo" <> string "bar")
  --     -- fooOrBarList = list fooBar

  -- log "test"
  -- log (show (parse foo "foo"))
  -- log (show (parse bar "foo"))
  -- log (show (parse fooBar "foobar"))
  -- log (show (parse fooBar "barfoo"))
  -- log (show (parse fooOrBar "bar"))
  -- log (show (parse fooOrBar "foo"))
  -- log (show (serialize fooBar (hSingleton ("foo" : "bar" : Nil))))

  -- log (show (parse fooOrBarList "foofoofoobarfoobar"))
  -- log (show (serialize fooOrBarList (hSingleton ("bar":"foo":"foo":"bar":Nil))))

