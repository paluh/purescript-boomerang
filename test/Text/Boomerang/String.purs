module Test.Text.Boomerang.String where

import Data.Generic (class Generic, gEq, gShow)
import Data.Generic.Rep as Generic.Rep
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Symbol (SProxy(..))
import Prelude
import Test.Unit (TestSuite, test)
import Test.Unit as Test.Unit
import Test.Unit.Assert (assert, equal)
import Text.Boomerang.Combinators (cons, listSep, nil, opt, pureBmg)
import Text.Boomerang.Generic (constructorBoomerang)
import Text.Boomerang.HStack (type (:-), hCons, hArg, (:-))
import Text.Boomerang.String (int, lit, parse, serialize, string, StringBoomerang)


data ProfileViewMode = Compact | Extended
derive instance genericProfileViewMode :: Generic ProfileViewMode

join :: forall a b c. StringBoomerang b c -> StringBoomerang a b -> StringBoomerang a c
join b1 b2 = b1 <<< lit "/" <<< b2

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

profileR :: forall r. StringBoomerang r (Profile :- r)
profileR =
  lit "profile" `join` pR <<< int `join` pvR <<< opt (lit "/")
 where
  -- Profile route
  pR :: forall t. StringBoomerang (Int :- ProfileViewMode :- t) (Profile :- t)
  pR = pureBmg
    (hArg (hArg hCons) (\i v -> Profile { id : i, view : v}))
    (\(Profile p :- t) -> Just (p.id :- p.view :- t))

  -- ProfileView route
  pvR :: forall t. StringBoomerang t (ProfileViewMode :- t)
  pvR =
    vb <<< (string "compact" <> string "extended")
   where
    vb = pureBmg (hArg (hCons) (\s -> if s == "compact" then Compact else Extended)) ser
    ser (Compact :- t) = Just (hCons "compact" t)
    ser (Extended :- t) = Just (hCons "extended" t)

data Products
  = Section Int
  | Category Int
derive instance eqProducts :: Eq Products
derive instance genericProducts :: Generic Products
derive instance genericrepProducts :: Generic.Rep.Generic Products _
instance showProducts :: Show Products where
  show = gShow

sectionR :: forall r. StringBoomerang r (Products :- r)
sectionR = constructorBoomerang (SProxy :: SProxy "Section") <<< lit "section-prefix" <<< lit "/" <<< int
categoryR :: forall r. StringBoomerang r (Products :- r)
categoryR = constructorBoomerang (SProxy :: SProxy "Category") <<< int

productsR :: forall r. StringBoomerang r (Products :- r)
productsR = sectionR <> categoryR

suite :: forall a. TestSuite a
suite = do
  test "String parsing" do
    let foo = (string "foo" :: forall r. StringBoomerang r (String :- r))
        bar = (string "bar" :: forall r. StringBoomerang r (String :- r))
    equal (Just "foo") (parse foo "foo")
    equal Nothing (parse foo "bar")

  test "`parse` function consume whole input" do
    let foo = (string "foo" :: forall r. StringBoomerang r (String :- r))
    equal Nothing (parse foo "foo-and-something-more")

  Test.Unit.suite "Basic composition with list aggregation" do
    let fooBar = (cons <<< string "foo" <<< cons <<< string "bar" <<< nil)
    test "parsing" do
      equal (Just ("foo":"bar":Nil)) (parse fooBar "foobar")
      equal Nothing (parse fooBar "barfoo")
    test "serialization" do
      equal (Just "foobar") (serialize fooBar ("foo" : "bar" : Nil))
      equal Nothing (serialize fooBar ("foo" : "bar" : "baz" : Nil))

  test "listSep combinator for non empty list" do
    let intList = (listSep int (lit ","))
    equal (Just (1:2:3:4:5:Nil)) (parse intList "1,2,3,4,5")
    equal (Just "1,2,3,4,5") (serialize intList (1:2:3:4:5:Nil))

  test "listSep combinator for singleton list" do
    let intList = (listSep int (lit ","))
    equal (Just (1:Nil)) (parse intList "1")
    equal (Just "1") (serialize intList (1:Nil))

  test "listSep combinator for empty list" do
    let intList = (listSep int (lit ","))
    equal (Just (Nil)) (parse intList "")
    equal (Just "") (serialize intList (Nil))

  Test.Unit.suite "Basic alternatives" do
    let fooOrBar = string "foo" <> string "bar"
    test "parsing" do
      equal (Just "bar") (parse fooOrBar "bar")
      equal (Just "foo") (parse fooOrBar "foo")
    test "serialization" do
      equal (Just "foo") (serialize fooOrBar "foo")
      equal (Just "bar") (serialize fooOrBar "bar")

  Test.Unit.suite "Alternatives with prefixes" do
    let fooOrBar = (lit "foo-prefix" <<< string "foo") <>
                   (lit "bar-prefix" <<< string "bar")
    test "parsing" do
      equal (Just "bar") (parse fooOrBar "bar-prefixbar")
      equal (Just "foo") (parse fooOrBar "foo-prefixfoo")
    test "serialization" do
      equal (Just "foo-prefixfoo") (serialize fooOrBar "foo")
      equal (Just "bar-prefixbar") (serialize fooOrBar "bar")

  Test.Unit.suite "Multiple choices with string prefixes" do
    let fooOrBar = string "/" <<< lit "foo-prefix" <>
                   string "/" <<< lit "bar-prefix"
    test "parsing" do
      equal (Just "/") (parse fooOrBar "/foo-prefix")
      equal (Just "/") (parse fooOrBar "/bar-prefix")

  Test.Unit.suite "Multiple choices with literal prefixes" do
    let fooOrBarOrBaz = lit "/" <<< lit "foo-prefix" <<< string "foo" <>
                        lit "/" <<< lit "bar-prefix" <<< string "bar" <>
                        lit "/" <<< lit "baz-prefix" <<< string "baz"
    test "parsing" do
      equal (Just "bar") (parse fooOrBarOrBaz "/bar-prefixbar")
      equal (Just "foo") (parse fooOrBarOrBaz "/foo-prefixfoo")
      equal (Just "baz") (parse fooOrBarOrBaz "/baz-prefixbaz")
    test "serialization" do
      equal (Just "/foo-prefixfoo") (serialize fooOrBarOrBaz "foo")
      equal (Just "/bar-prefixbar") (serialize fooOrBarOrBaz "bar")
      equal (Just "/baz-prefixbaz") (serialize fooOrBarOrBaz "baz")

  test "Profile routes" do
    let profile = Profile {id: 20, view: Compact}
    equal (Just profile) (parse profileR "profile/20/compact/")
    assert "profile route parsing without trailing slash"
      (parse profileR "profile/20/compact/" == Just profile)
    equal (Just "profile/20/compact/") (serialize profileR profile)

  test "Products routes" do
    let
      section = Section 20
      category = Category 30

    equal (Just section) (parse productsR "section-prefix/20")
    equal (Just category) (parse productsR "30")
    -- assert "profile route parsing without trailing slash"
    --   (parse profileR "profile/20/compact/" == Just profile)
    -- equal (Just "profile/20/compact/") (serialize profileR profile)
