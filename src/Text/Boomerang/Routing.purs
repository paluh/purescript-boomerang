module Text.Boomerang.Routing where

import Debug.Trace (trace)

import Data.Array as Array
import Data.Array.Unsafe (head)
import Data.Foldable (foldl, foldr)
import Data.Generic (class Generic, DataConstructor, fromSpine, GenericSignature(..), GenericSpine(..), toSignature, toSpine)
import Data.List (fromFoldable, List)
import Data.Maybe (Maybe(..))
import Prelude (const, id, show, unit, Unit, (<<<), (<>), (==), (<$>), ($))
import Text.Boomerang.Combinators (arrayFromList, cons, maph, nil, pure)
import Text.Boomerang.HStack (HCons, hNil, HNil)
import Text.Boomerang.String (int, lit, noneOf, StringBoomerang, string)
import Type.Proxy (Proxy)

join :: forall a b c. StringBoomerang b c -> StringBoomerang a b -> StringBoomerang a c
join b1 b2 = b1 <<< lit "/" <<< b2

infixl 6 join as </>

boolean :: forall r. StringBoomerang r (HCons Boolean r)
boolean =
  boolean' <<< (string "on" <> string "off")
 where
  boolean' :: forall s. StringBoomerang (HCons String s) (HCons Boolean s)
  boolean' =
    maph prs ser
   where
    prs "on" = true
    prs _    = false

    ser true  = Just "on"
    ser false = Just "off"

-- str :: forall r. StringBoomerang r (HCons String r)
-- str = noneOf ['/']

type GenericRecProp = {recLabel :: String, recValue :: Unit -> GenericSpine}

signatureToSpineBoomerang :: forall r. GenericSignature -> StringBoomerang r (HCons GenericSpine r)
signatureToSpineBoomerang SigBoolean =
  maph SBoolean ser <<< boolean
 where
  ser (SBoolean b) = Just b
  ser _            = Nothing
signatureToSpineBoomerang SigInt =
  maph SInt ser <<< int
 where
  ser (SInt b) = Just b
  ser _        = Nothing
signatureToSpineBoomerang (SigRecord props) =
  toRecord <<< foldl step nil props
 where
  step r e = cons <<< (toProp e.recLabel <<< signatureToSpineBoomerang (e.recValue unit)) </> r

  toProp :: forall s. String -> StringBoomerang (HCons GenericSpine s) (HCons GenericRecProp s)
  toProp l =
    maph propPrs propSer
   where
    propPrs v = {recLabel : l, recValue : \_ -> v}
    propSer p =
      trace ("p.recLabel = " <> p.recLabel <> "; l = " <> l)
        (\_ ->
          if p.recLabel == l
            then Just (p.recValue unit)
            else Nothing)

  toRecord :: forall s. StringBoomerang (HCons (List GenericRecProp) s) (HCons GenericSpine s)
  toRecord =
    maph SRecord recSer <<< arrayFromList
   where
    recSer (SRecord props) = Just props
    recSer _               = Nothing

signatureToSpineBoomerang s@(SigProd n cs) =
  -- XXX: this is coorectly only for single constructor
  let constructor = (head cs) in
  fromConstructor constructor
 where
  fromConstructor :: forall s. DataConstructor -> StringBoomerang s (HCons GenericSpine s)
  fromConstructor constructor =
    maph (SProd constructor.sigConstructor) ser <<< arrayFromList <<< foldl step nil constructor.sigValues
   where
    ser (SProd c values) = Just values
    ser _                = Nothing

    lazy :: forall a t. StringBoomerang (HCons a t) (HCons (Unit -> a) t)
    lazy = maph const (Just <<< (_ $ unit))

    step r e = cons <<< lazy <<< signatureToSpineBoomerang (e unit) </> r

gRoute :: forall a r. (Generic a) => Proxy a -> StringBoomerang r (HCons (Maybe a) r)
gRoute p =
  maph fromSpine (toSpine <$> _) <<< (signatureToSpineBoomerang (toSignature p))

