module Test.Main where

import Prelude
import Test.Text.Boomerang.String as Test.Text.Boomerang.String
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Unit (TIMER, suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff. Eff ( timer :: TIMER
                        , avar :: AVAR
                        , console :: CONSOLE
                        , testOutput :: TESTOUTPUT | eff ) Unit
main = runTest $ do
  suite "Text.Boomerang.String" Test.Text.Boomerang.String.suite
