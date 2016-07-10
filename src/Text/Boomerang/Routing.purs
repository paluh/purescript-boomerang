module Text.Boomerang.Routing where

import Prelude (compose)
import Text.Boomerang.HStack (hNil, HNil)
import Text.Boomerang.String (lit, StringBoomerang)

-- XXX: This module will be dropped soon
--      check pureBmgscript-routing-bob for
--      example of routing solution
--      based on this library

join :: forall a b c. StringBoomerang b c -> StringBoomerang a b -> StringBoomerang a c
join b1 b2 = b1 `compose` lit "/" `compose` b2

infixl 6 join as </>
