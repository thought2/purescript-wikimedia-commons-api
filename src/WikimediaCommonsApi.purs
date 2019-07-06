module WikimediaCommonsApi where

import Prelude
import Data.Tuple.Nested (type (/\))
import Data.Vec (Vec)
import Data.Typelevel.Num.Reps (D2)

data ImageResource = ImageResource
    { maxSize :: Vec D2 Number
    , urlTemplate :: String /\ String
    }
