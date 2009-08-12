import Test.QuickCheck
import Text.PrettyPrint.Boxes

import Control.Monad

instance Arbitrary Alignment where
  arbitrary = elements [ AlignFirst
                       , AlignCenter1
                       , AlignCenter2
                       , AlignLast
                       ]

instance Arbitrary Box where
  arbitrary = do
    (NonNegative r) <- arbitrary
    (NonNegative c) <- arbitrary
    liftM (Box r c) arbitrary

instance Arbitrary Content where
  arbitrary = oneof [ return Blank
                    , liftM Text arbitrary
                    , liftM Row arbitrary
                    , liftM Col arbitrary
                    , liftM3 SubBox arbitrary arbitrary arbitrary
                    ]

-- extensional equivalence for Boxes
b1 === b2 = render b1 == render b2

prop_render_text s = render (text s) == (s ++ "\n")

prop_empty_right_id b = b <> nilBox === b
prop_empty_left_id b  = nilBox <> b === b
prop_empty_top_id b   = nilBox // b === b
prop_empty_bot_id b   = b // nilBox === b