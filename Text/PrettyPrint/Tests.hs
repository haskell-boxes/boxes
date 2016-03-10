import Test.QuickCheck
import Text.PrettyPrint.Boxes

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad
import System.Exit (exitFailure, exitSuccess)

instance Arbitrary Alignment where
  arbitrary = elements [ AlignFirst
                       , AlignCenter1
                       , AlignCenter2
                       , AlignLast
                       ]

instance Arbitrary Box where
  arbitrary = sized arbBox

-- A sized generator for boxes. The larger the parameter is, the larger a
-- generated Box is likely to be. This is necessary in order to avoid
-- the tests getting stuck trying to generate ridiculously huge Box values.
arbBox :: Int -> Gen Box
arbBox n =
  Box <$> nonnegative <*> nonnegative <*> arbContent n
  where
  nonnegative = getNonNegative <$> arbitrary

instance Arbitrary Content where
  arbitrary = sized arbContent

-- A sized generator for Content values. The larger the parameter is, the
-- larger a generated Content is likely to be. This is necessary in order to
-- avoid the tests getting stuck trying to generate ridiculously huge Content
-- values.
--
-- See also section 3.2 of http://www.cs.tufts.edu/%7Enr/cs257/archive/john-hughes/quick.pdf
arbContent :: Int -> Gen Content
arbContent 0 = pure Blank
arbContent n =
  oneof [ pure Blank
        , Text <$> arbitrary
        , Row <$> halveSize (listOf box)
        , Col <$> halveSize (listOf box)
        , SubBox <$> arbitrary <*> arbitrary <*> decrementSize box
        ]
  where
  decrementSize = scale (\s -> max (s - 1) 0)
  halveSize = scale (`quot` 2)
  box = arbBox n

-- extensional equivalence for Boxes
b1 ==== b2 = render b1 == render b2

prop_render_text s = render (text s) == (s ++ "\n")

prop_empty_right_id b = b <> nullBox ==== b
prop_empty_left_id b  = nullBox <> b ==== b
prop_empty_top_id b   = nullBox // b ==== b
prop_empty_bot_id b   = b // nullBox ==== b

main = do
  quickCheck prop_render_text
  quickCheck prop_empty_right_id
  quickCheck prop_empty_left_id
  quickCheck prop_empty_top_id
  quickCheck prop_empty_bot_id
