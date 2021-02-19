import Test.QuickCheck
import Text.PrettyPrint.Boxes

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad
import System.Exit (exitFailure, exitSuccess)

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

import Data.Semigroup ((<>))

instance Arbitrary Alignment where
  arbitrary = elements [ top, bottom, center1, center2 ]

instance Arbitrary Box where
  arbitrary = do
    r <- nonnegative
    c <- nonnegative
    arbBox r c
    where
      nonnegative = getNonNegative <$> arbitrary

-- A sized generator for boxes. The larger the parameter is, the larger a
-- generated Box is likely to be. This is necessary in order to avoid
-- the tests getting stuck trying to generate ridiculously huge Box values.
arbBox :: Int -> Int -> Gen Box
arbBox r c
  | r <= 0 || c <= 0
  = return nullBox

  | r <= 1 || c <= 1
  = fmap char (arbitrary `suchThat` (/= '\n'))

  | otherwise
  = oneof [ splitR, splitC, return (emptyBox r c) ]
  where
    splitR = do
        r1 <- chooseInt (1, r-1)
        let r2 = r - r1
        a <- arbitrary
        b1 <- arbBox r1 c
        b2 <- arbBox r2 c
        return $ hcat a [ b1, b2 ]

    splitC = do
        c1 <- chooseInt (1, c-1)
        let c2 = c - c1
        a <- arbitrary
        b1 <- arbBox r c1
        b2 <- arbBox r c2
        return $ vcat a [ b1, b2 ]

-- extensional equivalence for Boxes
b1 ==== b2 = render b1 === render b2
infix 4 ====

-- | "Area"
prop_sizes b = label (l (rows b * cols b)) True where
  l n | n <= 0    = "0"
      | n < 10    = "<10"
      | n < 20    = "<20"
      | n < 50    = "<50"
      | n < 100   = "<100"
      | n < 200   = "<200"
      | n < 500   = "<500"
      | n < 1000  = "<1000"
      | otherwise = "large"

prop_render_text s = render (text s) == (s ++ "\n")

prop_empty_right_id b = b <> nullBox ==== b
prop_empty_left_id b  = nullBox <> b ==== b
prop_empty_top_id b   = nullBox // b ==== b
prop_empty_bot_id b   = b // nullBox ==== b
prop_associativity_horizontal :: Box -> Box -> Box -> Property
prop_associativity_horizontal a b c = a <> (b <> c) ==== (a <> b) <> c
prop_associativity_vertical   a b c = a // (b // c) ==== (a // b) // c

main = quickCheckOrError
    [ quickCheckResult prop_sizes
    , quickCheckResult prop_render_text
    , quickCheckResult prop_empty_right_id
    , quickCheckResult prop_empty_left_id
    , quickCheckResult prop_empty_top_id
    , quickCheckResult prop_empty_bot_id
    , quickCheckResult prop_associativity_horizontal
    , quickCheckResult prop_associativity_vertical
    ]

quickCheckOrError :: [IO Result] -> IO ()
quickCheckOrError xs = sequence xs >>= \results ->
    let n = (length . filter (not . isSuccess)) results
    in if n == 0 then return () else do
            putStrLn $ "There are " ++ show n ++ " failing checks! Exiting with error."
            exitFailure
