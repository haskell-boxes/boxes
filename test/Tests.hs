module Main where

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

import Control.Monad
import System.Exit (exitFailure)

import Test.QuickCheck

import Text.PrettyPrint.Boxes
import Definitions ()

-- extensional equivalence for Boxes
b1 ==== b2 = render b1 == render b2
infix 4 ====

prop_render_text s = render (text s) == (s ++ "\n")

prop_empty_right_id b = b <> nullBox ==== b
prop_empty_left_id b  = nullBox <> b ==== b
prop_empty_top_id b   = nullBox // b ==== b
prop_empty_bot_id b   = b // nullBox ==== b
prop_associativity_horizontal a b c = a <> (b <> c) ==== (a <> b) <> c
prop_associativity_vertical   a b c = a // (b // c) ==== (a // b) // c

main = quickCheckOrError
    [ quickCheckResult prop_render_text
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
