import Test.QuickCheck
import Text.PrettyPrint.Boxes

-- instance Arbitrary Box where
--  arbitrary = oneOf [ 
--                    ] 

-- extensional equivalence for Boxes
b1 === b2 = render b1 == render b2

prop_render_text s = render (text s) == (s ++ "\n")

prop_empty_right_id b = b <> emptyBox === b