{-# LANGUAGE OverloadedStrings #-}
module Text.PrettyPrint.Boxes where

import Data.String
import Control.Arrow ((***), first)

data Box = Box { rows    :: Int
               , cols    :: Int
               , content :: Content
               }
  deriving (Show)

instance IsString Box where
  fromString = text

data Alignment = AlignFirst | AlignCenter1 | AlignCenter2 | AlignLast
  deriving (Eq, Read, Show)

data Content = Blank
             | Text String
             | Row [Box]
             | Col [Box]
             | SubBox Alignment Alignment Box  -- ^ horizontal and vertical alignment.
  deriving (Show)

emptyBox :: Box
emptyBox = Box 0 0 Blank

char :: Char -> Box
char c = Box 1 1 (Text [c])

text :: String -> Box
text t = Box 1 (length t) (Text t)

(<>) :: Box -> Box -> Box
l <> r = hcat [l,r]

(//) :: Box -> Box -> Box
l // r = vcat [l,r]

alignHoriz :: Alignment -> Int -> Box -> Box
alignHoriz a c b = Box (rows b) c $ SubBox a AlignFirst b

alignVert :: Alignment -> Int -> Box -> Box
alignVert a r b = Box r (cols b) $ SubBox AlignFirst a b

align :: Alignment -> Alignment -> Int -> Int -> Box -> Box
align ah av r c = Box r c . SubBox ah av 

hcat :: [Box] -> Box
hcat = hcatA AlignFirst

hcatA :: Alignment -> [Box] -> Box
hcatA a bs = Box h w (Row $ map (alignVert a h) bs)
  where h = maximum . (0:) . map rows $ bs
        w = sum . map cols $ bs

vcat :: [Box] -> Box
vcat = vcatA AlignFirst

vcatA :: Alignment -> [Box] -> Box
vcatA a bs = Box h w (Col $ map (alignHoriz a w) bs)
  where h = sum . map rows $ bs
        w = maximum . (0:) . map cols $ bs

render :: Box -> String
render = unlines . renderBox

-- XXX make QC properties for takeP

takeP :: a -> Int -> [a] -> [a]
takeP _ n _      | n <= 0 = []
takeP b n []              = replicate n b
takeP b n (x:xs)          = x : takeP b (n-1) xs

-- like takeP, but with alignment.
takePA c b n = glue . (takeP b (numRev c n) *** takeP b (numFwd c n)) . split 
  where split t = first reverse . splitAt (numRev c (length t)) $ t
        glue    = uncurry (++) . first reverse
        numFwd AlignFirst   n = n
        numFwd AlignLast    _ = 0
        numFwd AlignCenter1 n = n `div` 2
        numFwd AlignCenter2 n = (n+1) `div` 2
        numRev AlignFirst   _ = 0
        numRev AlignLast    n = n
        numRev AlignCenter1 n = (n+1) `div` 2
        numRev AlignCenter2 n = n `div` 2

blanks :: Int -> String
blanks = flip replicate ' '

renderBox :: Box -> [String]

renderBox (Box r c Blank)            = resizeBox r c [""] 
renderBox (Box r c (Text t))         = resizeBox r c [t]
renderBox (Box r c (Row bs))         = resizeBox r c 
                                       . merge 
                                       . map (renderBoxWithRows r) 
                                       $ bs
                           where merge = foldr (zipWith (++)) (repeat [])
renderBox (Box r c (Col bs))         = resizeBox r c 
                                       . concatMap (renderBoxWithCols c) 
                                       $ bs
renderBox (Box r c (SubBox ha va b)) = resizeBoxAligned r c ha va 
                                       . renderBox 
                                       $ b

renderBoxWithRows :: Int -> Box -> [String]
renderBoxWithRows r b = renderBox (b{rows = r})

renderBoxWithCols :: Int -> Box -> [String]
renderBoxWithCols c b = renderBox (b{cols = c})

resizeBox :: Int -> Int -> [String] -> [String]
resizeBox r c = takeP (blanks c) r . map (takeP ' ' c)

resizeBoxAligned :: Int -> Int -> Alignment -> Alignment -> [String] -> [String]
resizeBoxAligned r c ha va = takePA va (blanks c) r . map (takePA ha ' ' c)
