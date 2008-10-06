module Text.PrettyPrint.Boxes where

data Box = Box { rows    :: Int
               , cols    :: Int
               , content :: Content
               }
  deriving (Show)

data Content = Blank
             | Text String
             | Row [Box]
             | Col [Box]
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

hcat :: [Box] -> Box
hcat bs = Box h w (Row $ map (padH h) bs)
  where h = maximum . (0:) . map rows $ bs
        w = sum . map cols $ bs

padH :: Int -> Box -> Box
padH h b | rows b < h = b // Box (h - rows b) (cols b) Blank
padH _ b = b

vcat :: [Box] -> Box
vcat bs = Box h w (Col $ map (padW w) bs)
  where h = sum . map rows $ bs
        w = maximum . (0:) . map cols $ bs

padW :: Int -> Box -> Box
padW w b | cols b < w = b <> Box (rows b) (w - cols b) Blank
padW _ b = b

render :: Box -> String
render b = unlines $ map (flip extractLine b) [0..rows b - 1]

extractLine :: Int -> Box -> String
extractLine _ (Box _ c Blank)        = replicate c ' '
extractLine _ (Box _ _ (Text t))     = t
extractLine i (Box _ _ (Row bs))     = concatMap (extractLine i) bs
extractLine i (Box _ _ (Col []))     = ""
extractLine i (Box r c (Col (b:bs))) | i < rows b = extractLine i b
                                       | otherwise  = extractLine (i - rows b) (Box (r - rows b) c (Col bs))