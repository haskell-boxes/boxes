module Text.PrettyPrint.Boxes.Internal
    ( Box(..)
    , Alignment(..)
    , Content(..)
    ) where

-- | The basic data type.  A box has a specified size and some sort of
--   contents.
data Box = Box { rows    :: Int
               , cols    :: Int
               , content :: Content
               }
  deriving (Show)

-- | Data type for specifying the alignment of boxes.
data Alignment = AlignFirst    -- ^ Align at the top/left.
               | AlignCenter1  -- ^ Centered, biased to the top/left.
               | AlignCenter2  -- ^ Centered, biased to the bottom/right.
               | AlignLast     -- ^ Align at the bottom/right.
  deriving (Eq, Read, Show)

-- | Contents of a box.
data Content = Blank        -- ^ No content.
             | Text String  -- ^ A raw string.
             | Row [Box]    -- ^ A row of sub-boxes.
             | Col [Box]    -- ^ A column of sub-boxes.
             | SubBox Alignment Alignment Box
                            -- ^ A sub-box with a specified alignment.
  deriving (Show)
