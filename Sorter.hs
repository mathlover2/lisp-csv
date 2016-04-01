module Sorter (sorter)
       where

import Data.Function (on)
import Data.Ord (comparing)

-- | The sorting test for the main program: a lexicographic ordering
-- on names.

sorter grade1 grade2
  = case comparing getLastName grade1 grade2
    of   LT -> True
         EQ -> ((<=) `on` getLastName) grade1 grade2
         GT -> False
