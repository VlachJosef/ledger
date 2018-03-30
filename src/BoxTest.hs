module BoxTest where

import           Data.List              (transpose)
import           Text.PrettyPrint.Boxes

blockIds :: [String]
blockIds = show <$> [1 .. 10]

txCount :: [String]
txCount = show <$> [10 .. 20]

timestamps :: [String]
timestamps = replicate 10 <$> ['A' .. 'B']

boxData :: [[String]]
boxData = [blockIds, txCount, timestamps]

test1 :: [[String]] -> IO ()
test1 rows = printBox $ hsep 2 left (map (vcat left . map text) rows)

box1 :: [Box]
box1 = text <$> ["PPPPPPPPPPPPPPPPPPPP", "QQQQQQQQQQQQQQQQQQQQ"]

box2 :: [Box]
box2 = text <$> ["POKLLOP", "JSHJDHSKDJSD"]

-- box3 = box1 <> box2
-- box4 = box1 <+> box2
box5 = hcat right box1

box6 = hcat right box2
