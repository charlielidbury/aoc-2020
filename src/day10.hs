
import Data.List
import qualified Data.Map as Map


partOne :: [Int] -> Int
partOne adapters
  = ones * threes
    where
      Just ones = Map.lookup 1 freqs
      Just threes = Map.lookup 3 freqs

      freqs = Map.fromListWith (+) [(c, 1) | c <- jumps]
      jumps = zipWith (-) (tail ordered) ordered

      rating = maximum adapters + 3
      ordered = 0 : sort (rating : adapters)

-- partTwo :: [Int] -> Int
partTwo adapters
  = combinations sorted
    where
      sorted = 0 : sort (maximum adapters + 3 : adapters)

      combinations :: [Int] -> Int
      combinations (_:[]) = 1
      combinations (n:ns)
        = (length options)
        * sum (map combinations options)
          where
            options = takeWhile (\ l -> not (null l) && (head l <= n + 3)) (tails ns)

testAdapters :: [Int]
testAdapters = [16,10,15,5,1,11,7,19,6,12,4]

adapters :: [Int]
adapters = [49,89,70,56,34,14,102,148,143,71,15,107,127,165,135,26,119,46,53,69,134,1,40,81,140,160,33,117,82,55,25,11,128,159,61,105,112,99,93,151,20,108,168,2,109,75,139,170,65,114,21,92,106,162,124,158,38,136,95,161,146,129,154,121,86,118,88,50,48,62,155,28,120,78,60,147,87,27,7,54,39,113,5,74,169,6,43,8,29,18,68,32,19,133,22,94,47,132,59,83,12,13,96,35]