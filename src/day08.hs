
import Data.List.Split
import Debug.Trace

type Instruction = (
  String, -- nop, jmp, acc
  Int, -- argument
  Bool) -- has been run before

parseProgram :: String -> [Instruction]
parseProgram
  = map parseOp . splitWhen (=='\n')
    where
      parseOp :: String -> Instruction
      parseOp str = (op, read $ dropWhile (=='+') arg, False)
        where
          (op, _:arg) = break (==' ') str

partOne :: String -> Int
partOne
  = runProgram 0 0 []
  . parseProgram
    where
      runProgram :: Int -> Int -> [Instruction] -> [Instruction] -> Int
      runProgram acc 0 _ ((_, _, True):_) = acc
      runProgram acc 0 ps (("nop", arg, _) : ns) = runProgram acc 0 (("nop", arg, True) : ps) ns
      runProgram acc 0 ps (("acc", arg, _) : ns) = runProgram (acc + arg) 0 (("acc", arg, True) : ps) ns
      runProgram acc 0 ps (("jmp", arg, _) : ns) = runProgram acc arg ps (("jmp", arg, True) : ns)
      runProgram acc jump ps (n:ns)
        | jump > 0 = runProgram acc (jump - 1) (n : ps) ns
      runProgram acc jump (p:ps) ns
        | jump < 0 = runProgram acc (jump + 1) ps (p : ns)
      runProgram acc jump ps ns = error $ show (acc, jump, ps, ns)

candidates :: [Instruction] -> [[Instruction]]
candidates [] = [[]]
candidates (("acc", arg, v) : is)
  = map (("acc", arg, v) :) (candidates is)
candidates ((_, arg, v) : is)
  = map (("nop", arg, v) :) (candidates is)
  ++ map (("jmp", arg, v) :) (candidates is)

partTwo :: String -> Int
partTwo program
  = head [acc | (acc, t) <- results, t]
    where
      results :: [(Int, Bool)]
      results = map (runProgram 0 0 []) (candidates instructions)
      
      candidates :: [Instruction] -> [[Instruction]]
      candidates [] = [[]]
      candidates (("acc", arg, v) : is)
        = map (("acc", arg, v) :) (candidates is)
      candidates ((op, arg, v) : is)
        = ((op', arg, v) : is)
        : map ((op, arg, v) :) (candidates is)
          where
            op' = if op == "jmp" then "nop" else "jmp"

      instructions = parseProgram program

      runProgram :: Int -> Int -> [Instruction] -> [Instruction] -> (Int, Bool)
      runProgram acc 0 _ []                      = (acc, True)
      runProgram acc 0 _ ((_, _, True):_)        = (acc, False)
      runProgram acc 0 ps (("nop", arg, _) : ns) = runProgram acc 0 (("nop", arg, True) : ps) ns
      runProgram acc 0 ps (("acc", arg, _) : ns) = runProgram (acc + arg) 0 (("acc", arg, True) : ps) ns
      runProgram acc 0 ps (("jmp", arg, _) : ns) = runProgram acc arg ps (("jmp", arg, True) : ns)
      runProgram acc jump ps (n:ns)
        | jump > 0 = runProgram acc (jump - 1) (n : ps) ns
      runProgram acc jump (p:ps) ns
        | jump < 0 = runProgram acc (jump + 1) ps (p : ns)

      runProgram acc jump ps ns = error $ show (acc, jump, ps, ns)


testProgram1 :: String
testProgram1 = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"

testProgram2 :: String
testProgram2 = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\nnop -4\nacc +6"

program :: String
program = "acc +3\njmp +599\nnop +311\njmp +605\nacc -3\nacc +50\nacc -6\njmp +461\njmp -4\nacc -7\njmp +1\nacc +19\nacc -18\njmp +485\nnop +182\njmp +174\nacc +41\nacc +10\nnop +570\njmp +428\nacc +18\nacc +33\njmp +197\njmp +202\nacc +43\nacc -19\nacc -12\njmp +453\nacc +8\njmp +55\nacc +5\nnop +482\nacc -11\njmp +475\nacc -5\nacc +38\nacc -16\nnop +111\njmp +230\nacc +41\nacc -4\njmp +16\nnop +147\njmp -15\nnop -28\njmp +96\nacc +34\nacc +27\njmp -25\njmp +8\nacc +8\nnop +28\njmp +515\njmp +247\njmp +474\nnop +392\njmp +57\nnop +271\nacc +20\njmp +514\nacc +22\njmp +337\nacc +47\nacc +43\nacc +42\nnop +263\njmp +144\nacc +26\nacc +49\nacc +22\njmp +170\nnop +502\nacc +26\nacc -3\njmp +96\nacc -9\nnop +213\nacc +1\njmp +111\nnop +189\njmp +533\nacc -18\nacc -15\njmp +209\nnop +464\njmp +463\nacc +16\nacc +39\nacc +36\njmp +499\nacc +42\njmp +1\njmp +444\nacc +33\nacc -5\nnop +513\nacc +17\njmp +377\njmp +410\nacc -5\njmp +312\njmp +235\nacc -4\nacc +32\nacc +40\njmp +477\njmp +388\njmp +112\nacc +45\nacc +36\njmp -68\nnop +296\njmp +496\nacc -19\nacc +1\nacc -8\njmp +1\njmp +479\njmp +195\nacc -13\nacc +50\nacc +30\njmp +167\njmp +217\nacc +17\nacc +8\njmp +22\nacc +46\nacc -5\njmp +53\njmp +152\nacc +29\nacc +1\nacc +24\njmp +278\nacc +20\njmp +95\nacc +15\njmp +1\nacc +36\njmp +286\nacc +44\nacc +33\njmp +117\nacc +12\nacc +16\njmp +1\njmp +284\nacc -15\nnop +478\nacc -17\njmp +13\nnop +274\nnop +217\nnop +91\njmp -113\nnop -58\nacc +11\nacc +28\nnop +301\njmp +132\nacc -7\nacc +18\njmp +173\nacc +39\nnop +435\njmp +388\nacc +15\nacc +50\njmp +152\nacc -8\nacc -10\nacc +15\nacc +39\njmp +166\nacc +14\njmp +310\nnop +371\nacc +26\njmp +161\nacc +37\njmp -147\nacc -12\nacc +37\nnop -78\njmp +11\nacc +5\nnop -130\njmp +182\nacc +23\nacc +17\njmp -14\nacc +42\nacc +16\nacc +40\njmp -39\nnop +325\nacc +15\njmp +70\nacc +39\nacc +13\nnop +211\njmp +210\nacc -18\nnop +384\nacc +28\njmp -98\nacc +21\nacc +12\njmp +217\nacc +22\nacc +4\nacc +12\njmp +421\nacc +26\nnop +298\nacc +1\nacc +43\njmp -15\nacc +39\nnop +217\nnop +31\nacc +17\njmp -189\njmp -68\nacc -14\njmp +287\nnop +62\nacc +20\nacc +50\njmp -5\nacc +26\nacc -14\nacc +24\nacc -2\njmp -181\nacc +12\nnop -89\nacc +13\njmp -50\nacc +39\njmp +233\nnop -214\nacc +47\njmp +216\nacc +21\nacc +30\nnop +347\nacc +34\njmp -240\nnop -196\njmp +345\nacc +48\nacc +43\nacc +4\nnop +266\njmp +72\nacc +7\nacc +43\njmp +1\nacc +44\nacc +1\nacc +21\njmp +358\nacc +20\nacc +28\nacc +48\njmp +266\nacc +14\nacc +30\njmp +167\nnop +18\nacc +17\nnop +125\nacc +14\njmp -111\nnop +332\nacc -12\nnop -177\njmp +355\nacc -8\njmp -125\nacc +6\njmp -185\nnop +270\nacc +32\nacc +19\nacc -9\njmp +339\njmp -13\nnop +23\njmp -109\nacc -4\nacc +23\nacc +39\nnop +305\njmp +130\nnop -57\nacc +46\njmp +301\njmp +1\njmp +150\nacc -6\nnop -184\nacc +18\njmp -123\nacc +11\nacc +40\njmp -304\nacc +16\nacc +26\nnop -307\njmp +3\njmp -194\njmp -224\nacc +8\nacc +22\nacc +1\nacc -1\njmp +73\njmp +41\nacc +40\njmp +80\nacc +0\nacc +39\nacc +6\nacc +45\njmp -186\nacc +32\nacc -5\njmp -99\nacc +47\nacc +17\nacc +1\nacc +0\njmp +265\njmp +264\nnop +114\nacc +13\njmp -108\nnop -278\nacc +29\nacc -14\njmp -297\nacc +20\nacc +37\nnop +175\nacc -4\njmp +9\nacc -11\nnop +136\nacc +2\njmp -37\nacc +48\nacc +9\nacc -7\njmp +36\nacc -15\njmp -118\nacc -9\njmp -68\nacc +26\nnop -1\nacc +9\njmp -15\nacc +21\nacc +13\nacc -2\nacc -17\njmp -365\nacc +5\nacc +8\njmp +255\nacc +16\nnop -312\nacc -14\njmp -19\nacc +32\nacc +37\nacc +9\njmp +1\njmp -302\njmp +1\nacc +5\nacc +45\nacc +42\njmp +61\nacc +20\nacc +36\njmp +156\nacc -9\njmp +117\nacc -1\nnop -389\njmp +242\nacc +9\nacc -18\njmp -5\njmp -77\nacc +17\nacc +30\njmp +172\nacc -1\nacc +11\nacc -6\njmp -334\njmp +215\nacc +3\nacc +24\njmp +13\njmp +1\njmp -369\nacc +49\nacc -6\nacc -14\nacc -6\njmp -234\nacc +13\nacc +9\nacc +11\nnop +78\njmp +115\nnop -332\nnop +177\njmp +109\njmp +157\nnop -372\nacc +25\njmp +166\nnop +171\njmp -253\nacc +27\nacc -11\nacc -4\nacc +34\njmp +98\njmp -240\nacc +41\nnop -381\nacc -4\nnop -270\njmp -328\nacc +31\nacc +11\nacc -2\nnop -163\njmp +148\njmp +1\nnop -91\njmp -197\njmp +132\nacc +31\nnop +109\nacc +43\njmp -319\nacc -19\nacc +49\nacc +38\nacc +48\njmp +86\nacc -1\nacc -11\nacc +2\njmp -355\nacc -3\nacc +11\nacc +39\njmp -110\nacc +10\nnop -465\nnop -121\njmp -110\nacc +0\njmp -5\nnop -278\nnop -199\nnop +118\nacc +6\njmp -47\njmp +129\nacc +26\njmp -391\nacc -15\nacc +8\nnop -86\njmp +115\nnop -94\nacc -7\nacc +14\njmp -183\nacc -16\nacc +15\nacc +23\njmp -178\njmp +1\njmp -365\njmp +1\njmp -320\nacc +42\nnop -289\nacc +21\nacc -17\njmp -440\nacc +0\nacc +5\nacc +35\nacc +20\njmp +29\nacc -1\nacc +20\nacc +44\njmp +50\njmp -61\nacc -2\nacc +41\nacc -5\njmp -410\nacc +13\nnop -315\nacc -2\njmp -46\nacc +20\nacc +9\nacc +38\nnop -279\njmp -113\nacc +48\njmp +86\njmp -151\njmp +1\nacc -18\nnop -291\njmp -101\njmp +49\nnop -378\njmp -445\nacc +36\nacc +41\nnop -286\nacc -19\njmp -142\nnop -393\nacc +0\nacc -3\njmp +10\nacc +17\njmp -327\njmp -219\nacc -5\nnop -123\nacc +49\nacc +36\njmp -145\njmp -496\njmp +48\nacc +10\njmp +11\njmp -97\nacc -8\nacc +22\njmp +53\njmp -316\nacc +32\nacc -15\nacc +27\nacc +33\njmp -266\njmp -10\nacc +48\nacc -10\nacc +7\nacc +5\njmp +28\nacc -15\nacc -19\nacc -8\nnop -150\njmp -388\nacc +14\nacc +45\nacc -11\njmp -451\nacc +42\nacc -8\njmp -104\nnop -228\nacc +0\njmp -327\nacc +19\nacc -7\njmp +1\njmp -291\nacc -8\njmp -495\njmp -61\njmp -392\nacc +1\njmp -227\nacc -10\njmp -286\njmp -397\njmp -539\njmp -215\nacc +15\nacc +36\nacc -12\nacc +5\njmp -147\nacc +28\nacc -15\nacc +19\njmp +16\njmp -493\nacc +7\nacc +40\nacc +23\nnop -122\njmp -567\nacc -4\nacc +23\njmp -218\njmp -13\nacc -18\nacc -10\nacc -13\nnop -541\njmp -105\nacc +14\nacc +40\nacc +0\njmp -614\nacc +3\nacc +14\njmp -357\njmp -510\njmp -416\nacc +12\nnop -245\nacc +26\nacc +15\njmp +1"

