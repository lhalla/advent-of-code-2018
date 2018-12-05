import Data.List
import System.IO

charCounts :: String -> [(Char, Int)]
charCounts s = map (\x -> (head x, length x)) $ group $ sort s

hasTpl :: [(Char, Int)] -> Int
hasTpl [] = 0
hasTpl (cc:ccs) = 
    if snd cc == 3
        then 1
        else hasTpl ccs

hasDbl :: [(Char, Int)] -> Int
hasDbl [] = 0
hasDbl (cc:ccs) =
    if snd cc == 2
        then 1
        else hasDbl ccs

wordVal :: String -> (Int, Int)
wordVal s = (hasDbl c, hasTpl c)
    where c = charCounts s

listVal :: [String] -> (Int, Int)
listVal words = sumVals (map wordVal words) (0,0)
    where
        sumVals [] res = res
        sumVals (v:vs) res = sumVals vs (fst res + fst v, snd res + snd v)

chkSum :: [String] -> Int
chkSum s = fst v * snd v
    where
        v = listVal s

printCS :: FilePath -> IO ()
printCS file = do
    handle <- openFile file ReadMode
    input <- hGetContents handle
    let inputLines = lines input
    print $ chkSum $ inputLines
    hClose handle

diffCharCount :: String -> String -> Int
diffCharCount a b = diffCount a b 0
    where
        diffCount (s:ss) (t:ts) res =
            if s == t
                then diffCount ss ts res
                else diffCount ss ts (res + 1)
        diffCount s [] res = res + length s
        diffCount [] t res = res + length t

corrIDs :: [String] -> (String, String)
corrIDs ids@(_:xs) = chkIDs ids xs
    where
        chkIDs (a:as) (b:bs) =
            if (diffCharCount a b) == 1
                then (a, b)
                else chkIDs (a:as) bs
        chkIDs (_:as@(_:bs)) [] = chkIDs as bs
        chkIDs [] [] = ("", "")

commonChars :: (String, String) -> String
commonChars a = findCommon (fst a) (snd a) []
    where
        findCommon (s:ss) (t:ts) cmn =
            if s == t
                then findCommon ss ts (cmn ++ [s])
                else findCommon ss ts cmn
        findCommon _ _ cmn = cmn


printCommons :: FilePath -> IO ()
printCommons file = do
    handle <- openFile file ReadMode
    input <- hGetContents handle
    let inputLines = lines input
    print $ commonChars $ corrIDs $ inputLines
    hClose handle