import qualified Data.Set as Set
import System.IO

readInts :: [String] -> [Int]
readInts = map read

fSum :: FilePath -> IO ()
fSum file = do
                handle <- openFile file ReadMode
                input <- hGetContents handle
                let filtInput = [x | x <- input, not (x `elem` "+")]
                let inputLines = lines filtInput
                print $ sum $ readInts inputLines
                hClose handle

firstDup :: Ord a => [a] -> Maybe a
firstDup ls = findDup ls Set.empty
    where
        findDup [] _ = Nothing
        findDup (x:xs) seen =
            if Set.member x seen
                then Just x
                else findDup xs (Set.insert x seen)

fFindDup :: FilePath -> IO ()
fFindDup file = do
                    handle <- openFile file ReadMode
                    input <- hGetContents handle
                    let filtInput = [x | x <- input, not (x `elem` "+")]
                    let inputLines = lines filtInput
                    let cumSums = scanl (+) 0 $ cycle $ readInts inputLines
                    let fstDup = firstDup cumSums
                    print fstDup
                    hClose handle