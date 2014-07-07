import Control.Exception

import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.List
import Data.Maybe

import System.Environment
import System.Exit
import System.IO

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n = if length xs < (n+1) then Nothing else Just(xs!!n)

showCharCount :: (Integral a, Show a) => (Char, a) -> String
showCharCount (c, n) =  '\'' : c : "': " ++ (show n)

getSortedElems :: (Hashable k, Ord k, Ord v) => HM.HashMap k v -> [(k, v)]
getSortedElems = sortBy (\(c1, n1) (c2, n2) ->
                          if n1 == n2
                            then compare c1 c2
                            else compare n2 n1) . -- reversed compare
                 HM.toList

countUniqueElems :: (Eq k, Hashable k, Integral v) => [k] -> HM.HashMap k v
countUniqueElems []     = HM.empty
countUniqueElems (x:xs) = HM.unionWith (+) (HM.singleton x 1) (countUniqueElems xs)

main :: IO ()
main = do
  args  <- getArgs
  fname <- case safeIndex args 0 of
             Nothing -> putStrLn "usage: ccount [file]" >> exitFailure
             Just f  -> return f
  machineOutput <- case safeIndex args 1 of
                     Nothing -> return False
                     Just s  -> if s == "--machine-output" then return True else return False
  res <- (try $ withFile fname ReadMode $
           \h -> hGetContents h >>= (putStrLn .
                                     (if machineOutput then show
                                                       else concat             .
                                                            intersperse ", "   .
                                                            map showCharCount) .
                                     getSortedElems                                .
                                     countUniqueElems                              .
                                     filter (/= '\n'))) :: IO (Either IOException ())
  case res of
    Right _ -> return ()
    Left  _ -> (putStrLn $ "Could not open file: " ++ fname) >> exitFailure
