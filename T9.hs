{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

keyMap :: M.Map Int String
keyMap = M.fromList [
    (2,     "abc"),
    (3,     "def"),
    (4,     "ghi"),
    (5,     "jkl"),
    (6,     "mno"), 
    (7,     "pqrs"),
    (8,     "tuv"),
    (9,     "wxyz")]

choices :: [Int] -> [String]
choices x = sequence $ catMaybes $ map (flip M.lookup keyMap) $ x

main :: IO ()
main = do
        putStrLn "Enter a t9 combination:"
        tcombo <- getLine
        let digits = map (digitToInt) tcombo
        let combinations = map (T.pack) (choices digits)
        file <- readFile "words_alpha.txt"
        let dict = filter ((== length digits) . T.length) . T.lines . T.pack $ file
        let suggestions = dict `Data.List.intersect` combinations
        mapM_ (TIO.putStrLn) suggestions
