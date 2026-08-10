module CourseworkOne where

import Backwords.Types
import Backwords.WordList

import Data.List
import Data.Char
import Data.Ratio
import Data.Bool (Bool(False, True))
import Data.Maybe (Maybe(Nothing))
import GHC.Num (integerMod)
import GHC.Read (list)
import Data.Bits (Bits(xor))
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- This file is your complete submission for the first coursework of CS141.
-- 
-- USER ID: -------
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------

-- Ex. 1: 
-- Read the spec to find out what goes here.
instance Display Char where
    -- surrounds letter in a 5x3 box, casts to lowercase so both upper & lower case
    -- is accepted
    display a = "+---+\n| " ++ [toUpper a] ++ " |\n+---+"

-- Ex. 2:
-- if string is empty then return "". otherwise, separate the display for each letter into their lines allLines :: [[String]]
-- then get each line by getting the respective part of each letter and connecting them :: [String]
-- then output the combination of these lines
instance Display [Char] where
    display "" = ""
    display x = unwords (map head allLines) ++ "\n" ++ unwords (map (head . tail) allLines) ++ "\n" ++ unwords (map (head . tail . tail) allLines)
        where
            allLines = map (lines . display) x

-- Ex. 3:
-- Determine if a word is valid.
isValidWord :: String -> Bool
-- casts to lowercase so that both upper & lower case is accepted, then outputs
-- true if word is equal to value in allWords list. lowerWord is stored so can
-- be reused & not repeatedly calculated for every item in allWords
isValidWord word = any (== lowerWord) allWords
    where
        lowerWord = map toLower word

-- Ex. 4:
-- Determine the points value of a letter.
letterValue :: Char -> Int
-- casts to lowercase so both upper and lower case is accepted, then map
-- letter to a value 1-10 (n to catch non-letter inputs)
letterValue x = case toLower x of
    'a' -> 1
    'b' -> 3
    'c' -> 3
    'd' -> 2
    'e' -> 1
    'f' -> 4
    'g' -> 2
    'h' -> 4
    'i' -> 1
    'j' -> 8
    'k' -> 5
    'l' -> 1
    'm' -> 3
    'n' -> 1
    'o' -> 1
    'p' -> 3
    'q' -> 10
    'r' -> 1
    's' -> 1
    't' -> 1
    'u' -> 1
    'v' -> 4
    'w' -> 4
    'x' -> 8
    'y' -> 4
    'z' -> 10
    n -> 0

-- Ex. 5:
-- Score a word according to the zBackwords scoring system.
scoreWord :: String -> Int
-- empty string scores 0, recursive function for letter scoring
scoreWord [] = 0
scoreWord [x] = letterValue x
scoreWord (x:xs) = letterValue x + 2 * scoreWord xs


-- Ex. 6:
-- Get all words that can be formed from the given letters.
possibleWords :: String -> [String]
-- gets every element in allWords list that is a subset of possibleWords using a helper
-- function called isPossibleWord (takes given letters and element of allWords, removes
-- matching letters from both, if element is empty first then True otherwise False)
possibleWords x = filter (isPossibleWord x) allWords
    where
        isPossibleWord _ [] = True
        isPossibleWord [] _ = False
        isPossibleWord str (y:ys)
            | y `elem` str = isPossibleWord (delete y str) ys
            | otherwise    = False

-- Ex. 7:
-- Given a set of letters, find the highest scoring word that can be formed from them.
bestWord :: String -> Maybe String
-- returns a Maybe String (Nothing or Just String), finds a highest score by finding
-- maximum scoreWord from possible words, then finds the possible word with this score
-- (if multiple are found pick the one with the shortest length)
bestWord [] = Nothing
bestWord word
    | null (possibleWords word) = Nothing
    | otherwise                 = Just $ minimumBy (comparing length) (filter compareToHighest (possibleWords word))
    where
        compareToHighest x = scoreWord x  == highestScore word
        highestScore word  = maximum $ map scoreWord (possibleWords word)

-- Ex. 8:
-- Given a list of letters, and a word, mark as used all letters in the list that appear in the word.
useTiles :: String -> String -> [Tile]
-- recursive function that goes through every letter and marks it as Used if it also
-- occurs in the word, otherwise Unused (if it is in both Strings then remove letter
-- from the word to prevent all occurences of same letter being marked as Used)
useTiles [] _ = []
useTiles (x:xs) usedTiles
      | x `elem` usedTiles = Used x : useTiles xs (delete x usedTiles)
      | otherwise          = Unused x : useTiles xs usedTiles

-- Ex. 9
-- Given a nonempty bag of possible letters as a list, return the chance of drawing 
-- each letter.
bagDistribution :: String -> [(Char, Rational)]
-- uses helper functions to provide chance of drawing each letter by (occurences of each letter % total bag length)
-- uses nub to provide single entry for each letter rather than an entry for each occurence of the letter
bagDistribution list = nub (map (letterDistribution list) list)
    where
        letterDistribution list item = (item, fromIntegral (countInBag 0 list item ) % fromIntegral (length list))
        countInBag amount list item
            | item `elem` list = countInBag (amount+1) (delete item list) item
            | otherwise        = amount


-- Ex. 10:
-- Write an AI which plays the Backwords game as well as possible.
aiMove :: String -> String -> Move
aiMove bag rack
    | length rack < 9 = pickTile bag rack
    | otherwise = playWord rack

-- not enough consonants (less than 4%3 ratio in rack) then pick consonant, otherwise pick a vowel. also handles
-- other situations e.g. bag only contains vowels (take vowel), only contains consonants (take consonant), contains
-- neither (will never occur, take consonant) 
pickTile bag rack
    | containsVowels bag && containsConsonants bag =
        if countVowels rack == 0
            then TakeVowel
            else if countConsonants rack % countVowels rack <= 4 % 3
                then TakeConsonant
                else TakeVowel
    | containsVowels bag     = TakeVowel
    | containsConsonants bag = TakeConsonant
    | otherwise              = TakeConsonant
    where
        containsVowels word     = countVowels word /= 0
        containsConsonants word = countConsonants word /= 0

countConsonants word = length (filter (`elem` consonants) word)
countVowels word = length (filter (`elem` vowels) word)

playWord rack = PlayWord (removeMaybe (newBestWord rack))
    where
        removeMaybe (Just x) = x
        removeMaybe Nothing  = ""

-- same logic as previous bestWord function but calculates score of words using doubleNewScore instead of scoreWord
newBestWord [] = Nothing
newBestWord word
    | null (possibleWords word) = Nothing
    | otherwise                 = Just $ minimumBy (comparing length) (filter compareToHighest (possibleWords word))
    where
        compareToHighest x = doubleNewScore word x  == highestScore word
        highestScore word  = maximum $ map (doubleNewScore word) (possibleWords word)
    
-- calculates score of word by seeing how many vowels it would leave behind in the rack (being too greedy with vowels
-- or not using enough reduces the score by 30 points). uses helper function removeWord that replicates delete function
-- but in a string rather than char
newScoreWord rack pickedWord
    | null (removeWord rack pickedWord) = scoreWord pickedWord
    | ratioVowels < 2 % 9               = scoreWord pickedWord - 30
    | ratioVowels > 5 % 9               = scoreWord pickedWord - 30
    | otherwise                         = scoreWord pickedWord
    where
        ratioVowels = countVowels (removeWord rack pickedWord) % length (removeWord rack pickedWord)

-- calculates score of word by subtracting 40 from newScoreWord if the rack contains a high scoring letter which is
-- not used, as playing high scoring letters may be harder later on into the bag/game
doubleNewScore rack pickedWord
    | highScorer (removeWord rack pickedWord) /= ' ' = newScoreWord rack pickedWord - 40
    | otherwise                                      = newScoreWord rack pickedWord
    where
        highScorer [] = ' '
        highScorer (x:xs)
            | x `elem` "xzqj" = x
            | otherwise       = highScorer xs    

removeWord rack "" = rack
removeWord rack (x:xs)
    | x `elem` rack = removeWord (delete x rack) xs
    | otherwise     = error "Wrong input"