import Text.Printf
import Data.List
import Data.Char
import System.Random
import HaskellManWords (haskellManWords)

main :: IO ()
main = do answerTuple <- pick haskellManWords
          let answer = fst answerTuple
          let clue = snd answerTuple
          printf "\nSo, you want to play HASKELLMAN?!\n\nClue: %s\n" clue          
          haskellMan 10 (take (length answer) $ repeat '_') "" answer
          return ()

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

haskellMan :: Int -> String -> String -> String -> IO ()
haskellMan guessesRemaining phrase guessedChars answer =
    case guessesRemaining of
      0 -> printf "\nYou\'re out of guesses. The answer was %s.\n" answer
      _ -> do printf prompt phrase guessesRemaining             
              input <- getSingleChar
              case input of
                Left s -> putStrLn s >> haskellMan guessesRemaining phrase guessedChars answer
                Right c -> updateGameState c guessesRemaining phrase guessedChars answer
                
prompt :: String
prompt = "\nYour answer is currently \"%s\" \
          \\n[Incorrect guesses remaining: %i]\
          \\n\nEnter a character:"
          
getSingleChar :: IO (Either String Char)
getSingleChar = do
    line <- getLine
    case line of
      [a] -> if isAlphaNum a then return (Right $ toLower a)
                             else return $ Left "Character must be a letter or a number!"
      [] -> return $ Left "You must enter one character!"
      _ -> return $ Left "You must only enter one character!" 
                                     
updateGameState :: Char -> Int -> String -> String -> String -> IO ()
updateGameState c guessesRemaining phrase guessedChars answer =
    case (isNewChar c guessedChars) of
      True ->  do let newPhrase = matchPhrase c phrase answer
                  case (not $ '_' `elem` newPhrase) of
                    True -> printf "\nThat's right, the answer is %s!\n" answer
                    False -> haskellMan (calcGuesses guessesRemaining newPhrase phrase) 
                                        newPhrase
                                        (c:guessedChars)
                                        answer
      False -> putStrLn "You've already guessed that one." >> haskellMan guessesRemaining phrase guessedChars answer
      
isNewChar :: Char -> String -> Bool
isNewChar input guessedChars = not (input `elem` guessedChars)
     
matchPhrase :: Char -> String -> String -> String
matchPhrase g p a = foldl (\acc x -> replaceAtIndex x g acc) p (g `elemIndices` a)

replaceAtIndex :: (Num a, Eq a) => a -> Char -> String -> String
replaceAtIndex _ _ [] = []
replaceAtIndex i newVal (x:xs)
     | i == 0 = newVal:xs
     | otherwise = x:replaceAtIndex (i-1) newVal xs
     
calcGuesses :: Int -> String -> String -> Int
calcGuesses guesses newPhrase oldPhrase = guesses - (boolToInt $ newPhrase == oldPhrase)

boolToInt :: Bool -> Int
boolToInt b
    | b == True = 1
    | otherwise = 0