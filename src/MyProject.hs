module MyProject where

import           Text.Read

type State = [Task]

type Task = String

initialState :: State
initialState = []

listAllTasks :: State -> IO ()
listAllTasks = listAllTasks' 1

listAllTasks' :: Int -> State -> IO ()
listAllTasks' _ [] = putStrLn ""
listAllTasks' n (task:tasks) = do
  putStr (show n ++ ") ")
  putStrLn task
  listAllTasks' (n + 1) tasks

removeTask :: Int -> State -> IO ()
removeTask _ state = runWith state

runWith :: State -> IO ()
runWith state = do
  putStr "> "
  input <- getLine
  case input of
    "/list" -> do
       listAllTasks state
       runWith state

    '/':'d':'o':'n':'e':' ':'#':strNum -> do
      case readMaybe strNum of
        Nothing -> do
          putStrLn ("INVALID task number: " ++ strNum)
          runWith state
        Just n -> do
          removeTask n state
    _ -> do
      runWith (state ++ [input])

run :: IO ()
run = runWith initialState

factorial :: Integer -> Integer
factorial n = f 1 n
  where
    f s n
      | n <= 0    = s
      | otherwise = f (s * n) (n - 1)
