module MyProject where

import           Data.List (intercalate)
import           Text.Read (readMaybe)

-- getLine :: IO String

type Task = String

type State = [Task]

-- factorial 0 = 1
-- factorial n = n * factorial (n - 1)

-- map :: (a -> b) -> [a] -> [b]
-- ... :: Task -> IO ()
-- tasks :: [Task]
-- map ... :: [Task] -> [IO ()]
-- map ... tasks :: [IO ()]

-- Bool
-- Int
--
-- IO a
-- IO ()
-- IO Int
-- IO String
--
-- [a]
-- [Int]
-- [String]
-- [()]
-- HashMap k v

-- return :: () -> IO ()
-- return :: a -> IO a

doList :: [IO ()] -> IO ()
doList [] = pure ()
doList (program:programs) = do
  program
  doList programs

showTasks :: State -> IO ()
showTasks [] = putStrLn "No tasks!"
showTasks tasks = doList
  (map (\task -> putStrLn ("* " ++ task)) tasks)

markTaskAsDone :: Int -> State -> IO State
markTaskAsDone i tasks
  | i < 0 || i > n - 1 = do
    putStrLn "Index is out of bounds!"
    return tasks
  | otherwise = do
      putStrLn "Marked as done!"
      return (take i tasks ++ drop (i + 1) tasks)
  where
    n = length tasks

-- data Maybe a = Nothing | Just a

type Handler = State -> (String, State)

showTasks' :: Handler
showTasks' tasks = (output, tasks)
  where
    output = intercalate "\n" (map ("* " ++ ) tasks)

markTaskAsDone' :: Int -> Handler
markTaskAsDone' i tasks
  | i < 0 || i > n - 1 = ("Index is out of bounds!", tasks)
  | otherwise          = ("Marked as done!", newTasks)
  where
    n = length tasks
    newTasks = take i tasks ++ drop (i + 1) tasks

addTask :: Task -> Handler
addTask task tasks = ("New task: " ++ task, task : tasks)

data Command
  = Exit
  | Show
  | Done Int
  | Add Task
  deriving (Show)

parseCommand :: String -> Maybe Command
parseCommand input =
  case input of
    "/exit" -> Just Exit
    "/show" -> Just Show
    '/':'d':'o':'n':'e':' ':arg ->
      case readMaybe arg of
        Nothing -> Nothing
        Just n  -> Just (Done n)
    task -> Just (Add task)

handleCommand :: Command -> Maybe Handler
handleCommand command =
  case command of
    Exit     -> Nothing
    Show     -> Just (showTasks')
    Done n   -> Just (markTaskAsDone' n)
    Add task -> Just (addTask task)

runWith :: State -> IO ()
runWith tasks = do
  putStr "[user]: "
  input <- getLine
  case parseCommand input of
    Nothing -> putStrLn "Cannot parse command!"
    Just command -> do
      case handleCommand command of
        Nothing -> putStrLn "Bye!"
        Just handler -> do
          let (output, newTasks) = handler tasks
          putStrLn output
          runWith newTasks

runWith'
  :: state
  -> (String -> Maybe command)
  -> (command -> Maybe (state -> (String, state)))
  -> IO ()
runWith' tasks parse handle = do
  putStr "[user]: "
  input <- getLine
  case parse input of
    Nothing -> putStrLn "Cannot parse command!"
    Just command -> do
      case handle command of
        Nothing -> putStrLn "Bye!"
        Just handler -> do
          let (output, newTasks) = handler tasks
          putStrLn output
          runWith' newTasks parse handle

run :: IO ()
run = runWith []
