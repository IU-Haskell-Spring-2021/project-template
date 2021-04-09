module MyProject where

import           Text.Read (readMaybe)

f :: Int -> Int
f = (+2)

type Task = String

type Tasks = [Task]

showTasks :: Tasks -> String
showTasks [] = ""
showTasks (task:tasks) =
  showTasks tasks ++ "\n" ++
  show (1 + length tasks) ++ ". " ++ task

-- printTasks :: Tasks -> IO ()
-- printTasks [] = putStrLn ""
-- printTasks (task:tasks) = do
--   printTasks tasks
--   putStrLn (show (1 + length tasks) ++ ". " ++ task)

markAsDone :: Int -> Tasks -> Tasks
markAsDone _ [] = []
markAsDone 1 (_task:tasks) = tasks
markAsDone i (task:tasks) =
  task : markAsDone (i - 1) tasks

data Command
  = List
  | Done Int
  | Add Task
  deriving (Show)

handleCommand
  :: Command -> Tasks -> (Tasks, Maybe String)
handleCommand command tasks =
  case command of
    List     -> (tasks, Just (showTasks tasks))
    Done i   -> (markAsDone i tasks, Nothing)
    Add task -> (task : tasks, Nothing)

parseCommand :: String -> Maybe Command
parseCommand input =
  case input of
    "/list" -> Just List
    '/':'d':'o':'n':'e':' ':indexStr ->
      case readMaybe indexStr of
        Nothing    -> Nothing
        Just index -> Just (Done index)
    task -> Just (Add task)

runWith
  :: (String -> Maybe command)
  -> (command -> state -> (state, Maybe String))
  -> state
  -> IO ()
runWith parse handle = loop
  where
    loop tasks = do
      putStr "[Enter command]> "
      input <- getLine
      case input of
        "/exit" -> putStrLn "Bye!"
        _ ->
          case parse input of
            Nothing -> do
              putStrLn "ERROR: Invalid command!"
              loop tasks
            Just command -> do
              let (newTasks, output) = handle command tasks
              case output of
                Nothing      -> pure ()
                Just output' -> putStrLn output'
              loop newTasks

run :: IO ()
run = runWith parseCommand handleCommand []
