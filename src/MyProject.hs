module MyProject where

import           Text.Read (readMaybe)

type Task = String

type State = [Task]

-- return :: a -> IO a

type Handler = State -> (String, State)

listTasks :: Handler
listTasks []    = ("There are no tasks :)", [])
listTasks tasks = (listTasks' tasks, tasks)

listTasks' :: State -> String
listTasks' [] = ""
listTasks' (task:tasks)
  = "* " ++ task ++ "\n" ++ output
  where
    output = listTasks' tasks

markAsDone :: Int -> Handler
markAsDone _ [] = ("There are no tasks!", [])
markAsDone 0 (task:tasks) =
  ("Marked task as done: " ++ task, tasks)
markAsDone n (task:tasks) =
  (output, task:tasks')
  where
    (output, tasks') = markAsDone (n - 1) tasks

addTask :: Task -> Handler
addTask task tasks
  = ("New task: " ++ task, task : tasks)

data Command
  = Exit
  | List
  | Done Int
  | Add Task
  deriving (Show, Read, Eq, Ord)

parseCommand :: String -> Maybe Command
parseCommand input =
  case input of
    "/exit" -> Just Exit
    "/list" -> Just List
    '/':'d':'o':'n':'e':' ':arg -> do
      case readMaybe arg of
        Nothing -> Nothing
        Just n  -> Just (Done n)
    task -> Just (Add task)

handleCommand :: Command -> Maybe Handler
handleCommand Exit       = Nothing
handleCommand List       = Just listTasks
handleCommand (Done n)   = Just (markAsDone n)
handleCommand (Add task) = Just (addTask task)

runWith :: State -> IO ()
runWith tasks = do
  putStr "[user]: "
  input <- getLine
  case parseCommand input of
    Nothing -> do
      putStrLn "Failed to parse the command!"
      runWith tasks
    Just command -> do
      case handleCommand command of
        Nothing -> putStrLn "Bye!"
        Just handler -> do
          let (output, newTasks) = handler tasks
          putStrLn output
          runWith newTasks

taskerOf
  :: state
  -> (String -> Maybe command)
  -> (command -> Maybe (state -> (String, state)))
  -> IO ()
taskerOf state parse handle = do
  putStr "[user]: "
  input <- getLine
  case parse input of
    Nothing -> do
      putStrLn "Failed to parse the command!"
      taskerOf state parse handle
    Just command -> do
      case handle command of
        Nothing -> putStrLn "Bye!"
        Just handler -> do
          let (output, newState) = handler state
          putStrLn output
          taskerOf newState parse handle

run :: IO ()
run = taskerOf [] parseCommand handleCommand
