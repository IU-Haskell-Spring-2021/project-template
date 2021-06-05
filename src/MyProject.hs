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

-- | Delete an element from a list at a given position
-- (indexes start from 1).
--
-- >>> deleteAt 2 [1..5]
-- [1,3,4,5]
-- >>> deleteAt 100 [1..5]
-- [1,2,3,4,5]
-- >>> deleteAt 1 [1..5]
-- [2,3,4,5]
deleteAt :: Int -> [a] -> [a]
deleteAt n []     = []
deleteAt 1 (_:xs) = xs
deleteAt n (x:xs) = x : deleteAt (n - 1) xs

-- | Early attempt at removeTask.
-- removeTask' :: Int -> [Task] -> State -> IO ()
-- removeTask' n before [] = runWith before
-- removeTask' 1 before (_task : tasks)
--   = runWith (before ++ tasks)
-- removeTask' n before (task : tasks)
--   = removeTask' (n - 1) (before ++ [task]) tasks

-- removeTask :: Int -> State -> IO ()
-- removeTask n state = runWith (deleteAt n state)

data Command
  = List
  | Done Int
  | Add Task
  deriving (Show)

parseCommand :: String -> Maybe Command
parseCommand input =
  case input of
    "/list" -> Just List
    '/':'d':'o':'n':'e':' ':'#':strNum ->
      case readMaybe strNum of
        Nothing -> Nothing
        Just n  -> Just (Done n)
    newTask -> Just (Add newTask)

type Handler = State -> (State, String)

listTasks :: Handler
listTasks state = (state, ppTasks state)

-- |
-- >>> putStrLn (ppTasks ["A", "B", "C"])
-- 1) A
-- 2) B
-- 3) C
ppTasks :: [Task] -> String
ppTasks tasks = unlines (map ppItem (zip [1..] tasks))
  where
    ppItem (n, task) = show n ++ ") " ++ task

-- | Same as 'unlines'.
sepByNewline :: [String] -> String
sepByNewline []           = ""
sepByNewline (line:lines) = line ++ "\n" ++ sepByNewline lines

-- |
-- >>> enumerateFrom 1 ["A", "B", "C"]
-- [(1,"A"),(2,"B"),(3,"C")]
enumerateFrom :: Int -> [a] -> [(Int, a)]
enumerateFrom n [] = []
enumerateFrom n (task:tasks)
  = (n, task) : enumerateFrom (n + 1) tasks

markTaskAsDone :: Int -> Handler
markTaskAsDone n state = (newState, "Done.")
  where
    newState = deleteAt n state

addNewTask :: Task -> Handler
addNewTask newTask state = (newState, "Added.")
  where
    newState = state ++ [newTask]

handleCommand :: Command -> Handler
handleCommand command state =
  case command of
    List        -> listTasks state
    Done n      -> markTaskAsDone n state
    Add newTask -> addNewTask newTask state

-- return :: a -> IO a
-- pure :: a -> IO a

runAll :: [IO ()] -> IO ()
runAll (program : programs) = do
  program
  return ()
  runAll programs
runAll [] = return ()

runWith
  :: (String -> Maybe command)
  -> (command -> state -> (state, String))
  -> state
  -> IO ()
runWith parse handle state = do
  putStr "> "
  input <- getLine
  case parse input of
    Nothing -> do
      putStrLn ("INVALID command: " ++ input)
      runWith parse handle state
    Just command -> do
      case handle command state of
        (newState, feedback) -> do
          putStrLn feedback
          runWith parse handle newState

run :: IO ()
run = runWith parseCommand handleCommand initialState
