module SpaceSloths.Sloth where

import Protolude
-- import SpaceSloths.Task (TaskPriority, Task)

-- class Sim s where
--   update :: s -> s
-- 
-- | Sloth is an intelligent being who has a mind of his own.
data Sloth
  = Sloth
  { _name :: Text
  , _position :: (Int, Int)
  , _render :: Char
  } deriving Show
--   , _curTask :: Task
--   , _taskBuf :: [Task]
--   , _needs :: [Task]              -- ^ Most important tasks
--   , _tasks :: [Task]              -- ^ Tasks from the jobs requested
--   , _wants :: [Task]              -- ^ Will do if has time
--   } deriving Show

-- getNextTask = 
-- 
-- instance Sim Sloth where
--   update Sloth {..} =
--     case _curTask of
--       MoveTo target ->
--         pth = pathFind _position target
--         let next = head pth
--         in if next /= target
--           then Sloth { _position = next, .. }
--           else Sloth { _position = next, _curTask = getNextTask _taskBuf _needs _tasks _wants, .. }
