module SpaceSloths.Task where

import Protolude

data TaskType
  = MoveTo Int Int Int
  deriving Show

data Task
  = Task
  { _taskName :: String
  , _taskType :: TaskType
  } deriving Show
