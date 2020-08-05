module Interpreter where

import Parser
import qualified Data.Map as Map
import Control.Monad.State

type Env = Map.Map String Int

eval :: Expr -> State Env Int
eval = undefined
