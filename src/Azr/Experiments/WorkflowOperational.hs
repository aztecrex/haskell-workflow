{-# LANGUAGE GADTs #-}

module Azr.Experiments.WorkflowOperational where

import Control.Monad.Operational

data WorkI t a r where
  Dispatch :: t -> a -> WorkI t a (Maybe r)
  Require :: Maybe r -> WorkI t a r

type Work t a r = Program (WorkI t a) r



-- type Job s a = Program (WorkI s) a

-- progress :: Job a ->  Maybe a
-- progress = eval . view
--   where
--   eval ::
