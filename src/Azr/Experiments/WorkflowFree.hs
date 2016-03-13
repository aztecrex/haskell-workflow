{-# LANGUAGE StandaloneDeriving #-}
module Azr.Experiments.WorkflowFree where

import Control.Monad.Free

data WorkF t a r next =
    Dispatch t a (Maybe r -> next)
  | Require (Maybe r) (r -> next)

instance Functor (WorkF t a r) where
  fmap f (Dispatch t a g) = Dispatch t a (f . g)
  fmap f (Require ma g) = Require ma (f . g)

dispatch :: t -> a -> Free (WorkF t a r) (Maybe r)
dispatch t a = liftF $ Dispatch t a id

require :: Maybe r -> Free (WorkF t a r) r
require mr = liftF $ Require mr id

type IntWork = Free (WorkF String Integer Integer)

data Task a = Completed a | Dispatched String a deriving (Eq,Show)
type Status = (Int, [Task Integer])

dispatchTask :: Status -> String -> Integer -> (Status, Maybe Integer)
dispatchTask (used, ts) op arg = (((used+1),ts'), p)
  where
    (ts',p)
      | length ts > used = (ts, promise $ reverse ts !! used)
      | otherwise = (Dispatched op arg : ts, Nothing)
    promise (Completed a) = Just a
    promise (Dispatched _ _) = Nothing

run :: Status -> IntWork r -> Either [Task Integer] r
run _ (Pure r) = Right r
run st (Free (Dispatch t a f)) = run st' $ f p
  where (st',p) = dispatchTask st t a
run st@(_,ts) (Free (Require ma f)) =
  case ma of
    Nothing -> Left ts
    Just a -> run st $ f a



job :: Integer -> IntWork Integer
job x = do
  ta <- dispatch "double" x
  tb <- dispatch "triple" x
  a <- require ta
  tc <- dispatch "square" a
  td <- dispatch "unknown" a
  c <- require tc
  b <- require tb
  require td    -- <----- uncomment this to prevent completion
  return $ c + b

initr :: Either [Task Integer] Integer
initr = Left []

runWork :: Either [Task Integer] Integer -> Either [Task Integer] Integer
runWork from = case from of
  (Left ts) -> run (0,fmap resolve ts) (job 3)
  _ -> from


v1 = runWork initr
v2 = runWork v1
v3 = runWork v2
v4 = runWork v3
v5 = runWork v4

resolve :: Task Integer -> Task Integer
resolve (Dispatched op x) = case op of
        "double" -> Completed $ 2 * x
        "triple" -> Completed $ 3 * x
        "square" -> Completed $ x * x
        _        -> Dispatched op x
resolve other = other
