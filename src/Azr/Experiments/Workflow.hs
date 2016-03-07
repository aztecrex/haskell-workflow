
module Azr.Experiments.Workflow where

import Control.Monad.State.Strict

data Task a = Completed a | Dispatched String a deriving (Eq,Show)

data Promise = Index Int deriving (Eq, Show)

type Status = (Int, [Task Integer])

update :: Task Integer -> Status -> Status
update t (pos,ts) = (pos + 1, ts')
  where ts' | length ts == pos = t : ts
            | length ts > pos  = ts
            | otherwise        = error $ "invariant violation " ++ show (pos,ts)

data Job a = Job a | Blocked [Task Integer] deriving (Eq,Show)

type Program = StateT Status Job

dispatch :: String -> Integer -> Program (Promise)
dispatch name arg = do
  idx <- gets fst
  modify $ update (Dispatched name arg)
  return $ Index idx


require :: Promise -> Program Integer
require (Index i) = do
  task <- gets $ (!! i) . reverse . snd
  case task of
    (Dispatched _ _) -> do
      (_,ts) <- get
      lift $ Blocked ts
    (Completed v) -> return v

instance Functor Job where
  fmap _ (Blocked ts) = Blocked ts
  fmap f (Job x) = Job (f x)

instance Applicative Job where
  pure = Job
  Blocked ts <*> _ = Blocked ts
  _ <*> Blocked ts = Blocked ts
  Job f <*> Job x = Job (f x)

instance Monad Job where
  Blocked ts >>= _ = Blocked ts
  Job x >>= f = f x

job :: Integer -> Program Integer
job x = do
  ta <- dispatch "double" x
  tb <- dispatch "triple" x
  a <- require ta
  tc <- dispatch "square" a
  td <- dispatch "unknown" a
  c <- require tc
  b <- require tb
  -- require td    <----- uncomment this to prevent completion
  return $ c + b

initr :: Job Integer
initr = Blocked []

runJob :: Job Integer -> Job Integer
runJob from = case from of
  (Blocked ts) -> evalStateT (job 3) (0, fmap resolve ts)
  _            -> from

v1 = runJob initr
v2 = runJob v1
v3 = runJob v2
v4 = runJob v3
v5 = runJob v4

resolve :: Task Integer -> Task Integer
resolve (Dispatched op x) = case op of
        "double" -> Completed $ 2 * x
        "triple" -> Completed $ 3 * x
        "square" -> Completed $ x * x
        _        -> Dispatched op x
resolve other = other
