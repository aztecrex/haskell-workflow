
module Azr.Experiments.Workflow where


-- data Task a =
--   Dispatch String a
--   Return a
--
--
--
--
--
--
--
-- data Dispatch =
--
--
-- data Stat = Dispatched | Completed String
--
-- data Instruction = Instruction
--
-- run :: [Instruction] -> [Stat] -> ([Instruction], [Stat])
-- run = error "NYI"
--
--
--

data Job a = Job a | Blocked deriving (Eq,Show)

data Task a = Completed a | Dispatched deriving (Eq,Show)

dispatch :: String -> Integer -> Job (Task Integer)
dispatch "double" x = Job . Completed $ 2 * x
dispatch "triple" x = Job . Completed $ 3 * x
dispatch "square" x = Job . Completed $ x * x
dispatch _ _ = error "no such dispatch"

require :: Task a -> Job a
require (Completed a) = Job a
require Dispatched = Blocked

instance Functor Job where
  fmap _ Blocked = Blocked
  fmap f (Job x) = Job (f x)

instance Applicative Job where
  pure = Job
  Blocked <*> _ = Blocked
  _ <*> Blocked = Blocked
  Job f <*> Job x = Job (f x)

instance Monad Job where
  Blocked >>= _ = Blocked
  Job x >>= f = f x

job :: Integer -> Job Integer
job x = do
  ta <- dispatch "double" x
  tb <- dispatch "triple" x
  a <- require ta
  tc <- dispatch "square" a
  c <- require tc
  b <- require tb
  return $ c + b



-- data Task a = Completed a || Waiting
--
-- dispatch :: a -> Thing a
-- dispatch _ = Waiting
--
-- wait :: Task a -> Maybe a
-- wait Waiting = Nothing
-- wait (Completed x) = Just x
--
--
--
--
-- dispatch :: String -> Maybe String
-- fakeDispatch = error "dispatch is not a real thing"
--
-- fakeRequire :: String -> Maybe ()
-- fakeRequire = error "fake require is not a real thing"
--
-- fake :: Maybe String
-- fake = do
--     a <- fakeDispatch "message 1"
--     b <- fakeDispatch "message 2"
--     c <- fakeDispatch "message 3"
--     fakeRequire c
--     return $ a ++ b
