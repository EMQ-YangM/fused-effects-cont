{-# LANGUAGE DeriveFunctor #-}

module Cont where

import Control.Carrier.State.Strict
import Control.Monad
import Control.Monad.IO.Class
import System.IO

newtype ContT r m a = ContT {runContT :: (m a -> m r) -> m r}

evalContT :: (Monad m) => (m a -> m r) -> ContT r m a -> m r
evalContT f m = runContT m f

instance Functor m => Functor (ContT r m) where
  fmap f m = ContT $ \c -> runContT m (c . fmap f)

instance Applicative m => Applicative (ContT r m) where
  pure x = ContT ($ pure x)
  f <*> v = ContT $ \c -> runContT f $ \g -> runContT v (\tmp -> c (g <*> tmp))

-- never del this
-- res <- runContT <$> (k <$> x)
-- res c
instance (Monad m) => Monad (ContT r m) where
  m >>= k = ContT $ \c -> runContT m ((<$>) k >=> (($ c) . runContT))

instance MonadIO m => MonadIO (ContT r m) where
  liftIO io = ContT $ \c -> c (liftIO io)

callCC1 :: ((m a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC1 f = ContT $ \c -> runContT (f (\x -> ContT $ \_ -> c x)) c

callCC :: Applicative m => ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC f = ContT $ \c -> runContT (f (\x -> ContT $ \_ -> c (pure x))) c

type CC r1 r s a = ContT r1 (StateC Int (ContT r (StateC s IO))) a

main = do
  hSetBuffering stdout NoBuffering
  runContT (callCC askString) $ \r -> do
    reportResult r
    reportResult r
    reportResult r
    reportResult r

askString :: (String -> ContT () IO String) -> ContT () IO String
askString next = do
  liftIO $ putStrLn "Please enter a string"
  s <- liftIO $ getLine
  next s
  liftIO $ putStrLn "Please enter a string"
  liftIO $ putStrLn "Please enter a string"
  liftIO $ putStrLn "Please enter a string"
  return "hello"

reportResult :: IO String -> IO ()
reportResult s' = do
  s <- s'
  putStrLn ("You entered: " ++ s)
