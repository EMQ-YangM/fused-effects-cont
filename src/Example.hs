{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import Control.Algebra
import Control.Effect.Labelled
import Control.Monad.Trans.Cont
import Data.Kind

data CC r (m :: Type -> Type) a where
  CC :: ((a -> m b) -> m a) -> CC r m a

cc :: HasLabelled CC (CC r) sig m => ((a -> m b) -> m a) -> m a
cc = sendLabelled @CC . CC

newtype CCC r m a = CCC {runCCC :: ContT r m a}
  deriving (Functor, Applicative, Monad, MonadFail)

-- instance Algebra sig m => Algebra (CC r :+: sig) (CCC r m) where
--   alg hdl sig ctx = CCC $ case sig of
--     L (CC f) -> runCCC $ hdl (callCC f <$ ctx)
--     R other -> undefined --  alg (undefined . runCCC . hdl) other ctx

-- fn (ContT r) = ContT $ \go -> r go
