{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Console.Fuseline
  ( Config(..)
  , Repl(..)
  , replLoop
  , runReplIO
  )
where

import           Prelude                                  ( (.)
                                                          , ($)
                                                          , undefined
                                                          )
import           Control.Applicative                      ( Applicative
                                                          , pure
                                                          )
import           Control.Effect                           ( Member
                                                          , run
                                                          , send
                                                          )
import           Control.Effect.Carrier                   ( Effect(..)
                                                          , Carrier(..)
                                                          , HFunctor(..)
                                                          , handleCoercible
                                                          )
import           Control.Effect.Error                     ( Error
                                                          , throwError
                                                          , catchError
                                                          )
import           Control.Effect.State                     ( State
                                                          , get
                                                          , put
                                                          )
import           Control.Effect.Sum                       ( (:+:)(..) )
import           Control.Monad                            ( Monad
                                                          , (>>)
                                                          , (>>=)
                                                          )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Data.Either                              ( Either(..) )
import           Data.Functor                             ( Functor
                                                          , (<$)
                                                          , fmap
                                                          )
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as Text
import           System.Exit                              ( exitFailure
                                                          , exitSuccess
                                                          )


data Config e s v = Config { _banner :: Text
                         , _interpreter :: s -> Text -> Either e v
                         , _showError :: e -> Text
                         , _showValue :: v -> Text
                         , _goodbye :: Text
                         , _initialState :: s
                         , _updateState :: s -> v -> s
                         }

data Repl (m :: * -> *) k
  = ShowBanner Text k
  | Read (Text -> k)
--  | forall e v . Eval (Text -> Either e v) Text k
  | forall v . Print (v -> Text) v k
  | Interrupt k
  | Quit Text k

deriving instance Functor (Repl m)

instance HFunctor Repl where
  hmap :: (forall x . m x -> n x) -> Repl m k -> Repl n k
  hmap _ (ShowBanner banner k    ) = ShowBanner banner k
  hmap _ (Read k                 ) = Read k
  hmap _ (Print showValue value k) = Print showValue value k
  hmap _ (Interrupt k            ) = Interrupt k
  hmap _ (Quit goodbye k         ) = Quit goodbye k

instance Effect Repl where
  handle
    :: Functor f
    => f ()
    -> (forall x . f (m x) -> n (f x))
    -> Repl m (m b)
    -> Repl n (n (f b))
  handle state handler (ShowBanner banner k) =
    ShowBanner banner (handler (k <$ state))
  handle state handler (Read k) = Read (handler . fmap (<$ state) k)
  handle state handler (Print showValue value k) =
    Print showValue value (handler (k <$ state))
  handle state handler (Interrupt k   ) = Interrupt (handler (k <$ state))
  handle state handler (Quit goodbye k) = Quit goodbye (handler (k <$ state))

showBanner :: (Member Repl sig, Carrier sig m) => Text -> m ()
showBanner banner = send (ShowBanner banner (pure ()))

read :: (Member Repl sig, Carrier sig m) => m Text
read = send (Read pure)

print :: (Member Repl sig, Carrier sig m) => (v -> Text) -> v -> m ()
print showValue value = send (Print showValue value (pure ()))

interrupt :: (Member Repl sig, Carrier sig m) => m ()
interrupt = send (Interrupt (pure ()))

quit :: (Member Repl sig, Carrier sig m) => Text -> m ()
quit message = send (Quit message (pure ()))

newtype ReplIOC m a = ReplIOC { runReplIOC :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Carrier sig m, MonadIO m) => Carrier (Repl :+: sig) (ReplIOC m) where
  eff (L (ShowBanner banner k)) =
    ReplIOC $ liftIO (Text.putStrLn banner) >> runReplIOC k
  eff (L (Read k)) = ReplIOC (liftIO Text.getLine >>= runReplIOC . k)
  eff (L (Print showValue value k)) =
    ReplIOC $ liftIO (Text.putStrLn (showValue value)) >> runReplIOC k
  eff (L (Interrupt _)) = ReplIOC (liftIO exitFailure)
  eff (L (Quit message _)) =
    ReplIOC (liftIO (Text.putStrLn message >> exitSuccess))
  eff (R other) = ReplIOC (eff (handleCoercible other))

runReplIO :: ReplIOC m a -> m a
runReplIO = runReplIOC

replLoop
  :: ( Member (Error e) sig
     , Member (State s) sig
     , Member Repl sig
     , Carrier sig m
     )
  => Config e s v
  -> m ()
replLoop config = put initialState >> loop
 where
  loop = do
    showBanner banner
    minput <- read
    case minput of
      ""    -> loop
      input -> do
        state <- get
        case _interpreter config state input of
          Left  err   -> throwError err
          Right value -> do
            put $ updateState state value
            print showValue value
            loop

  initialState = _initialState config
  banner       = _banner config
  updateState  = _updateState config
  showValue    = _showValue config
