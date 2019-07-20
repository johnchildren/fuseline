{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module System.Console.Fuseline
  ( Config(..)
  , ReplCore(..)
  , ReplValue(..)
  , ReplIOC(..)
  , ReplExcept(..)
  , runRepl
  , showBanner
  , read
  , eval
  , print
  , interrupt
  , handleInterrupt
  , kill
  , quit
  )
where

import           Control.Applicative                      ( Applicative
                                                          , pure
                                                          , (<$)
                                                          )
import           Control.Effect                           ( Member )
import           Control.Effect.Carrier                   ( Effect(..)
                                                          , Carrier(..)
                                                          , HFunctor(..)
                                                          , handleCoercible
                                                          , send
                                                          )
import           Control.Effect.Error                     ( ErrorC
                                                          , throwError
                                                          , catchError
                                                          , runError
                                                          )
import           Control.Effect.Reader                    ( ReaderC
                                                          , asks
                                                          , runReader
                                                          )
import           Control.Effect.State                     ( StateC
                                                          , get
                                                          , put
                                                          , runState
                                                          )
import           Control.Effect.Sum                       ( (:+:)(..) )
import           Control.Exception                        ( Exception )
import           Control.Monad                            ( Monad
                                                          , (>>)
                                                          , (>>=)
                                                          )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Data.Either                              ( Either(..) )
import           Data.Eq                                  ( Eq )
import           Data.Function                            ( (.)
                                                          , ($)
                                                          , (&)
                                                          )
import           Data.Functor                             ( Functor
                                                          , fmap
                                                          )
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as Text
import           GHC.Generics                             ( Generic1 )
import           System.Exit                              ( exitFailure
                                                          , exitSuccess
                                                          )
import           Text.Show                                ( Show )

--------------------------------------------------------------------------------
data Config s e v = Config
   { _banner :: Text
   , _interpreter :: s -> Text -> Either e v
   , _showError :: e -> Text
   , _showValue :: v -> Text
   , _goodbye :: Text
   , _updateState :: s -> v -> s
   }
--------------------------------------------------------------------------------
data ReplCore (m :: * -> *) k
  = ShowBanner (m k)
  | Read (Text -> m k)
  | forall b . HandleInterrupt (m b) (InterruptExcept -> m b) (b -> m k)
  | Interrupt (m k)
  | Kill (m k)
  | Quit (m k)

deriving stock instance Functor m => Functor (ReplCore m)

instance HFunctor ReplCore where
  hmap f (ShowBanner k         ) = ShowBanner (f k)
  hmap f (Read       k         ) = Read (f . k)
  hmap f (HandleInterrupt m h k) = HandleInterrupt (f m) (f . h) (f . k)
  hmap f (Interrupt k          ) = Interrupt (f k)
  hmap f (Kill      k          ) = Kill (f k)
  hmap f (Quit      k          ) = Quit (f k)

instance Effect ReplCore where
  handle state handler (ShowBanner k) = ShowBanner (handler (k <$ state))
  handle state handler (Read k) = Read (handler . (<$ state) . k)
  handle state handler (HandleInterrupt m h k) = HandleInterrupt
    (handler (m <$ state))
    (handler . (<$ state) . h)
    (handler . fmap k)
  handle state handler (Interrupt k) = Interrupt (handler (k <$ state))
  handle state handler (Kill      k) = Kill (handler (k <$ state))
  handle state handler (Quit      k) = Quit (handler (k <$ state))

showBanner :: (Member ReplCore sig, Carrier sig m) => m ()
showBanner = send $ ShowBanner (pure ())

read :: (Member ReplCore sig, Carrier sig m) => m Text
read = send $ Read pure

handleInterrupt
  :: (Member ReplCore sig, Carrier sig m)
  => m a
  -> (InterruptExcept -> m a)
  -> m a
handleInterrupt m h = send (HandleInterrupt m h pure)

interrupt :: (Member ReplCore sig, Carrier sig m) => m ()
interrupt = send $ Interrupt (pure ())

kill :: (Member ReplCore sig, Carrier sig m) => m ()
kill = send $ Kill (pure ())

quit :: (Member ReplCore sig, Carrier sig m) => m ()
quit = send $ Quit (pure ())

--------------------------------------------------------------------------------
data ReplValue v (m :: * -> *) k
  = Eval Text (v -> m k)
  | Print v (m k)

deriving stock instance Generic1 (ReplValue v m)
deriving stock instance Functor m => Functor (ReplValue v m)
deriving anyclass instance HFunctor (ReplValue v)
deriving anyclass instance Effect (ReplValue v)

eval :: (Member (ReplValue v) sig, Carrier sig m) => Text -> m v
eval t = send $ Eval t pure

print :: (Member (ReplValue v) sig, Carrier sig m) => v -> m ()
print val = send $ Print val (pure ())

--------------------------------------------------------------------------------
-- Fuseline exceptions
data InterruptExcept = InterruptExcept
  deriving (Show, Eq, Exception)

data ReplExcept a = UserExcept a
                  | System
                  | ReplInterruptExcept InterruptExcept
  deriving (Show, Eq, Exception)

--------------------------------------------------------------------------------
-- Carrier for a summation of Repl effects
newtype ReplIOC s e v m a = ReplIOC { runReplIOC :: ErrorC (ReplExcept e) (StateC s (ReaderC (Config s e v) m)) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Carrier sig m, Effect sig, MonadIO m) => Carrier (ReplCore :+: ReplValue v :+: sig) (ReplIOC s e v m) where
  eff
    :: forall a
     . (:+:) ReplCore (ReplValue v :+: sig) (ReplIOC s e v m) a
    -> ReplIOC s e v m a
  eff = \case

--------------------------------------------------
-- Core Effect handlers
    L (ShowBanner next) -> ReplIOC $ do
      banner <- asks @(Config s e v) _banner
      liftIO $ Text.putStrLn banner
      runReplIOC next

    L (Read nextF) -> ReplIOC $ liftIO Text.getLine >>= runReplIOC . nextF

    L (Interrupt next) ->
      ReplIOC
        $  throwError (ReplInterruptExcept @e InterruptExcept)
        >> runReplIOC next

    L (HandleInterrupt m h nextF) ->
      ReplIOC
        $   catchError @(ReplExcept e)
              (runReplIOC m)
              -- catch interrupts but throw everything else again.
              (\case
                ReplInterruptExcept exc -> runReplIOC $ h exc
                other                   -> throwError other
              )
        >>= runReplIOC
        .   nextF

    L (Kill next) -> ReplIOC $ liftIO exitFailure >> runReplIOC next

    L (Quit next) -> ReplIOC $ do
      goodbye <- asks @(Config s e v) _goodbye
      liftIO $ Text.putStrLn goodbye
      _ <- liftIO exitSuccess
      runReplIOC next

--------------------------------------------------
-- Value Effect handlers
    R (L (Eval input next)) -> ReplIOC $ do
      interpreter <- asks @(Config s e v) _interpreter
      updateState <- asks @(Config s e v) _updateState
      state       <- get
      case interpreter state input of
        Left  err   -> throwError (UserExcept err)
        Right value -> do
          put $ updateState state value
          runReplIOC . next $ value

    R (L (Print value next)) -> ReplIOC $ do
      showValue <- asks @(Config s e v) _showValue
      liftIO (Text.putStrLn (showValue value))
      runReplIOC next

    R (R other) -> ReplIOC $ eff (R (R (R (handleCoercible other))))

runRepl
  :: Config s e v -> s -> ReplIOC s e v m a -> m (s, Either (ReplExcept e) a)
runRepl config initial repl =
  runReplIOC repl & runError & runState initial & runReader config
