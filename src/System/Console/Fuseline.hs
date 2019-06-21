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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module System.Console.Fuseline
  ( Config(..)
  , ReplCore(..)
  , ReplValue(..)
  , ReplIOC(..)
  , runRepl
  , showBanner
  , read
  , eval
  , print
  , interrupt
  , quit
  )
where

import           Control.Applicative                      ( Applicative
                                                          , pure
                                                          )
import           Control.Effect                           ( Member
                                                          , send
                                                          )
import           Control.Effect.Carrier                   ( Effect(..)
                                                          , Carrier(..)
                                                          , HFunctor(..)
                                                          , handleCoercible
                                                          )
import           Control.Effect.Error                     ( ErrorC
                                                          , throwError
                                                          --, catchError
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
import           Control.Monad                            ( Monad
                                                          , (>>)
                                                          , (>>=)
                                                          )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Data.Either                              ( Either(..) )
import           Data.Function                            ( (.)
                                                          , ($)
                                                          )
import           Data.Functor                             ( Functor )
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as Text
import           System.Exit                              ( exitFailure
                                                          , exitSuccess
                                                          )

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
  = ShowBanner k
  | Read (Text -> k)
--  can't derive catch
--  | forall b . Catch (m b) (e -> m b) (b -> k)
  | Interrupt k
  | Quit k

deriving instance Functor (ReplCore m)
deriving instance HFunctor ReplCore
deriving instance Effect ReplCore

showBanner :: (Member ReplCore sig, Carrier sig m) => m ()
showBanner = send (ShowBanner (pure ()))

read :: (Member ReplCore sig, Carrier sig m) => m Text
read = send (Read pure)

--catch :: (Member (ReplCore s e v) sig, Carrier sig m) => m a -> (e -> m a) -> m a
--catch m h = send (Catch m h pure)

interrupt :: (Member ReplCore sig, Carrier sig m) => m ()
interrupt = send (Interrupt (pure ()))

quit :: (Member ReplCore sig, Carrier sig m) => m ()
quit = send (Quit (pure ()))

--------------------------------------------------------------------------------
data ReplValue v (m :: * -> *) k
  = Eval Text (v -> k)
  | Print v k

deriving instance Functor (ReplValue v m)
deriving instance HFunctor (ReplValue v)
deriving instance Effect (ReplValue v)

eval :: (Member (ReplValue v) sig, Carrier sig m) => Text -> m v
eval t = send (Eval t pure)

print :: (Member (ReplValue v) sig, Carrier sig m) => v -> m ()
print val = send (Print val (pure ()))

--------------------------------------------------------------------------------
-- Carrier for a summation of Repl effects
newtype ReplIOC s e v m a = ReplIOC { runReplIOC :: ErrorC e (StateC s (ReaderC (Config s e v) m)) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)


instance (Carrier sig m, Effect sig, MonadIO m) => Carrier (ReplCore :+: ReplValue v :+: sig) (ReplIOC s e v m) where
  eff
    :: forall a
     . (ReplCore :+: ReplValue v :+: sig)
      (ReplIOC s e v m)
      (ReplIOC s e v m a)
    -> ReplIOC s e v m a
  eff = \case

--------------------------------------------------
-- Core Effect handlers
    L (ShowBanner next) -> ReplIOC $ do
      banner <- asks @(Config s e v) _banner
      liftIO $ Text.putStrLn banner
      runReplIOC next

    L (Read      next) -> ReplIOC $ liftIO Text.getLine >>= runReplIOC . next

    L (Interrupt next) -> ReplIOC $ liftIO exitFailure >> runReplIOC next

    L (Quit      next) -> ReplIOC $ do
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
        Left  err   -> throwError err
        Right value -> do
          put $ updateState state value
          runReplIOC . next $ value

    R (L (Print value next)) -> ReplIOC $ do
      showValue <- asks @(Config s e v) _showValue
      liftIO (Text.putStrLn (showValue value))
      runReplIOC next

    R (R other) -> ReplIOC $ eff (R (R (R (handleCoercible other))))

runRepl :: Config s e v -> s -> ReplIOC s e v m a -> m (s, Either e a)
runRepl config initial =
  runReader config . runState initial . runError . runReplIOC
