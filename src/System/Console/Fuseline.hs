{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , runRepl
  , showBanner
  , read
  , eval
  , print
  , interrupt
  , quit
  , replLoop
  )
where

import           Prelude                                  ( (.)
                                                          , ($)
                                                          , IO
                                                          , const
                                                          )
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
import           Control.Effect.Lift                      ( runM )
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
import           Data.Functor                             ( Functor )
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as Text
import           System.Exit                              ( exitFailure
                                                          , exitSuccess
                                                          )


data Config s e v = Config
   { _banner :: Text
   , _interpreter :: s -> Text -> Either e v
   , _showError :: e -> Text
   , _showValue :: v -> Text
   , _goodbye :: Text
   , _updateState :: s -> v -> s
   }

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

data ReplValue v (m :: * -> *) k
  = Eval Text (v -> k)
  | Print v k

deriving instance Functor (ReplValue v m)
deriving instance HFunctor (ReplValue v)
deriving instance Effect (ReplValue v)

showBanner :: (Member ReplCore sig, Carrier sig m) => m ()
showBanner = send (ShowBanner (pure ()))

read :: (Member ReplCore sig, Carrier sig m) => m Text
read = send (Read pure)

eval :: (Member (ReplValue v) sig, Carrier sig m) => Text -> m v
eval t = send (Eval t pure)

print :: (Member (ReplValue v) sig, Carrier sig m) => v -> m ()
print val = send (Print val (pure ()))

--catch :: (Member (ReplCore s e v) sig, Carrier sig m) => m a -> (e -> m a) -> m a
--catch m h = send (Catch m h pure)

interrupt :: (Member ReplCore sig, Carrier sig m) => m ()
interrupt = send (Interrupt (pure ()))

quit :: (Member ReplCore sig, Carrier sig m) => m ()
quit = send (Quit (pure ()))

newtype ReplC s e v m a = ReplC { runReplC :: ErrorC e (StateC s (ReaderC (Config s e v) m)) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)


instance (Carrier sig m, Effect sig, MonadIO m) => Carrier (ReplCore :+: ReplValue v :+: sig) (ReplC s e v m) where
  eff
    :: forall a
     . (ReplCore :+: ReplValue v :+: sig) (ReplC s e v m) (ReplC s e v m a)
    -> ReplC s e v m a
  eff = \case

--------------------------------------------------
-- Core Effect handlers
    L (ShowBanner k) -> ReplC $ do
      banner <- asks @(Config s e v) _banner
      liftIO (Text.putStrLn banner) >> runReplC k

    L (Read      k) -> ReplC (liftIO Text.getLine >>= runReplC . k)

    L (Interrupt _) -> ReplC (liftIO exitFailure)

    L (Quit      _) -> ReplC $ do
      goodbye <- asks @(Config s e v) _goodbye
      liftIO (Text.putStrLn goodbye >> exitSuccess)


    --------------------------------------------------
    -- Value Effect handlers
    R (L (Eval input k)) -> ReplC $ do
      interpreter <- asks @(Config s e v) _interpreter
      updateState <- asks @(Config s e v) _updateState
      state       <- get
      case interpreter state input of
        Left  err   -> throwError err
        Right value -> do
          put $ updateState state value
          runReplC . k $ value

    R (L (Print value k)) -> ReplC $ do
      showValue <- asks @(Config s e v) _showValue
      liftIO (Text.putStrLn (showValue value)) >> runReplC k

    R (R other) -> ReplC $ eff (R (R (R (handleCoercible other))))

runRepl :: Config s e v -> s -> ReplC s e v m a -> m (s, Either e a)
runRepl config initial =
  runReader config . runState initial . runError . runReplC

replLoop
  :: forall v m sig
   . (Member ReplCore sig, Member (ReplValue v) sig, Carrier sig m)
  => m ()
replLoop = showBanner >> loop
 where
  loop = do
    minput <- read
    case minput of
      ""    -> loop
      input -> do
        val <- eval input
        print @v val


example :: IO ((), Either () ())
example = runM $ runRepl config () (replLoop @())
 where
  config = Config { _banner      = "hi"
                  , _interpreter = \_ _ -> Right ()
                  , _showError   = const ""
                  , _showValue   = const ""
                  , _goodbye     = ""
                  , _updateState = \_ _ -> ()
                  }
