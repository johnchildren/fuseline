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

module System.Console.Fuseline
  ( Config(..)
  , Repl(..)
  , replLoop
  , runRepl
  )
where

import           Prelude                                  ( (.)
                                                          , ($)
                                                          , undefined
                                                          , IO
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
import           Control.Effect.Error                     ( Error
                                                          , ErrorC
                                                          , throwError
                                                          , catchError
                                                          , runError
                                                          )
import           Control.Effect.Lift                      ( Lift
                                                          , LiftC(..)
                                                          , sendM
                                                          , runM
                                                          )
import           Control.Effect.Reader                    ( Reader
                                                          , ReaderC
                                                          , ask
                                                          , runReader
                                                          )
import           Control.Effect.State                     ( State
                                                          , StateC
                                                          , get
                                                          , put
                                                          , runState
                                                          )
import           Control.Effect.Sum                       ( (:+:)(..) )
import           Control.Monad                            ( Monad
                                                          , (>>)
                                                          , (>>=)
                                                          )
import           Data.Either                              ( Either(..) )
import           Data.Functor                             ( Functor )
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as Text
import           System.Exit                              ( exitFailure
                                                          , exitSuccess
                                                          )


data Config s e v = Config { _banner :: Text
                         , _interpreter :: s -> Text -> Either e v
                         , _showError :: e -> Text
                         , _showValue :: v -> Text
                         , _goodbye :: Text
                         , _updateState :: s -> v -> s
                         }

data Repl s e v (m :: * -> *) k
  = ShowBanner k
  | Read (Text -> k)
  | Eval Text (v -> k)
  | Print v k
--  can't derive catch
--  | forall b . Catch (m b) (e -> m b) (b -> k)
  | Interrupt k
  | Quit k

deriving instance Functor (Repl s e v m)
deriving instance HFunctor (Repl s e v)
deriving instance Effect (Repl s e v)

showBanner
  :: forall s e v m sig . (Member (Repl s e v) sig, Carrier sig m) => m ()
showBanner = send (ShowBanner @s @e @v (pure ()))

read :: forall s e v m sig . (Member (Repl s e v) sig, Carrier sig m) => m Text
read = send (Read @s @e @v pure)

eval
  :: forall s e v m sig
   . (Member (Repl s e v) sig, Carrier sig m)
  => Text
  -> m v
eval t = send (Eval @s @e @v t pure)

print
  :: forall s e v m sig . (Member (Repl s e v) sig, Carrier sig m) => v -> m ()
print val = send (Print @s @e @v val (pure ()))

--catch :: (Member (Repl s e v) sig, Carrier sig m) => m a -> (e -> m a) -> m a
--catch m h = send (Catch m h pure)

newtype ReplC s e v m a = ReplC { runReplC :: ReaderC (Config s e v) (ErrorC e (StateC s (LiftC m))) a }
  deriving newtype (Functor, Applicative, Monad)

instance (Carrier sig m, Effect sig, Member (Reader (Config s e v)) sig, Member (State s) sig, Member (Error e) sig, Member (Lift IO) (Lift m)) => Carrier (Repl s e v :+: sig) (ReplC s e v m) where
  eff
    :: forall a
     . (:+:) (Repl s e v) sig (ReplC s e v m) (ReplC s e v m a)
    -> ReplC s e v m a

  eff (L (ShowBanner k)) = do
    config <- ask @(Config s e v)
    ReplC $ sendM (Text.putStrLn (_banner config)) >> runReplC k

  eff (L (Read k      )) = ReplC (sendM Text.getLine >>= runReplC . k)

  eff (L (Eval input k)) = do
    config <- ask @(Config s e v)
    state  <- get
    ReplC $ case _interpreter config state input of
      Left  err   -> throwError err
      Right value -> runReplC . k $ value

  eff (L (Print value k)) = do
    config <- ask @(Config s e v)
    ReplC $ sendM (Text.putStrLn (_showValue config value)) >> runReplC k

  eff (L (Interrupt _)) = ReplC (sendM exitFailure)

  eff (L (Quit      _)) = do
    config <- ask @(Config s e v)
    ReplC (sendM (Text.putStrLn (_goodbye config) >> exitSuccess))

  eff (R other) = eff (R (handleCoercible other))


runRepl :: Config s e v -> s -> ReplC s e v m a -> m (s, Either e a)
runRepl config s = runM . runState s . runError . runReader config . runReplC

replLoop
  :: forall s e v m sig
   . ( Effect sig
     , Member (Reader (Config s e v)) sig
     , Member (State s) sig
     , Member (Error e) sig
     , Member (Lift IO) (Lift m)
     , Carrier sig m
     )
  => Config s e v
  -> s
  -> m (s, Either e ())
replLoop config initial = runRepl config initial loop
 where
  loop = do
    showBanner @s @e @v
    minput <- read @s @e @v
    case minput of
      ""    -> loop
      input -> do
        val <- eval @s @e @v input
        print @s @e @v val
