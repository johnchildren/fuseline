{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Applicative
import           Control.Monad
import           Data.Text
import           Data.Either
import           Data.Function
import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Writer
import           Control.Effect.State
import           Control.Effect.Sum
import           System.Console.Fuseline
import           System.IO                                ( IO )
import           Test.Tasty
import           Test.Tasty.Hspec


main :: IO ()
main = do
  spec <- testSpec "example" exampleSpec
  defaultMain (testGroup "tests" [spec])

exampleSpec :: Spec
exampleSpec =
  describe "repl"
    $          it "can show a banner"
    $          run (runReplRet [] showBanner)
    `shouldBe` (["banner"], ([], ()))

simpleExample :: IO ((), Either (ReplExcept ()) ())
simpleExample = runM $ runRepl config () (showBanner >> loop @())
 where
  config = Config { _banner      = "hi"
                  , _interpreter = \_ _ -> Right ()
                  , _showError   = const ""
                  , _showValue   = const ""
                  , _goodbye     = ""
                  , _updateState = \_ _ -> ()
                  }
  loop
    :: forall v m sig
     . (Member ReplCore sig, Member (ReplValue v) sig, Carrier sig m)
    => m ()
  loop = do
    minput <- read
    case minput of
      ""    -> loop @v
      input -> do
        val <- eval input
        print @v val

runReplRet :: [Text] -> ReplRetC v m a -> m ([Text], ([Text], a))
runReplRet i = runWriter . runState i . runReplRetC

newtype ReplRetC v m a = ReplRetC { runReplRetC :: StateC [Text] (WriterC [Text] m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (Carrier sig m, Effect sig) => Carrier (ReplCore :+: ReplValue v :+: sig) (ReplRetC v m) where
  eff = \case
    L (ShowBanner next ) -> ReplRetC (tell @[Text] ["banner"]) *> next
    R (R          other) -> ReplRetC $ eff (R (R (handleCoercible other)))
