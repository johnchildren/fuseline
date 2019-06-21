{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Data.Function                            ( const
                                                          , ($)
                                                          )
import           Data.Either
import           Control.Effect
import           Control.Monad                            ( (>>) )
import           System.Console.Fuseline
import           System.IO                                ( IO )

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

main :: IO ((), Either () ())
main = runM $ runRepl config () (replLoop @())
 where
  config = Config { _banner      = "hi"
                  , _interpreter = \_ _ -> Right ()
                  , _showError   = const ""
                  , _showValue   = const ""
                  , _goodbye     = ""
                  , _updateState = \_ _ -> ()
                  }
