module System.Terminal.Platform
  ( withTerminal ) where

import           Control.Monad          (void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class

withTerminal :: (MonadIO m, MonadMask m) => m a -> m a
withTerminal = bracket before after . const
  where
    before = set (Just True)
    after = void . set
    set Nothing = pure Nothing
    set (Just x) = do
      r <- liftIO $ setVirtualTerminalMode (if x then 1 else 0)
      pure $ case r of
        0 -> Just False
        1 -> Just True
        _ -> Nothing

foreign import ccall unsafe "hs_set_vt_mode"
  setVirtualTerminalMode  :: Int -> IO Int
