{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Text.Prettyprint.Doc
import           System.Terminal

import           Prelude                   hiding ((<>))

main :: IO ()
main = withTerminal $ runTerminalT app

app :: (MonadTerminal m, MonadMask m) => m ()
app = do
    putTextLn "Regular screen buffer..."
    flush
    liftIO (threadDelay 1000000)
    withAlternateScreenBuffer do

        lw <- getLineWidth
        putDoc $ mconcat
            [ pretty $ "The line width is " ++ show lw ++ " columns."
            , hardline 
            , annotate (foreground $ bright blue) "This is blue!"
            , hardline
            , annotate bold ("Just bold!" <+> annotate (background red) "BOLD ON RED BACKGROUND" <+> "..just bold again")
            , hardline
            , annotate (foreground $ bright red) "Hallo Welt!"
            , hardline
            , hang 10 $ "ssdfhsjdfhksjdhfkjsdhfks" <+> "hdfjkshdfkjshddh" <+> "fjksdhfkshdfkjshdfjks"
                    <+> "hdfkjshdfjhskdjfhsjksdhfjshdfjshdkj" <+> "fhsdkjfhskjdfhjksdhfjksdhfjks"
                    <+> "hdfkjshdfkh" <+> annotate bold "jdhfkjshdfkjshdfksjhdkfjhsdkjfhs" <+> "dkjfhskjdhfkjshdfkjshdfj"
                    <+> "中國哲學書電子化計劃"
                    <+> "jfksdfjlksdfjls"
                    <+> "\x1d11e"
            , line
            , line
            , annotate (background blue) "FOOBAR"
            , hardline
            ]
        flush
        liftIO (threadDelay 10000000)
