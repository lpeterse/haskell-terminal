module System.Terminal.Pretty where

import           Data.Text                     as T
import           Prettyprinter
import           Prelude                   hiding (putChar)

import           System.Terminal.MonadPrinter

-- | Print an annotated `Doc`.
--
-- Example:
--
-- @
-- import System.Terminal
-- import Prettyprinter
--
-- printer :: (`MonadFormatingPrinter` m, `MonadColorPrinter` m) => m ()
-- printer = `putDoc` $ `annotate` (foreground $ `bright` `blue`) "This is blue!" <> `line`
--                 <> `annotate` `bold` ("Just bold!" <> otherDoc <> "..just bold again")
--
-- otherDoc :: (`MonadColorPrinter` m, `Attribute` m ~ ann) => `Doc` ann
-- otherDoc = `annotate` (`background` `red`) " BOLD ON RED BACKGROUND "
-- @
--
-- Note the necessary unification of `Attribute` `m` and `ann` in the definition of `otherDoc`!
putDoc :: (MonadMarkupPrinter m) => Doc (Attribute m) -> m ()
putDoc doc = do
    w <- getLineWidth
    putSimpleDocStream (sdoc w)
    where
        options w = defaultLayoutOptions { layoutPageWidth = AvailablePerLine w 1.0 }
        sdoc w    = layoutSmart (options w) doc

-- | Like `putDoc` but adds an additional newline.
putDocLn :: (MonadMarkupPrinter m) => Doc (Attribute m) -> m ()
putDocLn doc = putDoc doc >> putLn

-- | Prints types instantiating the `Pretty` class.
putPretty :: (MonadMarkupPrinter m, Pretty a) => a -> m ()
putPretty a = putDoc (pretty a)

-- | Prints types instantiating the `Pretty` class and adds an additional newline.
putPrettyLn :: (MonadMarkupPrinter m, Pretty a) => a -> m ()
putPrettyLn a = putPretty a >> putLn

-- | Prints `SimpleDocStream`s (rather internal and not for the average user).
putSimpleDocStream :: (MonadMarkupPrinter m) => SimpleDocStream (Attribute m) -> m ()
putSimpleDocStream sdoc = do
    resetAttributes
    f [] sdoc
    where
        f _       SFail          = pure ()
        f _       SEmpty         = pure ()
        f    aa  (SChar c    xx) = putChar c                             >> f    aa  xx
        f    aa  (SText _ t  xx) = putText t                             >> f    aa  xx
        f    aa  (SLine i    xx) = putLn >> putText (T.replicate i " ")  >> f    aa  xx
        f    aa  (SAnnPush a xx) = setAttribute a                        >> f (a:aa) xx
        f    []  (SAnnPop    xx) =                                          f    []  xx
        f (a:aa) (SAnnPop    xx) = case Prelude.filter (resetsAttribute a) aa of
            []    -> resetAttribute a >> f aa xx
            (e:_) -> setAttribute   e >> f aa xx
