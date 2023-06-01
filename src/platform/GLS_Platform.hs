module GLS_Platform
  ( Int,
    Text,
    Maybe(Just),
    Monad(return),
    App(App),
    SomeApp(SomeApp),
    layoutText,
  ) where

import GHC.Records (HasField(getField))
import Foreign.C.Types (CInt)
import qualified Data.Text as Text
import Data.Text (Text)
import Control.Monad.State.Strict ( MonadState (get, put), runState )
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import qualified SDL
import Data.IORef
import System.Mem.Weak (Weak, finalize)
import Control.Exception
import Control.Concurrent.MVar
import Data.Word
import qualified SDL
import qualified Data.Vector.Unboxed as V.U

data App s = App (IO s) (s -> LayoutCtx_ -> Layout)

data SomeApp where
  SomeApp :: App s -> SomeApp

layoutText :: Layout
layoutText = undefined

type LayoutCtx = (?ctx :: LayoutCtx_)

data LayoutCtx_ =
  LayoutCtx {
    getFontLayoutInfo :: Int -> FontLayoutInfo
  }

data FontLayoutInfo =
  FontLayoutInfo {
    lineSkip :: CInt,
    ascent :: CInt,
    getGlyphMetrics :: Char -> (SDL.V2 CInt, SDL.V2 CInt),
    getKerningSize :: Char -> Char -> CInt
  }

data Extents =
  Extents {
    width :: CInt,
    height :: CInt
  }

zeroExtents :: Extents
zeroExtents = Extents 0 0

data Layout =
    LayoutText TextLayout
  | LayoutVBox Extents (Seq Layout)

instance HasField "extents" Layout Extents where
  getField (LayoutText t) = t.extents
  getField (LayoutVBox extents _) = extents

instance HasField "extents" LineLayout Extents where
  getField (LineLayout extents _) = extents

-- With a relative offset on a single axis (horizontal or vertical)
data RelOffset a = RelOff CInt a

data TextLayout =
  TextLayout {
    extents :: Extents,
    fontSize :: Int,
    lineLayouts :: Seq (RelOffset LineLayout),
    cursorPos :: Maybe Int
  }

data LineLayout = LineLayout Extents (Seq (RelOffset CharLayout))

lineLayoutCharCount :: LineLayout -> Int
lineLayoutCharCount (LineLayout _ charLayouts) = Seq.length charLayouts

data CharLayout = CharLayout CInt (SDL.V2 CInt) Char

newtype Path = Path [Text]

withCtx :: ((?ctx :: a) => b) -> (a -> b)
withCtx b a = let ?ctx = a in b

ctx :: (?ctx :: a) => a
ctx = ?ctx
