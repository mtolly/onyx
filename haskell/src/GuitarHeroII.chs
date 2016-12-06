{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
module GuitarHeroII (writeVGS, arkListTest) where

import           Control.Monad                (liftM4, replicateM_)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Binary.Put
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Audio           as A
import           Data.Int                     (Int16)
import qualified Data.Text                    as T
import qualified Data.Text.Foreign            as T
import qualified Data.Vector.Storable         as V
import           Foreign
import Control.Exception (bracket, bracket_)
import Control.Applicative (liftA2)
import           Foreign.C
import qualified System.IO                    as IO
import           System.IO.Unsafe             (unsafePerformIO)

#include "encode_vag.h"

{#fun encodeVAGBlock
  { castPtr `Ptr Int16'
  , castPtr `Ptr Word8'
  , `Word8'
  , castPtr `Ptr Double'
  , castPtr `Ptr Double'
  , castPtr `Ptr Double'
  , castPtr `Ptr Double'
  } -> `()'
#}

xaBlockSamples :: Int
xaBlockSamples = 28

makeXABlock
  :: (Int, V.Vector Int16, (Double, Double, Double, Double))
  -> (B.ByteString, (Double, Double, Double, Double))
makeXABlock (channel, v, (a, b, c, d)) = unsafePerformIO $ do
  V.unsafeWith (V.take xaBlockSamples $ v V.++ V.replicate xaBlockSamples 0) $ \pin -> do
  allocaBytes 16 $ \pout -> do
  with a $ \pa -> do
  with b $ \pb -> do
  with c $ \pc -> do
  with d $ \pd -> do
    encodeVAGBlock pin pout (fromIntegral channel) pa pb pc pd
    bs <- B.packCStringLen (castPtr pout, 16)
    newState <- liftM4 (,,,) (peek pa) (peek pb) (peek pc) (peek pd)
    return (bs, newState)

writeVGS :: (MonadResource m) => FilePath -> A.AudioSource m Int16 -> m ()
writeVGS fp src = let
  A.AudioSource s r c _ = A.reorganize xaBlockSamples src
  header blocksPerChannel = BL.take 0x80 $ runPut $ do
    putByteString $ B8.pack "VgS!"
    putWord32le 2
    replicateM_ c $ do
      putWord32le $ round r
      putWord32le blocksPerChannel
    putByteString $ B.replicate 0x80 0
  writeBlock h input = do
    let (bs, newState) = makeXABlock input
    liftIO $ B.hPut h bs
    return newState
  writeBlocks h xaStates !soFar = C.await >>= \case
    Nothing  -> return soFar
    Just blk -> do
      xaStates' <- mapM (writeBlock h) $ zip3 [0..] (A.deinterleave c blk) xaStates
      writeBlocks h xaStates' $ soFar + 1
  go h = do
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0x80
    blocksPerChannel <- writeBlocks h (repeat (0, 0, 0, 0)) 0
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0
    liftIO $ BL.hPut h $ header blocksPerChannel
  in s C.$$ C.bracketP (IO.openBinaryFile fp IO.WriteMode) IO.hClose go

#include "ArkTool_v6.1/Onyx.h"

{#pointer ArkTool as ArkTool newtype #}
{#pointer ArkFileIterator as ArkFileIterator newtype #}
{#pointer ArkFileEntry as ArkFileEntry newtype #}

{#fun ark_new
  {} -> `ArkTool'
#}

{#fun ark_delete
  { `ArkTool'
  } -> `()'
#}

{#fun ark_new_iterator
  {} -> `ArkFileIterator'
#}

{#fun ark_delete_iterator
  { `ArkFileIterator'
  } -> `()'
#}

withCText :: T.Text -> (CString -> IO a) -> IO a
withCText t f = T.withCStringLen (T.snoc t '\0') (f . fst)

peekText :: CString -> IO T.Text
peekText cstr = T.pack <$> peekCString cstr

{#fun ark_Open
  { `ArkTool'
  , withCText* `T.Text'
  } -> `Bool'
#}

{#fun ark_Save
  { `ArkTool'
  } -> `Bool'
#}

{#fun ark_Close
  { `ArkTool'
  } -> `()'
#}

{#fun ark_GetFile
  { `ArkTool'
  , withCText* `T.Text'
  , withCText* `T.Text'
  , `Bool'
  } -> `Bool'
#}

{#fun ark_AddFile
  { `ArkTool'
  , withCText* `T.Text'
  , withCText* `T.Text'
  , `Bool'
  } -> `Bool'
#}

{#fun ark_RemoveFile
  { `ArkTool'
  , withCText* `T.Text'
  } -> `Bool'
#}

{#fun ark_ReplaceAFile
  { `ArkTool'
  , withCText* `T.Text'
  , withCText* `T.Text'
  , `Bool'
  } -> `Bool'
#}

{#fun ark_RenameFile
  { `ArkTool'
  , withCText* `T.Text'
  , withCText* `T.Text'
  } -> `Bool'
#}

{#fun ark_First
  { `ArkTool'
  , `ArkFileIterator'
  , withCText* `T.Text'
  } -> `ArkFileEntry'
#}

{#fun ark_Next
  { `ArkTool'
  , `ArkFileIterator'
  , withCText* `T.Text'
  } -> `ArkFileEntry'
#}

{#fun ark_Filename
  { `ArkFileEntry'
  } -> `T.Text' peekText*
#}

{#fun ark_Arkname
  { `ArkFileEntry'
  } -> `T.Text' peekText*
#}

arkListTest :: FilePath -> IO [T.Text]
arkListTest dir = bracket ark_new ark_delete $ \ark -> do
  bracket_ (ark_Open ark $ T.pack dir) (ark_Close ark) $ do
    bracket ark_new_iterator ark_delete_iterator $ \iter -> do
      let go entry@(ArkFileEntry p) = if p == nullPtr
            then return []
            else liftA2 (:) (ark_Arkname entry) (ark_Next ark iter "*" >>= go)
      ark_First ark iter "*" >>= go
