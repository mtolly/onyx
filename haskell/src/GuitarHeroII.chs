{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
module GuitarHeroII (writeVGS, replaceSong) where

import           Control.Monad                (liftM4, replicateM_, unless, forM_)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Binary.Put
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Audio           as A
import           Data.Int                     (Int16)
import qualified Data.Vector.Storable         as V
import           Foreign
import Control.Exception (bracket, bracket_)
import           Foreign.C
import qualified System.IO                    as IO
import           System.IO.Unsafe             (unsafePerformIO)
import qualified Data.DTA as D
import System.IO.Temp (withSystemTempFile)
import Data.Monoid ((<>))

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

withByteString :: B.ByteString -> (CString -> IO a) -> IO a
withByteString = B.useAsCString

peekByteString :: CString -> IO B.ByteString
peekByteString = B.packCString

{#fun ark_Open
  { `ArkTool'
  , withCString* `FilePath' -- ^ ark directory
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
  , withCString* `FilePath' -- ^ destination filepath
  , withByteString* `B.ByteString' -- ^ filename in ark
  , `Bool' -- ^ whether to perform decryption
  } -> `Bool'
#}

{#fun ark_AddFile
  { `ArkTool'
  , withCString* `FilePath' -- ^ source filepath
  , withByteString* `B.ByteString' -- ^ filename in ark
  , `Bool' -- ^ whether to perform encryption
  } -> `Bool'
#}

{#fun ark_RemoveFile
  { `ArkTool'
  , withByteString* `B.ByteString' -- ^ filename in ark
  } -> `Bool'
#}

{#fun ark_ReplaceAFile
  { `ArkTool'
  , withCString* `FilePath' -- ^ source filepath
  , withByteString* `B.ByteString' -- ^ filename in ark
  , `Bool' -- ^ whether to perform encryption
  } -> `Bool'
#}

{#fun ark_First
  { `ArkTool'
  , `ArkFileIterator'
  , withByteString* `B.ByteString' -- ^ search filepath
  } -> `ArkFileEntry'
#}

{#fun ark_Next
  { `ArkTool'
  , `ArkFileIterator'
  , withByteString* `B.ByteString' -- ^ search filepath
  } -> `ArkFileEntry'
#}

{#fun ark_Arkname
  { `ArkFileEntry'
  } -> `B.ByteString' peekByteString*
#}

searchFiles :: ArkTool -> B.ByteString -> IO [ArkFileEntry]
searchFiles ark pat = bracket ark_new_iterator ark_delete_iterator $ \iter -> do
  let go entry@(ArkFileEntry p) = if p == nullPtr
        then return []
        else (entry :) <$> (ark_Next ark iter pat >>= go)
  ark_First ark iter pat >>= go

-- | Replaces a song in Guitar Hero II (U).
replaceSong
  :: FilePath -- ^ the @GEN@ folder containing ark and hdr files
  -> B.ByteString -- ^ the folder name (and DTB key) of the song to replace
  -> [D.Chunk B.ByteString] -- ^ the DTB snippet to insert into @config/gen/songs.dtb@
  -> [(B.ByteString, FilePath)] -- ^ the files to copy into the song folder, e.g. @("yyz.mid", "some/dir/notes.mid")@
  -> IO ()
replaceSong gen key snippet files = do
  let wrap msg f = f >>= \b -> unless b $ fail msg
  bracket ark_new ark_delete $ \ark -> do
    let open = wrap "Couldn't open the ARK file" $ ark_Open ark gen
    bracket_ open (ark_Close ark) $ do
      withSystemTempFile "songs.dtb" $ \fdtb hdl -> do
        IO.hClose hdl
        wrap "Couldn't find songs.dtb in the ARK." $
          ark_GetFile ark fdtb "config/gen/songs.dtb" True
        D.DTA z (D.Tree _ chunks) <- D.readFileDTB fdtb
        let adjust chunk = case chunk of
              D.Parens (D.Tree _ (D.Key k : _)) | k == key ->
                D.Parens $ D.Tree 0 $ D.Key k : snippet
              _ -> chunk
        D.writeFileDTB fdtb $ D.renumberFrom 1 $ D.DTA z $ D.Tree 0 $ map adjust chunks
        wrap "Couldn't update songs.dtb in the ARK." $
          ark_ReplaceAFile ark fdtb "config/gen/songs.dtb" True
        entries <- searchFiles ark $ "songs/" <> key <> "/*"
        forM_ entries $ \entry -> do
          name <- ark_Arkname entry
          wrap ("Couldn't remove file from ARK: " <> show name) $
            ark_RemoveFile ark name
        forM_ files $ \(arkName, localPath) -> do
          let arkPath = "songs/" <> key <> "/" <> arkName
          wrap ("Couldn't add file " <> show localPath <> " to ARK as " <> show arkPath) $
            ark_AddFile ark localPath arkPath True -- encryption doesn't matter
        wrap "Couldn't save the updated ARK file." $ ark_Save ark
