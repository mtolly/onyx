{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module RhythmGame.Audio where

import           Audio                          (stretchRealtime)
import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Async       (async)
import           Control.Concurrent.MVar
import           Control.Exception              (bracket, throwIO)
import           Control.Monad                  (forM, forM_)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace (logStdout)
import           Data.Conduit                   (runConduit, (.|))
import qualified Data.Conduit                   as C
import qualified Data.Conduit.Audio             as CA
import           Data.Conduit.Audio.Sndfile     (sourceSndFrom)
import           Data.Int                       (Int16)
import           Data.IORef
import qualified Data.Set                       as Set
import qualified Data.Vector.Storable           as V
import           Foreign                        hiding (void)
import           Foreign.C                      (CFloat (..))
import           MoggDecrypt
import           Sound.OpenAL                   (($=))
import qualified Sound.OpenAL                   as AL
import           System.FilePath                ((</>))
import           System.IO.Temp

{-
{-# NOINLINE lockAL #-}
lockAL :: MVar ()
lockAL = unsafePerformIO $ newMVar ()

checkAL :: String -> IO a -> IO a
checkAL desc f = withMVar lockAL $ \() -> do
  _ <- AL.get AL.alErrors
  x <- f
  errs <- AL.get AL.alErrors
  unless (null errs) $ putStrLn $ desc <> ": " <> show errs
  return x
-}

-- | Can be swapped out with checkAL to see OpenAL errors
doAL :: String -> IO a -> IO a
doAL _ f = f

withMOGG :: FilePath -> (FilePath -> IO a) -> IO a
withMOGG mogg fn = withSystemTempDirectory "onyxLoadMogg" $ \tmp -> do
  let ogg = tmp </> "audio.ogg"
  logStdout (moggToOgg mogg ogg) >>= either throwIO return
  fn ogg

sndSecsSpeed :: (MonadResource m) => Double -> Maybe Double -> FilePath -> IO (CA.AudioSource m Int16)
sndSecsSpeed pos mspeed f = do
  src <- sourceSndFrom (CA.Seconds pos) f
  let adjustSpeed = maybe id (\speed -> stretchRealtime (recip speed) 1) mspeed
  return $ CA.mapSamples CA.integralSample $ adjustSpeed src

withAL :: IO a -> IO a
withAL fn = let
  openDevice = AL.openDevice Nothing >>= maybe (error "couldn't open audio") return
  createContext dev = AL.createContext dev [] >>= maybe (error "couldn't create context") return
  destroyContext ctx = do
    AL.currentContext $= Nothing
    AL.destroyContext ctx
  in bracket openDevice AL.closeDevice $ \dev -> do
    bracket (createContext dev) destroyContext $ \ctx -> do
      AL.currentContext $= Just ctx
      fn

data AudioState = Filling | Playing

{-
emptySources :: [AL.Source] -> IO ()
emptySources srcs = do
  srcs' <- flip filterM srcs $ \src -> do
    cur <- doAL "emptySources buffersQueued" $ AL.buffersQueued src
    proc <- doAL "emptySources buffersProcessed" $ AL.buffersProcessed src
    if cur == 0
      then do
        doAL "emptySources deleteObjectNames source" $ AL.deleteObjectNames [src]
        return False
      else do
        -- this runs into problems because sometimes an unqueued buffer still
        -- can't be deleted for a bit! that's why we don't use this anymore
        when (proc /= 0) $ doAL "emptySources unqueueBuffers" (AL.unqueueBuffers src proc)
          >>= doAL "emptySources deleteObjectNames buffers" . AL.deleteObjectNames
        return True
  case srcs' of
    [] -> return ()
    _ -> do
      putStrLn $ "Waiting for " <> show (length srcs') <> " to empty"
      threadDelay 5000
      emptySources srcs'
-}

playSource
  :: [Float]
  -> [Float]
  -> CA.AudioSource (ResourceT IO) Int16
  -> IO (IO ())
playSource pans vols ca = do
  let chanCount = CA.channels ca
      floatRate = realToFrac $ CA.rate ca
  srcs <- doAL "playSource genObjectNames sources" $ AL.genObjectNames chanCount
  forM_ (zip srcs pans) $ \(src, pan) ->
    doAL "playSource setting sourcePosition" $
    AL.sourcePosition src $= AL.V3 (CFloat pan) 0 0
  forM_ (zip srcs vols) $ \(src, volDB) ->
    doAL "playSource setting sourceGain" $
    AL.sourceGain src $= CFloat (10 ** (volDB / 20))
  firstFull <- newEmptyMVar
  stopper <- newIORef False
  stopped <- newEmptyMVar
  let queueSize = 7
      waitTime = 5000
      t1 = runResourceT $ runConduit $ CA.source ca .| let
        loop currentBuffers audioState = liftIO (readIORef stopper) >>= \case
          True -> liftIO $ do
            doAL "playSource stopping sources" $ AL.stop srcs
            doAL "playSource deleting sources" $ AL.deleteObjectNames srcs
            doAL "playSource deleting remaining buffers" $ AL.deleteObjectNames $ Set.toList currentBuffers
            putMVar stopped ()
          False -> do
            current <- liftIO $ doAL "playSource buffersQueued" $ AL.buffersQueued $ head srcs
            if current < queueSize
              then do
                -- liftIO $ putStrLn $ "Filling because queue has " <> show current
                C.await >>= \case
                  Nothing -> do
                    case audioState of
                      Filling -> liftIO $ putMVar firstFull ()
                      _       -> return ()
                    loop currentBuffers Playing
                  Just chunk -> do
                    bufs <- liftIO $ doAL "playSource genObjectNames buffers" $ AL.genObjectNames chanCount
                    forM_ (zip bufs $ CA.deinterleave chanCount chunk) $ \(buf, chan) -> do
                      liftIO $ V.unsafeWith chan $ \p -> do
                        let _ = p :: Ptr Int16
                        doAL "playSource set bufferData" $ AL.bufferData buf $= AL.BufferData
                          (AL.MemoryRegion p $ fromIntegral $ V.length chan * sizeOf (V.head chan))
                          AL.Mono16
                          floatRate
                    forM_ (zip srcs bufs) $ \(src, buf) -> do
                      liftIO $ doAL "playSource queueBuffers" $ AL.queueBuffers src [buf]
                    let newBuffers = Set.fromList bufs
                    loop (Set.union currentBuffers newBuffers) audioState
              else do
                -- liftIO $ putStrLn "Queue is full"
                case audioState of
                  Filling -> liftIO $ putMVar firstFull ()
                  _       -> return ()
                removedBuffers <- fmap Set.unions $ liftIO $ forM srcs $ \src -> doAL "playSource buffersProcessed" (AL.buffersProcessed src) >>= \case
                  0 -> return Set.empty
                  n -> do
                    -- liftIO $ putStrLn $ "Removing " <> show n <> " finished buffers"
                    bufs <- doAL "playSource unqueueBuffers" $ AL.unqueueBuffers src n
                    doAL "playSource deleteObjectNames buffers" $ AL.deleteObjectNames bufs
                    return $ Set.fromList bufs
                liftIO $ threadDelay waitTime
                loop (Set.difference currentBuffers removedBuffers) Playing
        in loop Set.empty Filling
  _ <- async t1
  () <- takeMVar firstFull
  doAL "playSource play" $ AL.play srcs
  return $ writeIORef stopper True >> takeMVar stopped

-- | Use libvorbisfile to read an OGG
oggSecsSpeed :: (MonadResource m) => Double -> Maybe Double -> FilePath -> IO (CA.AudioSource m Int16)
oggSecsSpeed pos mspeed ogg = do
  src <- sourceVorbisFile (CA.Seconds pos) ogg
  let adjustSpeed = maybe id (\speed -> stretchRealtime (recip speed) 1) mspeed
  return $ CA.mapSamples CA.integralSample $ adjustSpeed src
