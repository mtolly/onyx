{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RhythmGame.Audio
( projectAudio, withAL, AudioHandle(..)
, withMOGG, oggSecsSpeed, playSource
) where

import           Audio
import           AudioSearch
import           Config
import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Async       (async)
import           Control.Concurrent.MVar
import           Control.Exception              (bracket, throwIO)
import           Control.Monad                  (forM, forM_, join)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace (QueueLog, StackTraceT,
                                                 errorToWarning, fatal,
                                                 logStdout)
import           Data.Conduit                   (runConduit, (.|))
import qualified Data.Conduit                   as C
import qualified Data.Conduit.Audio             as CA
import           Data.Conduit.Audio.Sndfile     (sourceSndFrom)
import           Data.Foldable                  (toList)
import qualified Data.HashMap.Strict            as HM
import           Data.Int                       (Int16)
import           Data.IORef
import qualified Data.List.NonEmpty             as NE
import qualified Data.Set                       as Set
import qualified Data.Text                      as T
import qualified Data.Vector.Storable           as V
import           Foreign                        hiding (void)
import           Foreign.C                      (CFloat (..), CInt (..),
                                                 CUInt (..))
import           MoggDecrypt
import           OpenProject
import           Path                           (parseAbsDir)
import           RenderAudio                    (computeChannelsPlan)
import           Sound.OpenAL                   (($=))
import qualified Sound.OpenAL                   as AL
import           System.FilePath                (takeDirectory, (<.>), (</>))
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

withAL :: (Bool -> IO a) -> IO a
withAL fn = let
  destroyContext ctx = do
    AL.currentContext $= Nothing
    AL.destroyContext ctx
  in bracket (AL.openDevice Nothing) (mapM_ AL.closeDevice) $ \mdev -> do
    case mdev of
      Nothing -> fn False
      Just dev -> bracket (AL.createContext dev []) (mapM_ destroyContext) $ \mctx -> do
        case mctx of
          Nothing -> fn False
          Just ctx -> do
            AL.currentContext $= Just ctx
            fn True

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

foreign import ccall unsafe
  alSourcei :: AL.ALuint -> AL.ALenum -> AL.ALint -> IO ()

data AudioHandle = AudioHandle
  { audioStop    :: IO ()
  , audioSetGain :: Float -> IO ()
  }

data AssignedSource
  = AssignedMono Float Float -- pan vol
  | AssignedStereo Float -- vol
  deriving (Show)

-- | Splits off adjacent L/R pairs into stereo sources for OpenAL
assignSources :: [Float] -> [Float] -> [AssignedSource]
assignSources ((-1) : 1 : ps) (v1 : v2 : vs) | v1 == v2
  = AssignedStereo v1 : assignSources ps vs
assignSources (p : ps) (v : vs)
  = AssignedMono p v : assignSources ps vs
assignSources _ _ = []

playSource
  :: [Float] -- ^ channel pans, -1 (L) to 1 (R)
  -> [Float] -- ^ channel volumes, in decibels
  -> Float -- ^ initial gain, 0 to 1
  -> CA.AudioSource (ResourceT IO) Int16
  -> IO AudioHandle
playSource pans vols initGain ca = do
  let assigned = assignSources pans vols
      srcCount = length assigned
      floatRate = realToFrac $ CA.rate ca
  srcs <- doAL "playSource genObjectNames sources" $ AL.genObjectNames srcCount
  forM_ srcs $ \src -> do
    with src $ \p -> do
      srcID <- peek $ castPtr p -- this is dumb but OpenAL pkg doesn't expose constructor
      doAL "playSource setting direct mode" $ do
        alSourcei srcID 0x1033 1 -- this is AL_DIRECT_CHANNELS_SOFT (should use c2hs!)
  let setGain g = forM_ (zip srcs $ assigned) $ \(src, srcAssigned) -> let
        volDB = case srcAssigned of
          AssignedMono _ vol -> vol
          AssignedStereo vol -> vol
        in doAL "playSource setting sourceGain" $ do
          AL.sourceGain src $= CFloat (g * (10 ** (volDB / 20)))
  setGain initGain
  firstFull <- newEmptyMVar
  stopper <- newIORef False
  stopped <- newEmptyMVar
  let queueSize = 10 -- TODO rework so this is in terms of frames/seconds, not buffer chunks
      waitTime = 5000
      t1 = runResourceT $ runConduit $ CA.source (CA.reorganize CA.chunkSize ca) .| let
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
                    liftIO $ threadDelay waitTime
                    loop currentBuffers Playing
                  Just chunk -> do
                    bufs <- liftIO $ doAL "playSource genObjectNames buffers" $ AL.genObjectNames srcCount
                    let grouped = groupChannels assigned $ CA.deinterleave (CA.channels ca) chunk
                        groupChannels (AssignedMono pan _ : xs) (chan : ys) = let
                          (ratioL, ratioR) = stereoPanRatios pan
                          newStereo = V.generate (V.length chan * 2) $ \i -> case quotRem i 2 of
                            (j, 0) -> CA.integralSample $ ratioL * CA.fractionalSample (chan V.! j)
                            (j, _) -> CA.integralSample $ ratioR * CA.fractionalSample (chan V.! j)
                          in newStereo : groupChannels xs ys
                        groupChannels (AssignedStereo{} : xs) (c1 : c2 : ys) = let
                          interleaved = CA.interleave [c1, c2]
                          in interleaved : groupChannels xs ys
                        groupChannels _ _ = []
                    forM_ (zip bufs grouped) $ \(buf, chan') -> do
                      liftIO $ V.unsafeWith chan' $ \p -> do
                        let _ = p :: Ptr Int16
                        doAL "playSource set bufferData" $ AL.bufferData buf $= AL.BufferData
                          (AL.MemoryRegion p $ fromIntegral $ V.length chan' * sizeOf (V.head chan'))
                          AL.Stereo16
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
  return AudioHandle
    { audioStop = writeIORef stopper True >> takeMVar stopped
    , audioSetGain = setGain
    }

-- | Use libvorbisfile to read an OGG
oggSecsSpeed :: (MonadResource m) => Double -> Maybe Double -> FilePath -> IO (CA.AudioSource m Int16)
oggSecsSpeed pos mspeed ogg = do
  src <- sourceVorbisFile (CA.Seconds pos) ogg
  let adjustSpeed = maybe id (\speed -> stretchRealtime (recip speed) 1) mspeed
  return $ CA.mapSamples CA.integralSample $ adjustSpeed src

projectAudio :: (MonadIO m) => T.Text -> Project -> StackTraceT (QueueLog m) (Maybe (Double -> Maybe Double -> Float -> IO AudioHandle))
projectAudio k proj = case lookup k $ HM.toList $ _plans $ projectSongYaml proj of
  Just MoggPlan{..} -> errorToWarning $ do
    -- TODO maybe silence crowd channels
    ogg <- shakeBuild1 proj [] $ "gen/plan/" <> T.unpack k <> "/audio.ogg"
    return $ \t speed gain -> oggSecsSpeed t speed ogg >>= playSource (map realToFrac _pans) (map realToFrac _vols) gain
  Just Plan{..} -> errorToWarning $ do
    let planAudios = toList _song ++ (toList _planParts >>= toList) -- :: [PlanAudio Duration AudioInput]
    lib <- newAudioLibrary
    audioDirs <- getAudioDirs proj
    forM_ audioDirs $ \dir -> do
      p <- parseAbsDir dir
      addAudioDir lib p
    let evalAudioInput = \case
          Named name -> do
            afile <- maybe (fatal "Undefined audio name") return $ HM.lookup name $ _audio $ projectSongYaml proj
            case afile of
              AudioFile ainfo -> searchInfo (takeDirectory $ projectLocation proj) lib (\n -> shakeBuild1 proj [] $ "gen/audio" </> T.unpack n <.> "wav") ainfo
              AudioSnippet expr -> join <$> mapM evalAudioInput expr
          JammitSelect{} -> fatal "Jammit audio not supported in preview yet" -- TODO
    planAudios' <- forM planAudios $ \planAudio -> do
      let chans = computeChannelsPlan (projectSongYaml proj) $ _planExpr planAudio
      evaled <- mapM evalAudioInput planAudio
      let expr = join $ _planExpr evaled
          pans = case _planPans evaled of
            [] -> case chans of
              1 -> [0]
              2 -> [-1, 1]
              _ -> replicate chans 0
            pansSpecified -> pansSpecified
          vols = case _planVols evaled of
            []            -> replicate chans 0
            volsSpecified -> volsSpecified
      return $ PlanAudio expr pans vols
      -- :: [PlanAudio Duration FilePath]
    case NE.nonEmpty planAudios' of
      Nothing -> fatal "No audio in plan"
      Just ne -> return $ \t mspeed gain -> do
        src <- buildSource' $ Merge $ fmap (Drop Start (CA.Seconds t) . _planExpr) ne
        playSource
          (map realToFrac $ planAudios' >>= _planPans)
          (map realToFrac $ planAudios' >>= _planVols)
          gain
          $ CA.mapSamples CA.integralSample
          $ maybe id (\speed -> stretchRealtime (recip speed) 1) mspeed src
  {-
  Just _ -> errorToWarning $ do
    wav <- shakeBuild1 proj [] $ "gen/plan/" <> T.unpack k <> "/everything.wav"
    return $ \t speed -> sndSecsSpeed t speed wav >>= playSource [-1, 1] [0, 0]
  -}
  Nothing -> return Nothing
