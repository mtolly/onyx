{-# LANGUAGE LambdaCase #-}
module Readme (makeReadme) where

import           Config
import           Control.Monad              (forM_, unless, when)
import           Control.Monad.Trans.Writer (execWriter, tell)
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import           Difficulty
import           System.FilePath            (takeDirectory, takeFileName)

makeReadme :: SongYaml -> FilePath -> String
makeReadme songYaml yamlPath = execWriter $ do
  let escape = concatMap $ \c -> if c `elem` "\\`*_{}[]()#+-.!"
        then ['\\', c]
        else [c]
      line str = tell $ str ++ "\n"
  line $ "# " ++ escape (T.unpack $ getTitle $ _metadata songYaml)
  line ""
  line $ "## " ++ escape (T.unpack $ getArtist $ _metadata songYaml)
  line ""
  case T.unpack $ getAuthor $ _metadata songYaml of
    "Onyxite" -> return ()
    auth      -> line $ "Author: " ++ auth
  line ""
  let titleDir = takeFileName $ takeDirectory yamlPath
      groupDir = takeFileName $ takeDirectory $ takeDirectory yamlPath
      link = "http://pages.cs.wisc.edu/~tolly/customs/" ++ groupDir ++ "/" ++ titleDir
  when (HM.member (T.pack "album") $ _plans songYaml) $ line $ "[Play in browser](" ++ link ++ ")"
  line ""
  line "Instruments:"
  line ""
  let diffString f dm = case f $ _difficulty $ _metadata songYaml of
        Just (Rank rank) -> g $ rankToTier dm rank
        Just (Tier tier) -> g tier
        Nothing          -> ""
        where g = \case
                1 -> " ⚫️⚫️⚫️⚫️⚫️"
                2 -> " ⚪️⚫️⚫️⚫️⚫️"
                3 -> " ⚪️⚪️⚫️⚫️⚫️"
                4 -> " ⚪️⚪️⚪️⚫️⚫️"
                5 -> " ⚪️⚪️⚪️⚪️⚫️"
                6 -> " ⚪️⚪️⚪️⚪️⚪️"
                7 -> " 😈😈😈😈😈"
                _ -> ""
  when (_hasDrums     $ _instruments songYaml) $ line $ if _proDrums $ _options songYaml
    then                                                "  * (Pro) Drums" ++ diffString _difficultyDrums     drumsDiffMap
    else                                                "  * Basic Drums" ++ diffString _difficultyDrums     drumsDiffMap
  when (_hasBass      $ _instruments songYaml) $ line $ "  * Bass"        ++ diffString _difficultyBass      bassDiffMap
  when (_hasGuitar    $ _instruments songYaml) $ line $ "  * Guitar"      ++ diffString _difficultyGuitar    guitarDiffMap
  when (_hasProBass   $ _instruments songYaml) $ line $ "  * Pro Bass"    ++ diffString _difficultyProBass   proBassDiffMap
  when (_hasProGuitar $ _instruments songYaml) $ line $ "  * Pro Guitar"  ++ diffString _difficultyProGuitar proGuitarDiffMap
  when (_hasKeys      $ _instruments songYaml) $ line $ "  * Keys"        ++ diffString _difficultyKeys      keysDiffMap
  when (_hasProKeys   $ _instruments songYaml) $ line $ "  * Pro Keys"    ++ diffString _difficultyProKeys   keysDiffMap
  case _hasVocal $ _instruments songYaml of
    Vocal0 -> return ()
    Vocal1 -> line $ "  * Vocals (1)" ++ diffString _difficultyVocal vocalDiffMap
    Vocal2 -> line $ "  * Vocals (2)" ++ diffString _difficultyVocal vocalDiffMap
    Vocal3 -> line $ "  * Vocals (3)" ++ diffString _difficultyVocal vocalDiffMap
  line ""
  line "Supported audio:"
  line ""
  forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do
    line $ "  * `" ++ T.unpack planName ++ "`" ++ if planName == T.pack "album"
      then " (" ++ escape (T.unpack $ getAlbum $ _metadata songYaml) ++ ")"
      else ""
    line ""
    forM_ (_planComments plan) $ \cmt -> do
      line $ "    * " ++ T.unpack cmt
      line ""
  unless (null $ _comments $ _metadata songYaml) $ do
    line "Notes:"
    line ""
    forM_ (_comments $ _metadata songYaml) $ \cmt -> do
      line $ "  * " ++ T.unpack cmt
      line ""
