{
{-# OPTIONS_GHC -w #-}
module Onyx.Reaper.Scan (scanStack, Token(..), AlexPosn(..)) where

import Onyx.StackTrace (StackTraceT, fatal)
import qualified Data.Text as T
}

%wrapper "monad"

tokens :-

[\ \t] ;

[\r\n] { emit $ const Newline }
\< { emit $ const AngleL }
\> { emit $ const AngleR }

-- This is a bit ridiculous
\" [^\"]* \" { emit $ Atom . T.pack . dropEdges }
\' [^\']* \' { emit $ Atom . T.pack . dropEdges }
\` [^\`]* \` { emit $ Atom . T.pack . dropEdges }

(. # [ $white \" \' \` \< \> ]) (. # $white)* { emit $ Atom . T.pack }

{

emit :: (String -> a) -> AlexInput -> Int -> Alex (Maybe (AlexPosn, a))
emit f (pn, _, _, str) len = return $ Just $ (pn, f $ take len str)

dropEdges :: String -> String
dropEdges = reverse . drop 1 . reverse . drop 1

data Token
  = Newline
  | AngleL
  | AngleR
  | Atom T.Text
  deriving (Eq, Ord, Show, Read)

scanAll :: Alex [(AlexPosn, Token)]
scanAll = do
  res <- alexMonadScan
  case res of
    Nothing   -> return []
    Just pair -> (pair :) <$> scanAll

scanEither :: T.Text -> Either String [(AlexPosn, Token)]
scanEither t = runAlex (T.unpack t) scanAll

scan :: T.Text -> [(AlexPosn, Token)]
scan = either error id . scanEither

scanStack :: (Monad m) => T.Text -> StackTraceT m [(AlexPosn, Token)]
scanStack = either fatal return . scanEither

alexEOF :: Alex (Maybe (AlexPosn, Token))
alexEOF = return Nothing

}
