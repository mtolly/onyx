{
{-# OPTIONS_GHC -w #-}
module Sound.MIDI.Script.Scan (scan, Token(..)) where

import Data.Char (toLower, isDigit)
import Data.Maybe (fromJust)
import Control.Arrow (first)

}

%wrapper "basic"

@str_char = [^ \\ \"] | "\" (. | \n)

tokens :-

$white+ ;
"#" [^ \n]* ;

\" @str_char* \" { Token . Str . read }
[0-9]+ { Token . Rat . fromInteger . read }
[0-9]+ "." [0-9]+ { \s -> let
  (whole, '.' : part) = span isDigit s
  wholeRat = fromInteger $ read whole
  partDenom = fromInteger $ 10 ^ length part
  partRat = fromInteger (read part) / partDenom
  in Token $ Rat $ wholeRat + partRat
  }
0x [0-9A-Fa-f]+ { Token . Rat . fromInteger . read }
[CDEFGABcdefgab] ([IiEe] [Ss])*
  { \s -> Token $ Tone $ case map toLower s of
    k : s' -> let
      readSuffix sfx = case sfx of
        'i' : 's' : sfx' -> readSuffix sfx' + 1
        'e' : 's' : sfx' -> readSuffix sfx' - 1
        "" -> 0
      key = fromJust $ lookup k $ zip "cdefgab" [0, 2, 4, 5, 7, 9, 11]
      in key + readSuffix s'
  }
[A-Za-z]+ { Ident }

"+" { const $ Token Plus }
"-" { const $ Token Dash }
"*" { const $ Token Star }
"/" { const $ Token Slash }
":" { const $ Token Colon }
"," { const $ Token Comma }
";" { const $ Token Semi }
"(" { const $ Token LParen }
")" { const $ Token RParen }
"{" { const $ Token LBrace }
"}" { const $ Token RBrace }
"|" { const $ Token Pipe }

{

data Token'
  = Ident String
  | Token Token
  deriving (Eq, Ord, Show, Read)

data Token
  = Str String
  | Rat Rational
  | Tone Int -- C is 0, D is 2, etc.
  | Plus
  | Dash
  | Star
  | Slash
  | Colon
  | Comma
  | Semi
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Pipe
  | SequenceNum
  | TextEvent
  | Copyright
  | TrackName
  | InstrumentName
  | Lyric
  | Marker
  | CuePoint
  | MIDIPrefix
  | EndOfTrack
  | Tempo
  | SMPTEOffset
  | TimeSig
  | KeySig
  | Major
  | Minor
  | Channel
  | NoteOn
  | NoteOff
  | Value
  | Aftertouch
  | ProgramChange
  | Control
  | PitchBend
  | AllSoundOff
  | ResetAllControllers
  | LocalControl
  | AllNotesOff
  | OmniMode
  | MonoMode
  | PolyMode
  | BPM
  | BPS
  | Sequencer
  | Meta
  | SysEx
  | Escape
  | Seconds
  | Length
  deriving (Eq, Ord, Show, Read)

identify :: Token' -> Token
identify (Ident i) = case map toLower i of
  "true" -> Rat 1
  "false" -> Rat 0
  "seqnum" -> SequenceNum
  "text" -> TextEvent
  "copy" -> Copyright
  "name" -> TrackName
  "inst" -> InstrumentName
  "lyric" -> Lyric
  "mark" -> Marker
  "cue" -> CuePoint
  "prefix" -> MIDIPrefix
  "end" -> EndOfTrack
  "tempo" -> Tempo
  "smpte" -> SMPTEOffset
  "time" -> TimeSig
  "key" -> KeySig
  "major" -> Major
  "minor" -> Minor
  "ch" -> Channel
  "on" -> NoteOn
  "off" -> NoteOff
  "v" -> Value
  "after" -> Aftertouch
  "pc" -> ProgramChange
  "con" -> Control
  "bend" -> PitchBend
  "soundoff" -> AllSoundOff
  "reset" -> ResetAllControllers
  "local" -> LocalControl
  "notesoff" -> AllNotesOff
  "omni" -> OmniMode
  "mono" -> MonoMode
  "poly" -> PolyMode
  "bpm" -> BPM
  "bps" -> BPS
  "seq" -> Sequencer
  "meta" -> Meta
  "sysex" -> SysEx
  "escape" -> Escape
  "s" -> Seconds
  "len" -> Length
  _ -> error $ "scan: unrecognized bare word " ++ show i
identify (Token tok) = tok

scan :: String -> [Token]
scan = map identify . alexScanTokens

}
