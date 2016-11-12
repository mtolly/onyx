{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Sound.MIDI.Script.Parse
( parse
, File(..)
, Track(..)
, Number(..)
, Event(..)
, Event'(..)
, Meta(..)
, SysEx(..)
, MIDI(..)
, Mode(..)
) where

import qualified Sound.MIDI.Script.Scan as S

}

%name parse
%tokentype { S.Token }
%error { parseError }

%token
  str { S.Str $$ }
  rat { S.Rat $$ }
  tone { S.Tone $$ }
  '+' { S.Plus }
  '-' { S.Dash }
  '*' { S.Star }
  '/' { S.Slash }
  ':' { S.Colon }
  ',' { S.Comma }
  ';' { S.Semi }
  '(' { S.LParen }
  ')' { S.RParen }
  '{' { S.LBrace }
  '}' { S.RBrace }
  '|' { S.Pipe }
  seqnum { S.SequenceNum }
  text { S.TextEvent }
  copy { S.Copyright }
  name { S.TrackName }
  inst { S.InstrumentName }
  lyric { S.Lyric }
  mark { S.Marker }
  cue { S.CuePoint }
  prefix { S.MIDIPrefix }
  end { S.EndOfTrack }
  tempo { S.Tempo }
  smpte { S.SMPTEOffset }
  time { S.TimeSig }
  key { S.KeySig }
  major { S.Major }
  minor { S.Minor }
  ch { S.Channel }
  on { S.NoteOn }
  off { S.NoteOff }
  v { S.Value }
  after { S.Aftertouch }
  pc { S.ProgramChange }
  con { S.Control }
  bend { S.PitchBend }
  soundoff { S.AllSoundOff }
  reset { S.ResetAllControllers }
  local { S.LocalControl }
  notesoff { S.AllNotesOff }
  omni { S.OmniMode }
  mono { S.MonoMode }
  poly { S.PolyMode }
  bpm { S.BPM }
  bps { S.BPS }
  seq { S.Sequencer }
  meta { S.Meta }
  sysex { S.SysEx }
  escape { S.Escape }
  s { S.Seconds }
  len { S.Length }

%%

File
  : { [] }
  | TopTrack File { $1 : $2 }

TopTrack
  : tempo Track { (Nothing, $2) }
  | time Track { (Nothing, $2) }
  | str Track { (Just $1, $2) }

Track
  : ch Num '{' TrackEvents '}' { Track (Just $2) $4 }
  | '{' TrackEvents '}' { Track Nothing $2 }

TrackEvents
  : { [] }
  | TrackEventNotEndSubtrack { [$1] }
  | TrackEventEndSubtrack ';' TrackEvents { $1 : $3 }
  | TrackEventEndSubtrack TrackEvents { $1 : $2 }
  | TrackEventNotEndSubtrack ';' TrackEvents { $1 : $3 }

TrackEventEndSubtrack
  : Num ':' EventsEndSubtrack { ($1, $3) }

TrackEventNotEndSubtrack
  : Num ':' EventsEndAtom { ($1, $3) }
  | Num ':' { ($1, []) }

EventsEndAtom
  : EventNotSubtrack { [$1] }
  | Event ',' EventsEndAtom { $1 : $3 }

EventsEndSubtrack
  : Track { [Subtrack $1] }
  | Event ',' EventsEndSubtrack { $1 : $3 }

Event
  : Track { Subtrack $1 }
  | EventNotSubtrack { $1 }

EventNotSubtrack
  : Meta { Event $ Meta $1 }
  | ch Num MIDI { Event $ MIDI (Just $2) $3 }
  | MIDI { Event $ MIDI Nothing $1 }
  | SysEx { Event $ SysEx $1 }
  -- combined note on/off events
  | ch Num FullNote { $3 (Just $2) }
  | FullNote { $1 Nothing }

FullNote
  : on Num v Num len Num
    { \ch -> Subtrack $ Track ch
        [ (0 , [Event $ MIDI Nothing $ NoteOn  $2 $4])
        , ($6, [Event $ MIDI Nothing $ NoteOff $2 0 ])
        ]
    }
  | on Num len Num
    { \ch -> Subtrack $ Track ch
        [ (0 , [Event $ MIDI Nothing $ NoteOn  $2 96])
        , ($4, [Event $ MIDI Nothing $ NoteOff $2 0 ])
        ]
    }

Meta
  : seqnum Num { SequenceNum $2 }
  | text str { TextEvent $2 }
  | str { TextEvent $1 }
  | copy str { Copyright $2 }
  | name str { TrackName $2 }
  | inst str { InstrumentName $2 }
  | lyric str { Lyric $2 }
  | mark str { Marker $2 }
  | cue str { CuePoint $2 }
  | prefix Num { MIDIPrefix $2 }
  | end { EndOfTrack }
  | tempo Num { SetTempo $2 }
  | smpte '(' Num ',' Num ',' Num ',' Num ',' Num ')'
    { SMPTEOffset $3 $5 $7 $9 $11 }
  | time '(' TimeSig ClockDetails ')'
    { TimeSig (fst $3) (snd $3) (fst $4) (snd $4) }
  | key Mode Num { KeySig $2 $3 }
  | seq '(' Nums ')' { SequencerSpecific $3 }
  | meta Num '(' Nums ')' { Unknown $2 $4 }

SysEx
  : sysex '(' Nums ')' { Regular $3 }
  | escape '(' Nums ')' { Escape $3 }

-- Parses the numerator and denominator of a time signature.
TimeSig
  : Num ',' Num { ($1, $3) }
  | Num ':' Num { ($1, Log2 $3) }

-- Parses the # of MIDI clocks in a quarter note,
-- and the number of 32nd notes in a quarter note.
ClockDetails
  : ',' Num ',' Num { ($2, $4) }
  | { (24, 8) }

Mode
  : major { Major }
  | minor { Minor }

MIDI
  : on Num v Num { NoteOn $2 $4 }
  | on Num { NoteOn $2 96 }
  | off Num v Num { NoteOff $2 $4 }
  | off Num { NoteOff $2 0 }
  | after Num v Num { PolyAftertouch $2 $4 }
  | pc Num { ProgramChange $2 }
  | con Num v Num { Control $2 $4 }
  | bend Num { PitchBend $2 }
  | after v Num { MonoAftertouch $3 }
  | soundoff { AllSoundOff }
  | reset { ResetAllControllers }
  | local Num { LocalControl $2 }
  | notesoff { AllNotesOff }
  | omni Num { OmniMode $2 }
  | mono Num { MonoMode $2 }
  | poly { PolyMode }

Nums
  : { [] }
  | Num { [$1] }
  | Num ',' Nums { $1 : $3 }

-- Precedence levels:
--   (tight)
-- prefixes
-- suffixes
-- * /
-- + -
-- |
--   (loose)
-- All binary ops are left associative

Num
  : NumMsr { $1 }

NumMsr
  : NumMsr '|' NumAdd { Measures $1 + $3 }
  | NumAdd { $1 }

NumAdd
  : NumAdd '+' NumMult { $1 + $3 }
  | NumAdd '-' NumMult { $1 - $3 }
  | NumMult { $1 }

NumMult
  : NumMult '*' NumSuffix { $1 * $3 }
  | NumMult '/' NumSuffix { $1 / $3 }
  | NumSuffix { $1 }

NumSuffix
  : NumSuffix s { Seconds $1 }
  | NumSuffix bpm { let
    bps = $1 / 60
    spb = 1 / bps
    uspb = spb * 1000000
    in uspb
    }
  | NumSuffix bps { let
    bps = $1
    spb = 1 / bps
    uspb = spb * 1000000
    in uspb
    }
  | NumPrefix { $1 }

NumPrefix
  : tone NumPrefix { Rat (fromIntegral $1) + (12 * $2) }
  | '-' NumPrefix { negate $2 }
  | NumBase { $1 }

NumBase
  : rat { Rat $1 }
  | '(' Num ')' { $2 }

{

parseError :: [S.Token] -> a
parseError _ = error "Parse error"

type File = [(Maybe String, Track)]

data Track = Track
  { trackChannel :: Maybe Number
  , trackEvents :: [(Number, [Event])]
  } deriving (Eq, Ord, Show, Read)

data Number
  = Rat Rational
  | Measures Number -- ^ measures (floored) to beats
  | Seconds Number -- ^ seconds to beats
  | Add Number Number
  | Sub Number Number
  | Mult Number Number
  | Div Number Number
  | Abs Number
  | Signum Number
  | Log2 Number
  deriving (Eq, Ord, Show, Read)

instance Num Number where
  fromInteger = Rat . fromInteger
  (+) = Add
  (-) = Sub
  (*) = Mult
  abs = Abs
  signum = Signum

instance Fractional Number where
  fromRational = Rat
  (/) = Div

data Event
  = Subtrack Track
  | Event Event'
  deriving (Eq, Ord, Show, Read)

data Event'
  = Meta Meta
  | MIDI (Maybe Number) MIDI
  | SysEx SysEx
  deriving (Eq, Ord, Show, Read)

data Meta
  = SequenceNum Number
  | TextEvent String
  | Copyright String
  | TrackName String
  | InstrumentName String
  | Lyric String
  | Marker String
  | CuePoint String
  | MIDIPrefix Number
  | EndOfTrack
  | SetTempo Number
  | SMPTEOffset Number Number Number Number Number
  | TimeSig Number Number Number Number
  | KeySig Mode Number
  | SequencerSpecific [Number]
  | Unknown Number [Number]
  deriving (Eq, Ord, Show, Read)

data SysEx
  = Regular [Number]
  | Escape [Number]
  deriving (Eq, Ord, Show, Read)

data MIDI
  = NoteOn Number Number
  | NoteOff Number Number
  | PolyAftertouch Number Number
  | ProgramChange Number
  | Control Number Number
  | PitchBend Number
  | MonoAftertouch Number
  | AllSoundOff
  | ResetAllControllers
  | LocalControl Number
  | AllNotesOff
  | OmniMode Number
  | MonoMode Number
  | PolyMode
  deriving (Eq, Ord, Show, Read)

data Mode
  = Major
  | Minor
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

}
