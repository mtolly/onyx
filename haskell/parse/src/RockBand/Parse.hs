{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module RockBand.Parse where

import           Control.Monad                    (guard, (>=>))
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (asum)
import           Data.Maybe                       (isJust)
import           Language.Haskell.TH
import qualified Numeric.NonNegative.Class        as NNC
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Message.Channel       as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util                  as U

import           RockBand.Common

type ParseOne t e a = RTB.T t e -> Maybe ((t, a), RTB.T t e)
type ParseAll t e a = RTB.T t e -> (RTB.T t a, RTB.T t e)
type UnparseOne t e a = a -> RTB.T t e
type UnparseAll t e a = RTB.T t a -> RTB.T t e

parseAll :: (NNC.C t, Ord e, Ord a) => ParseOne t e a -> ParseAll t e a
parseAll p rtb = case p rtb of
  Nothing -> case RTB.viewL rtb of
    Nothing -> (RTB.empty, rtb)
    Just ((dt, y), rtb') -> case parseAll p rtb' of
      (xs, rtb'') -> (RTB.delay dt xs, RTB.cons dt y rtb'')
  Just ((t, x), rtb') -> case parseAll p rtb' of
    (xs, rtb'') -> (RTB.insert t x xs, rtb'')

combineParseOne :: [ParseOne t e a] -> ParseOne t e a
combineParseOne ps rtb = asum $ map ($ rtb) ps

unparseAll :: (NNC.C t, Ord e) => UnparseOne t e a -> UnparseAll t e a
unparseAll u rtb = U.trackJoin $ fmap u rtb

class MIDIEvent a where
  parseOne   ::   ParseOne U.Beats E.T a
  unparseOne :: UnparseOne U.Beats E.T a

-- | The first element of each pair should be an expression of type @ParseOne t e a@.
-- The second element should be a lambda-case expression which forms
-- a partial function of type @UnparseOne t e a@.
-- The result is an instance of 'MIDIEvent'.
instanceMIDIEvent :: Q Type -> [(Q Exp, Q Exp)] -> Q [Dec]
instanceMIDIEvent typ cases = let
  parser = [e| combineParseOne $(listE $ map fst cases) |]
  unparser = do
    es <- mapM snd cases :: Q [Exp]
    return $ LamCaseE $ do
      e <- es
      case e of
        LamCaseE matches -> matches
        _ -> error "instanceMIDIEvent: improper form for event unparser. must be lambda-case"
  in [d|
    instance MIDIEvent $(typ) where
      parseOne   = $(  parser)
      unparseOne = $(unparser)
  |]

isNoteEdgeCPV :: E.T -> Maybe (Int, Int, Maybe Int)
isNoteEdgeCPV = \case
  E.MIDIEvent (C.Cons c (C.Voice (V.NoteOn  p v))) ->
    Just (C.fromChannel c, V.fromPitch p, case V.fromVelocity v of 0 -> Nothing; v' -> Just v')
  E.MIDIEvent (C.Cons c (C.Voice (V.NoteOff p _))) ->
    Just (C.fromChannel c, V.fromPitch p, Nothing)
  _ -> Nothing

isNoteEdge :: E.T -> Maybe (Int, Bool)
isNoteEdge e = isNoteEdgeCPV e >>= \(_c, p, v) -> return (p, isJust v)

parseBlips :: (NNC.C t) => [Int] -> ParseOne t E.T Int
parseBlips ns = combineParseOne [ mapParseOne (const n) $ parseBlip n | n <- ns ]

parseBlipCPV :: (NNC.C t) => ParseOne t E.T (Int, Int, Int)
parseBlipCPV rtb = do
  ((t, (c, p, mv)), rtb') <- firstEventWhich isNoteEdgeCPV rtb
  v <- mv
  ((_, ()), rtb'') <- flip U.extractFirst rtb' $ isNoteEdgeCPV >=> \case
    (c', p', Nothing) | (c, p) == (c', p') -> Just ()
    _                                      -> Nothing
  return ((t, (c, p, v)), rtb'')

-- | Parses a note-on and note-off with any positive duration between them.
parseBlip :: (NNC.C t) => Int -> ParseOne t E.T ()
parseBlip i rtb = do
  ((t, (_, p, _)), rtb') <- parseBlipCPV rtb
  guard $ i == p
  return ((t, ()), rtb')

mapParseOne :: (a -> b) -> ParseOne t e a -> ParseOne t e b
mapParseOne f p rtb = (\((t, x), rtb') -> ((t, f x), rtb')) <$> p rtb

-- | Does its best to turn a pattern back into an expression.
patToExp :: Pat -> Exp
patToExp = \case
  LitP l -> LitE l
  VarP _ -> error "patToExp: free variable in pattern can't be expression-ified"
  TupP ps -> TupE $ map patToExp ps
  UnboxedTupP ps -> UnboxedTupE $ map patToExp ps
  ConP n ps -> foldl AppE (ConE n) $ map patToExp ps
  InfixP p1 n p2 -> InfixE (Just $ patToExp p1) (VarE n) (Just $ patToExp p2)
  UInfixP p1 n p2 -> UInfixE (patToExp p1) (VarE n) (patToExp p2)
  ParensP p -> ParensE $ patToExp p
  TildeP p -> patToExp p
  BangP p -> patToExp p
  AsP _ p -> patToExp p
  WildP -> error "patToExp: wildcard in pattern can't be expression-ified"
  RecP n fps -> RecConE n $ map (\(f, p) -> (f, patToExp p)) fps
  ListP ps -> ListE $ map patToExp ps
  SigP p t -> SigE (patToExp p) t
  ViewP _ _ -> error "patToExp: view pattern can't be expression-ified"

unparseBlipCPV :: UnparseOne U.Beats E.T (Int, Int, Int)
unparseBlipCPV (c, p, v) = RTB.fromPairList
  [ (0     , makeEdgeCPV c p $ Just v)
  , (1 / 32, makeEdgeCPV c p Nothing )
  ]

unparseBlip :: UnparseOne U.Beats E.T Int
unparseBlip p = unparseBlipCPV (0, p, 96)

-- | A blip is an event which is serialized as a MIDI note of unimportant length.
-- In Rock Band, these can always have their length set to 1\/32 of a beat,
-- which is the smallest allowed distance between events.
blip :: Int -> Q Pat -> (Q Exp, Q Exp)
blip i pat =
  ( [e| mapParseOne (const $(fmap patToExp pat)) $ parseBlip i |]
  , lamCaseE [match pat (normalB [e| unparseBlip i |]) []]
  )

parseEdge :: (NNC.C t) => Int -> (Bool -> a) -> ParseOne t E.T a
parseEdge i f rtb = RTB.viewL rtb >>= \case
  ((t, e), rtb') -> isNoteEdge e >>= \case
    (i', b) | i == i' -> Just ((t, f b), RTB.delay t rtb')
    _                 -> Nothing

makeEdgeCPV :: Int -> Int -> Maybe Int -> E.T
makeEdgeCPV c p v = E.MIDIEvent $ C.Cons (C.toChannel c) $ C.Voice $
  V.NoteOn (V.toPitch p) $ maybe (V.toVelocity 0) V.toVelocity v

makeEdge :: Int -> Bool -> E.T
makeEdge p b = makeEdgeCPV 0 p $ guard b >> Just 96

-- | Makes a translation pair for a note edge event (an event which is serialized
-- as a note on or note off).
edge :: Int -> (Bool -> Q Pat) -> (Q Exp, Q Exp)
edge i patf =
  ( [e| parseEdge i
      (\b -> if b
        then $(patToExp <$> patf True )
        else $(patToExp <$> patf False)
      )
    |]
  , lamCaseE
    [ match (patf True ) (normalB [e| RTB.singleton NNC.zero $ makeEdge i True  |]) []
    , match (patf False) (normalB [e| RTB.singleton NNC.zero $ makeEdge i False |]) []
    ]
  )

-- | Applies a constructor (in the form of an incomplete pattern) to a boolean.
applyB :: Q Pat -> Bool -> Q Pat
applyB patcon b = patcon >>= \case
  ConP n pats -> do
    pat' <- if b then [p| True |] else [p| False |]
    return $ ConP n $ pats ++ [pat']
  _ -> error "applyB: pattern must be a constructor pattern"

stripPredicates :: [a -> Bool] -> [a] -> Maybe [a]
stripPredicates [] xs = Just xs
stripPredicates (p : ps) xs = case break p xs of
  (_ , []    ) -> Nothing
  (ys, _ : zs) -> stripPredicates ps $ ys ++ zs

-- | Parses a group of simultaneous events that match a series of predicates.
parsePredicates :: (NNC.C t, Ord e) => [e -> Bool] -> ParseOne t e ()
parsePredicates ps rtb = RTB.viewL rtb >>= \case
  ((t, x), rtb') -> case U.trackSplitZero rtb' of
    (xs, rtb'') -> do
      ys <- stripPredicates ps $ x : xs
      return ((t, ()), RTB.delay t $ foldr (RTB.cons NNC.zero) rtb'' ys)

isNoteEdgeB :: Bool -> Int -> E.T -> Bool
isNoteEdgeB b i e = case isNoteEdge e of
  Just (i', b') | b == b' && i == i' -> True
  _                                  -> False

makeEdges :: (NNC.C t) => [Int] -> Bool -> RTB.T t E.T
makeEdges is b = foldr (RTB.cons NNC.zero) RTB.empty $ map (`makeEdge` b) is

-- | Parses a collection of simultaneous note-ons or note-offs.
edges :: [Int] -> (Bool -> Q Pat) -> (Q Exp, Q Exp)
edges is patf =
  ( [e| combineParseOne
      [ mapParseOne (const $(patToExp <$> patf True )) $ parsePredicates (map (isNoteEdgeB True ) is)
      , mapParseOne (const $(patToExp <$> patf False)) $ parsePredicates (map (isNoteEdgeB False) is)
      ]
    |]
  , lamCaseE
    [ match (patf True ) (normalB [e| makeEdges is True  |]) []
    , match (patf False) (normalB [e| makeEdges is False |]) []
    ]
  )

-- | Parses any one note-edge with a pitch in the given set.
edgeRange :: [Int] -> (Int -> Bool -> Q Pat) -> (Q Exp, Q Exp)
edgeRange ps patf = let
  trues = listE $ flip map ps $ \i -> do
    e <- patToExp <$> patf i True
    return $ TupE [LitE $ IntegerL $ fromIntegral i, e]
  falses = listE $ flip map ps $ \i -> do
    e <- patToExp <$> patf i False
    return $ TupE [LitE $ IntegerL $ fromIntegral i, e]
  in  ( [e| firstEventWhich $ \e -> do
          (i, b) <- isNoteEdge e
          if b then lookup i $trues else lookup i $falses
        |]
      , lamCaseE
        [ match (patf i b) (normalB [e| makeEdges [i] b |]) []
        | i <- ps
        , b <- [True, False]
        ]
      )

parseCommand :: (Command a, NNC.C t) => ParseOne t E.T a
parseCommand = U.extractFirst readCommand'

unparseCommand :: (Command a, NNC.C t) => UnparseOne t E.T a
unparseCommand = RTB.singleton NNC.zero . showCommand'

firstEventWhich :: (NNC.C t) => (a -> Maybe b) -> ParseOne t a b
firstEventWhich f rtb = do
  ((t, x), rtb') <- RTB.viewL rtb
  y <- f x
  return $ ((t, y), RTB.delay t rtb')

commandPair :: [String] -> Q Pat -> (Q Exp, Q Exp)
commandPair cmd pat =
  ( [e| firstEventWhich $ readCommand' >=> \x ->
      if x == cmd then Just $(fmap patToExp pat) else Nothing
    |]
  , lamCaseE
    [ match pat (normalB [e| unparseCommand cmd |]) []
    ]
  )

boolP :: Bool -> Q Pat
boolP True  = [p| True  |]
boolP False = [p| False |]
