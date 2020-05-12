module Main where

import Euterpea
import Data.List (unfoldr, nub)
import Data.List as List
import Control.Monad.State (State, state, runState)
import Control.Monad (forever, when)
import System.Random
import System.IO
import Control.Concurrent
import Control.Exception as E
import Control.Concurrent.STM

import Debug.Trace

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- to respond immediately to input
    (sendToPlayer, stop) <- spawnPlaybackChannel
    Control.Monad.forever $ do
                c <- getChar
                case c of 'n' -> do
                                  let atomicPlay = atomically . sendToPlayer
                                  stdGen <- newStdGen
                                  atomicPlay $ generate stdGen
                          's' -> stop
                          _   -> return ()

{-
 - The Constructors Used By All The Parts Of This System
 - (one to create, one to output to the next stage)
 -
 - The Piece is random weights and settings from which a consistent set of music will be
 - generated. it represents the composition and provides controls to alter compositions.
 -}
data Piece = Piece { seed   :: Int,
                     tracks :: [(Int, [Sequence])]
                   } deriving (Show, Read, Eq)

data Sequence = Sequence { seedS   :: Int,
                           beats   :: Int,
                           repeats :: Int
                         } deriving (Show, Read, Eq)
{--
 - Generating and manipulating a Piece
 -
 - A Piece is either created from a base random seed gotten via IO and in response to
 - a user action or it is loaded from persistence as an object that had been created
 - previously and saved.
 -
 - Within a Piece there is no randomness, the same Piece will not generate a different
 - composition on two different executions.
 -
 - Mutation and inter-breeding
 -
 - Pieces can also be created from other existing pieces by messing with settings and
 - returning a new copy. Mutations can be introduced which provide some kind of
 - shading via for example changing thresholds or adding other random patterns.
 - Inter-breeding can be done between Pieces to copy over some values or settings from
 - two pieces randomly into a new one.
 -
 - There is also potential for more targeted manipulation later if desired. Being able to
 - directly boost a certain instrument's threshold to hear it less frequently for example.
 -}
percussionRange = (0, 46) -- 47 percussion instruments are defined in Euterpea

createPiece :: RandomGen g => g -> Piece
createPiece g = fst $ runState (do
        seed <- r
        instCount <- rR (1, 10) -- hard coded for now
        instruments <- rRs instCount percussionRange
        seqs <- rs instCount
        let sequences = map ((: []) . createSequence) seqs
        return Piece {seed=seed, tracks= zip instruments sequences}) g
    where r = state random
          rR = state . randomR
          rs = state . randoms'
          rRs i r = state $ randomRs' i r

randoms' :: (RandomGen g, Random a) => Int -> g -> ([a], g)
randoms' i g = ((take i $ randoms g), fst $ split g)

randomRs' :: (RandomGen g, Random a) => Int -> (a,a) -> g -> ([a], g)
randomRs' i r g = ((take i $ randomRs r g), fst $ split g)

createSequence :: Int -> Sequence
createSequence seed = Sequence {seedS=seed,
                                beats=4, -- temporary fixed value
                                repeats=2} -- remporary fixed value

{--
 - Turning a Piece into a Euterpea Music object to be played
 -
 - Euterpea handles converting a Piece into MIDI data and hopefully some of its tempo
 - changing, etc. abilities can also be used here.
 -}
generate :: RandomGen g => g -> Music (Pitch)
generate g = loadPiece $ traceShowId $ createPiece g

loadPiece :: Piece -> Music (Pitch)
loadPiece (Piece s t) = line $ map chord $ List.transpose $ map loadTrack t

-- Turns a track descriptor into a list of measures
loadTrack :: (Int, [Sequence]) -> [Music (Pitch)]
loadTrack (i, seqs) =
    let perc = instrumentToPerc i
        seqToMeasures :: Sequence -> [Music (Pitch)]
        seqToMeasures seq = take (repeats seq) $ repeat (seqToMeasure perc seq)
    in foldl1 (++) $ map seqToMeasures seqs

instrumentToPerc :: Int -> Music (Pitch)
instrumentToPerc i = perc (toEnum i::PercussionSound) qn --hard coded for now

seqToMeasure :: Music (Pitch) -> Sequence -> Music (Pitch)
seqToMeasure inst Sequence{seedS=s, beats=b} =
    let chooser n = if n > 50 then inst else rest qn -- hard coded for now
        randomChanceList = randomRs (1, 100) (mkStdGen s) :: [Int]
    in line $ map chooser $ take b randomChanceList

{--
 - Utility functions for overall system
 -}

-- based off of https://wiki.haskell.org/Background_thread_example
spawnPlaybackChannel :: IO (Music Pitch -> STM (), IO ())
spawnPlaybackChannel = do
    workChan <- atomically newTChan

    let stop    = atomically(writeTChan workChan Nothing)
        die e   = do
                   id <- myThreadId
                   print ("Playback Thread " ++ show id ++ " died with exception " ++ show (e :: ErrorCall))
                   stop
        pieceWork :: [Music Pitch] -> IO ()
        pieceWork (x:xs) = do
                   mJob <- atomically(tryPeekTChan workChan)
                   case mJob of Nothing -> do
                                             play x
                                             pieceWork (xs ++ [x])
                                Just job -> work
        work :: IO ()
        work    = do
                   mJob <- atomically(readTChan workChan) -- this should block on receiving a value
                   case mJob of Nothing -> work -- waiting on a new job
                                Just music -> pieceWork $ lineToList $ traceShowId music

    forkIO work

    return (writeTChan workChan . Just, stop)

