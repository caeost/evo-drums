module Main where
import Euterpea
import Data.List (unfoldr, nub)
import Control.Monad.State (State, state, runState)
import System.Random
import qualified Data.Map as Map

main = play $ forever $ generate

{-
 - The Constructors Used By All The Parts Of This System
 - (one to create, one to output to the next stage)
 -
 - The Piece is random weights and settings from which a consistent set of music will be
 - generated. it represents the composition and provides controls to alter compositions.
 -}
data Piece = Piece { seed :: Int,
                     tracks :: [(Instrument, [Sequence])]
                   } deriving (Show)

data Instrument = Int deriving (Show)

data Sequence = Sequence { seedS :: Int,
                           repeats :: Int
                         } deriving (Show)
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

createPiece :: State StdGen Piece
createPiece i = do
        seed <- r
        instCount <- rR percussionRange
        instruments <- rRs instCount percussionRange
        seqs <- rs instCount
        let sequences = map ((: []) . createSequence) seqs
        return Piece {seed=seed, tracks= zip instruments sequences}
    where r = state random
          rR = state . randomR
          rs = state . randoms'
          rRs i r = state $ randomRs' i r

randoms' :: (RandomGen g, Random a) => Int -> g -> ([a], g)
randoms' i g = ((take i $ randoms g), mkStdGen (fst $ next g))

randomRs' :: (RandomGen g, Random a) => Int -> (a,a) -> g -> ([a], g)
randomRs' i r g = ((take i $ randomRs r g), mkStdGen (fst $ next g))

createSequence :: Int -> Sequence
createSequence seed = Sequence {seedS=seed, repeats=4} -- temporary fixed value

{--
 - Turning a Piece into a Euterpea Music object to be played
 -
 - Euterpea handles converting a Piece into MIDI data and hopefully some of its tempo
 - changing, etc. abilities can also be used here.
 -}
generate :: Music (Pitch)
generate = loadPiece $ fst $ runState createPiece (mkStdGen 12)

loadPiece :: Piece -> Music (Pitch)
loadPiece (Piece s t) = chord $ map loadTrack t

loadTrack :: (Instrument, [Sequence]) -> Music (Pitch)
loadTrack (i, seqs) =
    let perc = instrumentToPerc i
    in line $ map (\Sequence{seedS=s, repeats=r} -> genInstrument s perc r) seqs

instrumentToPerc :: Instrument -> Music (Pitch)
instrumentToPerc i = perc (toEnum i::PercussionSound)

genInstrument :: Int -> Music (Pitch) -> Int -> Music (Pitch)
genInstrument seed inst l =
    let chooser n = if n > 49 then inst else rest qn
    in line $ map (chooser) $ take l $ randomNumbers seed 0 100

