module Main where

import Euterpea
import Data.List as List
import Control.Monad.State (State, state, runState, get)
import Control.Monad (forever, when)
import System.Random
import System.IO
import Control.Concurrent
import Control.Exception as E
import Control.Concurrent.STM

import Debug.Trace

{- Evo-Drums
 -
 - The goal of this is to provide a drummer that I can jam along with.
 -
 - This means the goal is not to make totally wild music or to fit to some style or anything
 - like that. The music it produces should have a decent hit rate of being "music".
 -
 - There is a (currently very basic) interface that should make it easy to control and play
 - along with. This will hopefully at some point be extended to provide more history via
 - undoing mutations and recalling past Pieces (even from some kind of persistence). How to 
 - make this actually simple is still a problem, perhaps it should be based off of ratings
 - aka myself as a fitness function and some kind of basic searchability.
 -
 - Behind the interface there is the generation of Pieces which are my intermediate
 - representation. It is on this level that random seeds and weights which effect the composition
 - are stored and this should suffice to be persisted.
 -
 - Pieces can be mutated via random change in order to change songs. In general the goal is
 - that some kind of consistency should continue to exist in songs. This means extending 
 - the amount of weights and how the seeds to work, since just changing a fundamental seed
 - would totally change everything. At the same time large and unexpected changes are part of 
 - the fun and the point.
 -
 - There is also some potential that one could have "guided mutations" where for example 
 - things could be made more "intense" rather then randomly more or less intense by plugging
 - in a random gen that only produces positive values. This could be controlled by interface
 - extensions to allow "conducting" like behavior.
 -
 - From here it is passed to the very nice Euterpea which will generate music. Even here there are
 - options of what to do since Euterpea can handle phrasing concerns and change music on its level.
 -
 - From there it goes out to a synth, either on my computer or my drum synth and from there into
 - effect pedals or what have you.
 -
 - This is also an attempt to learn Haskell and to simply do some programming the way I like it.
 - Programming to make a tool for a person to do something, rather then for its own output.
 -}

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- to respond immediately to input
    (sendToPlayer, stop) <- spawnPlaybackChannel -- all instructions take effect after the current measure
    let act :: [Piece] -> IO ()
        act history = do
            let play = sendToPlayer . loadPiece . traceShowId
            c <- getChar
            case c of 'n' -> do -- generate a new piece
                              stdGen <- newStdGen
                              let piece = createPiece stdGen
                              play piece
                              act (piece:history)
                      's' -> do -- stop playing music
                              stop
                              act history
                      'm' -> do -- perform a random mutation of the playing track
                              if length history > 0 then do
                                                          stdGen <- newStdGen
                                                          let piece = mutate stdGen $ head history
                                                          play piece
                                                          act (piece:history)
                                                    else act history
                      'b' -> do -- go back one step
                              if length history > 1 then do
                                                          let (_:x:xs) = history
                                                          play x
                                                          act (x:xs)
                                                    else act history
                      _   -> act history -- default case do nothing
    act []

{-
 - The Constructors Used By All The Parts Of This System
 - (one to create, one to output to the next stage)
 -
 - The Piece is random weights and settings from which a consistent set of music will be
 - generated. it represents the composition and provides controls to alter compositions.
 -}
data Piece = Piece { seed   :: Int,
                     beats   :: Int,
                     parts :: [Part]
                   } deriving (Show, Read, Eq)

data Part = Part { pSeed   :: Int,
                   repeats :: Int,
                   tracks :: [Track]
                 } deriving (Show, Read, Eq)

data Track = Track  { tSeed :: Int,
                      inst :: Int,
                      playWeight :: Int
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

        gen <- get
        return Piece {seed=seed,
                      beats=4,
                      parts=[createPart instruments gen]}
      ) g
    where r = state random
          rR = state . randomR
          rRs i r = state $ randomRs' i r

randoms' :: (RandomGen g, Random a) => Int -> g -> ([a], g)
randoms' i g = ((take i $ randoms g), fst $ split g)

randomRs' :: (RandomGen g, Random a) => Int -> (a,a) -> g -> ([a], g)
randomRs' i r g = ((take i $ randomRs r g), fst $ split g)

createPart :: RandomGen g => [Int] -> g -> Part
createPart insts gen =
    let (val, nextGen) = random gen
    in Part { pSeed=val,
              repeats=2, -- temporary fixed value
              tracks=createTracks insts nextGen}

createTracks :: RandomGen g => [Int] -> g -> [Track]
createTracks [] _ = []
createTracks (i:is) gen =
    let (val, nextGen) = random gen
        seq = createTrack i val
    in seq : createTracks is nextGen

createTrack :: Int -> Int -> Track
createTrack inst seed = Track {tSeed=seed,
                               inst=inst,
                               playWeight=0} -- temporary fixed value
{- Mutation station
 -}
-- take a random gen and return a mutator with a split off random gen applied to it
mutate :: RandomGen g => g -> Piece -> Piece
mutate gen =
    let mutators = [
          addPart,
          removePart,
          addTrack,
          modifyRepeats]
        (index, nextGen) = randomR (0, (length mutators) - 1) gen
    in  (mutators!!index) nextGen

addPart :: RandomGen g => g -> Piece -> Piece
addPart g p =
    let oldParts = parts p
        indexToAdd = fst $ randomR (0, (length oldParts) - 1) g
        (left, right) = splitAt indexToAdd (parts p)
        insts [] = []
        insts (t:ts) = (inst t): insts ts
        newPart = createPart (insts $ tracks $ head oldParts)  g
    in  p{parts=(left++(newPart:right))}

removePart :: RandomGen g => g -> Piece -> Piece
removePart g p =
    let pas = (parts p)
        indexToRemove = fst $ randomR (0, (length pas) - 1) g
        (left, (_:right)) = splitAt indexToRemove pas
    in  if (length pas) > 1 then p{parts=(left++right)} else p

addTrack :: RandomGen g => g -> Piece -> Piece
addTrack g p = -- adds the same track to each part changed
    let (inst, nextG) = randomR percussionRange g -- should probably check for collision
        (seed, nexterG) = random nextG
        nTrack = createTrack inst seed
        adder _ pa = pa{tracks=nTrack:(tracks pa)}
    in  whichParts nexterG p adder

modifyRepeats :: RandomGen g => g -> Piece -> Piece
modifyRepeats g p =
    let factor = fst $ randomR (1,2) g :: Int
    in  whichParts g p $ (\ g pa -> pa{repeats=(repeats pa) * 2^factor})

{- M(util)ators
 -
 - utils for mutatin'
 -}
whichParts :: RandomGen g => g -> Piece -> (g -> Part -> Part) -> Piece
whichParts g = -- probably should balance the "All" against the number of items in the list
    let options = [
          mutateAllParts,
          mutateRandomPart]
        (index, nextG) = randomR (0, (length options) - 1) g
    in  (options!!index) nextG

mutateAllParts :: RandomGen g => g -> Piece -> (g -> Part -> Part) -> Piece
mutateAllParts   g p t = p{parts=mutateAllInList g (parts p) t}

mutateRandomPart :: RandomGen g => g -> Piece -> (g -> Part -> Part) -> Piece
mutateRandomPart g p t = p{parts=mutateRandomInList g (parts p) t}

mutateAllInList :: RandomGen g => g -> [a] -> (g -> a -> a) -> [a]
mutateAllInList g as f =
    let mutater g t [] = []
        mutater g t (a:as) =
            let (_, ng) = next g
            in  t ng a : mutater ng t as
    in  mutater g f as

mutateRandomInList :: RandomGen g => g -> [a] -> (g -> a -> a) -> [a]
mutateRandomInList g as f =
    let (val, nextGen) = randomR (0, (length as) - 1) g
        mutater _ _ [] = []
        mutater f i (a:as) = (if i == 0 then f a else a):(mutater f (i - 1) as)
    in  mutater (f nextGen) val as

{--
 - Turning a Piece into a Euterpea Music object to be played
 -
 - Euterpea handles converting a Piece into MIDI data and hopefully some of its tempo
 - changing, etc. abilities can also be used here.
 -
 - For example the phrase function can be used like `phrase [Dyn $ Accent 0.5]` to make a note
 - half intensity. This could be combined in different patterns (every other, or per measure, etc.)
 -}
loadPiece :: Piece -> Music (Pitch) -- this Music (Pitch) should be at the top level a "line" of measures
loadPiece (Piece s b p) = line $ foldl (++) [] $ map (partToMeasures b) p

partToMeasures :: Int -> Part -> [Music (Pitch)]
partToMeasures b pa = take (repeats pa) $ repeat (chord $ map (trackToMeasure b) (tracks pa))

trackToMeasure :: Int -> Track -> Music (Pitch)
trackToMeasure b Track{tSeed=s, inst=i, playWeight=w} =
    let inst = perc (toEnum i::PercussionSound) qn --hard coded for now
        chooser n = if (n + w) > 50 then inst else rest qn -- hard coded for now
        randomChanceList = randomRs (1, 100) (mkStdGen s) :: [Int]
    in  line $ map chooser $ take b randomChanceList

{--
 - Utility functions for overall system
 -}

-- based off of https://wiki.haskell.org/Background_thread_example
spawnPlaybackChannel :: IO (Music Pitch -> IO (), IO ())
spawnPlaybackChannel = do
    workVar <- atomically newEmptyTMVar

    let write j = atomically(do
                              empty <- isEmptyTMVar workVar
                              if empty
                                  then putTMVar workVar j
                                  else swapTMVar workVar j >> return ())
        stop    = write Nothing
        die e   = do
                   id <- myThreadId
                   print ("Playback Thread " ++ show id ++ " died with exception " ++ show (e :: ErrorCall))
                   stop
        pieceWork :: [Music Pitch] -> IO ()
        pieceWork [] = work
        pieceWork (x:xs) = do
                   noNewMessage <- atomically(isEmptyTMVar workVar) -- peek at the state
                   if noNewMessage then do
                                             play x
                                             pieceWork (xs ++ [x])
                                   else work
        work :: IO ()
        work    = do
                   mJob <- atomically(takeTMVar workVar) -- this should block on receiving a value
                   case mJob of Nothing -> work -- waiting on a new job
                                Just music -> pieceWork $ lineToList music

    forkIO work

    return (write . Just, stop)

