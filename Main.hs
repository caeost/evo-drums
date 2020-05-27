module Main where

import Euterpea hiding(a,as,b,bs,c,cs,d,ds,e,es,f,fs,g,gs, left, right)
import Control.Monad.State (state, runState)
import System.Random
import System.IO
import Control.Concurrent
import Control.Exception as E
import Control.Concurrent.STM
import Data.Ratio
import Data.List (isPrefixOf)
import System.Process

import Debug.Trace

{--
 - Evo-Drums
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
    let act :: [[String]] -> Maybe Piece -> IO ()
        act changes playing = do
            let continue = act changes playing
            let queuePlay = sendToPlayer . loadPiece . traceShowId
            let playAndContinue chas p  = queuePlay p >> (act chas $ Just p)
            let playChangesAndContinue chas = case chas of []      -> act [] Nothing
                                                           l@(x:_) -> playAndContinue l (parseToPiece x)

            c <- getChar
            -- Start Interface
            case c of 'n' -> do -- generate and play a new piece
                              stdGen <- newStdGen
                              let p = createPiece stdGen
                              playAndContinue ([show stdGen]:changes) p
                      'p' -> do -- pause / unpause playing music
                              case playing of Just _  -> stop >> act changes Nothing
                                              Nothing -> playChangesAndContinue changes
                      'm' -> do -- perform a random mutation of the playing track
                              case playing of Just x -> do
                                                          stdGen <- newStdGen
                                                          let p = mutate x stdGen
                                                          let (y:ys) = changes
                                                          playAndContinue ((y ++ [show stdGen]):ys) p
                                              Nothing -> continue
                      'b' -> do -- go back one step
                              case changes of []    -> playChangesAndContinue []
                                              (_:xs)-> playChangesAndContinue xs
                      'w' -> do -- write songs to file (takes more input)
                              filename <- getLine
                              saveToFile filename (unlines $ map show changes)
                              continue
                      'r' -> do -- read file and play (takes more input)
                              filename <- getLine
                              loaded <- readAction filename
                              playChangesAndContinue loaded
                      'u' -> continue -- TODO up vote for current piece
                      'd' -> continue -- TODO down vote for current piece
                      _   -> do -- nothing, default case
                              continue
            -- End Interface
    act [] Nothing

readAction :: String -> IO [[String]]
readAction filename= do
    contents <- readFile filename
    return $ map (\a -> (read a) :: [String]) $ filter (isPrefixOf "[") $ lines contents

{--
 - The Constructors Used By All The Parts Of This System
 - (one to create, one to output to the next stage)
 -
 - The Piece is random weights and settings from which a consistent set of music will be
 - generated. it represents the composition and provides controls to alter compositions.
 -
 - Potentially it would be better to not destructively write to the various weights but
 - instead to save a list of what operations are applied to them.
 -}

data Piece = Piece { seed   :: Int, -- the initial random seed used to generate the Piece
                     beats   :: Int, -- how many beats per measure in the Piece
                     parts :: [Part] -- sequential ordered segments of the Piece
                   } deriving (Show, Read, Eq)

data Part = Part { pSeed   :: Int, -- seed used to generate the initial Part
                   repeats :: Int, -- how many times to repeat this part for within the piece
                   tracks :: [Track] -- different instruments that play simultaneously for this part
                 } deriving (Show, Read, Eq)

data Track = Track  { tSeed :: Int, -- seed used to generate notes for this track
                      inst :: Int, -- the instrument used by this track
                      playWeight :: Int, -- make the track more/less likely to play notes
                      noteDurWeight :: Int, -- make the track tend towards long/short notes
                      restDurWeight :: Int -- make the track tend towards long/short rests
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
percussionRange :: (Int, Int)
percussionRange = (0, 46) -- 47 percussion instruments are defined in Euterpea

parseToPiece :: [String] -> Piece
parseToPiece (i:ms) =
    let root = createPiece $ ((read i) :: StdGen)
        folder r m = mutate r $ (read m :: StdGen)
    in  foldl folder root ms

createPiece :: RandomGen g => g -> Piece
createPiece g = fst $ runState (do
        s <- ra
        instCount <- raR (1, 10) -- hard coded for now
        instruments <- raRs instCount percussionRange

        partCount <- raR (1,4) -- hard coded for now
        partSeeds <- ras partCount
        return Piece {seed=s,
                      beats=4,
                      parts=map ((createPart instruments) . mkStdGen) partSeeds}
      ) g
    where ra = state random
          raR = state . randomR
          ras = state . randoms'
          raRs i r = state $ randomRs' i r

randoms' :: (RandomGen g, Random a) => Int -> g -> ([a], g)
randoms' i g = ((take i $ randoms g), fst $ split g)

randomRs' :: (RandomGen g, Random a) => Int -> (a,a) -> g -> ([a], g)
randomRs' i r g = ((take i $ randomRs r g), fst $ split g)

createPart :: RandomGen g => [Int] -> g -> Part
createPart insts gen =
    let (val, nextGen) = random gen
    in  Part { pSeed=val,
               repeats=2, -- temporary fixed value
               tracks=createTracks insts nextGen}

createTracks :: RandomGen g => [Int] -> g -> [Track]
createTracks [] _ = []
createTracks (i:is) gen =
    let (val, nextGen) = random gen
        s = createTrack i val
    in  s : createTracks is nextGen

createTrack :: Int -> Int -> Track
createTrack i s = 
    let (pw:nw:rw:_) = randomRs (-10, 10) (mkStdGen s)
    in  Track {tSeed=s,
               inst=i,
               playWeight=pw,
               noteDurWeight=nw,
               restDurWeight=rw}

{--
 - Merging Reseaarching~ (pronounciation guide to follow)
 -
 - Merging will be some amount of the random space of createPiece seeds  which randomly
 - finds two seeds and can replay them. If there aren't enough parents in the history
 - list then the gen should be rerolled until it chooses not to merge and that value
 - should be saved.
 -
 - Steps of mutation should not repeatedly reuse their given RandomGen but should instead
 - split a gen off of it and use that for their internal processing, while passing
 - the next generator(s) off to substeps unaffected.
 -
 - Potentially a RandomGen could be made that uses a fed in list of numbers up till
 - a point.
 -}
{--
 - Mutation station
 -
 - Split off whenever the shape of the Piece or backing data would effect RandomGen generation
 - If splitting is necessary:
 - lg = local generator, the generator used within the method to decide things randomly
 - sg = sub generator, the generator(s) given to whatever sub operations
 -}
-- take a random gen and return a mutator with a split off random gen applied to it
mutate :: RandomGen g => Piece -> g -> Piece
mutate p g =
    let mutators = [
          addPart,
          removePart,
          addTrack,
          modifyRepeats,
          modifyPlayWeights,
          modifyNoteDuration,
          modifyRestDuration]
        (lg, sg) = split g
        (index, _) = randomR (0, (length mutators) - 1) lg
    in  (mutators!!index) sg p

addPart :: RandomGen g => g -> Piece -> Piece
addPart g p =
    let pas = parts p
        (lg, sg) = split g
        newPart = createPart (map inst $ tracks $ head pas) sg -- just one strategy for picking
        i = fst $ randomR (0, (length pas) - 1) lg
        (left, right) = splitAt i pas
    in  p{parts=left++(newPart:right)}

removePart :: RandomGen g => g -> Piece -> Piece
removePart g p =
    let pas = (parts p)
        indexToRemove = fst $ randomR (0, (length pas) - 1) g
        (left, (_:right)) = splitAt indexToRemove pas
    in  if (length pas) > 1 then p{parts=(left++right)} else p

addTrack :: RandomGen g => g -> Piece -> Piece
addTrack g p = -- adds the same track to each part changed
    let (lg, sg) = split g
        (i, nlg) = randomR percussionRange lg -- should probably check for collision
        nTrack = createTrack i $ fst $ random nlg
        adder _ pa = pa{tracks=nTrack:(tracks pa)}
    in  whichParts adder sg p

modifyRepeats :: RandomGen g => g -> Piece -> Piece
modifyRepeats g p =
    let (factor, ng) = randomR (0,3) g
    in  whichParts (\ _ pa -> pa{repeats=2^(factor::Int)}) ng p

modifyPlayWeights :: RandomGen g => g -> Piece -> Piece
modifyPlayWeights = whichParts (whichTracks (\g t -> t{playWeight=(playWeight t) + (weighter g)}))

modifyNoteDuration :: RandomGen g => g -> Piece -> Piece
modifyNoteDuration = whichParts (whichTracks (\g t -> t{noteDurWeight=(noteDurWeight t) + (weighter g)}))

modifyRestDuration :: RandomGen g => g -> Piece -> Piece
modifyRestDuration = whichParts (whichTracks (\g t -> t{restDurWeight=(restDurWeight t) + (weighter g)}))

{--
 - M(util)ators
 -
 - utils for mutatin'
 -}
weighter :: RandomGen g => g -> Int -- swings by at most +-10
weighter g =
    let (out, _)= randomR (-1000, 1000) g
    in  round $ (out :: Int) % 100

whichParts :: RandomGen g => (g -> Part -> Part) -> g -> Piece -> Piece
whichParts t g p = -- probably should balance the "All" against the number of items in the list
    let options = [
          mutateAllParts,
          mutateRandomPart]
        (index, sg) = randomR (0, (length options) - 1) g
    in  (options!!index) t sg p

mutateAllParts :: RandomGen g => (g -> Part -> Part) -> g -> Piece -> Piece
mutateAllParts   f g p = p{parts=mutateAllInList f g (parts p)}

mutateRandomPart :: RandomGen g => (g -> Part -> Part) -> g -> Piece -> Piece
mutateRandomPart f g p = p{parts=mutateRandomInList f g (parts p)}

whichTracks :: RandomGen g => (g -> Track -> Track) -> g -> Part -> Part
whichTracks t g pa = -- probably should balance the "All" against the number of items in the list
    let options = [
          mutateAllTracks,
          mutateRandomTrack]
        (index, nextG) = randomR (0, (length options) - 1) g
    in  (options!!index) t nextG pa

mutateAllTracks :: RandomGen g => (g -> Track -> Track) -> g -> Part -> Part
mutateAllTracks f g pa = pa{tracks=mutateAllInList f g (tracks pa)}

mutateRandomTrack :: RandomGen g => (g -> Track -> Track) -> g -> Part -> Part
mutateRandomTrack f g pa = pa{tracks=mutateRandomInList f g (tracks pa)}

mutateAllInList :: RandomGen g => (g -> a -> a) -> g -> [a] -> [a]
mutateAllInList _ _ [] = []
mutateAllInList f g (x:xs) =
    let (_, ng) = next g
    in  f ng x : mutateAllInList f ng xs

mutateRandomInList :: RandomGen g => (g -> a -> a) -> g -> [a] -> [a]
mutateRandomInList f g list =
    let (val, nextGen) = randomR (0, (length list) - 1) g
        mutater _ _ [] = []
        mutater t i (x:xs) = (if i == 0 then t x else x):(mutater t (i - 1) xs)
    in  mutater (f nextGen) val list

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
loadPiece (Piece _ b p) = line $ foldl (++) [] $ map (partToMeasures b) p

partToMeasures :: Int -> Part -> [Music (Pitch)]
partToMeasures b pa = take (repeats pa) $ repeat (chord $ map (trackToMeasure b) (tracks pa))

trackToMeasure :: Int -> Track -> Music (Pitch)
trackToMeasure b Track{tSeed=s, inst=i, playWeight=pw, noteDurWeight=nw, restDurWeight=rw} =
    let randomBounds@(lower,higher) = (1,100)
        sound = perc (toEnum i::PercussionSound)
        durs = [sn,en,qn,hn,wn] -- supporting sixteenth to whole notes, no dots
        randomList = randomRs randomBounds (mkStdGen s)
        playThreshold = min (lower-1) $ max (higher-1) (round ((higher - lower) % 2) + pw)

        durator space weight r =
            let candidates = filter (\n -> (denominator $ space / n) == 1) durs
                percent = (toRational $ r + weight) / (toRational $ (higher-lower) % (length candidates))
            in  candidates!!(min ((length candidates) - 1) $ max 0 (round percent))

        consume _ []    = []
        consume _ [_]   = []
        consume space (r1:r2:rs)
            | space > 0 =
                   let isNote = r1 > playThreshold
                       event = if isNote then sound else rest 
                       d = durator space (if isNote then nw else rw) r2
                   in  event d : consume (space - d) rs
            | otherwise = []
    in  line $ consume (toRational $ b % 4) randomList

{--
 - Utility functions for overall system
 -}

pp :: PlayParams
pp = defParams{closeDelay=0} -- may need to tune this but it is better then the 1 second delay

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
        die err   = do
                   tid <- myThreadId
                   print ("Playback Thread " ++ show tid ++ " died with exception " ++ show (err :: ErrorCall))
                   stop
        pieceWork :: [Music Pitch] -> IO ()
        pieceWork [] = work
        pieceWork (x:xs) = do
                   noNewMessage <- atomically(isEmptyTMVar workVar) -- peek at the state
                   if noNewMessage then do
                                             playC pp x
                                             pieceWork (xs ++ [x])
                                   else work
        work :: IO ()
        work    = do
                   mJob <- atomically(takeTMVar workVar) -- this should block on receiving a value
                   case mJob of Nothing -> work -- waiting on a new job
                                Just music -> E.catch (pieceWork $ lineToList music) die

    _ <- forkIO work

    return (write . Just, stop)

-- including git commit info so that files can be matched up against the code that can render them
gitInfo :: IO String
gitInfo = readProcess "git" [ "log", "-n", "1"] "" >>= (\a -> return $ (head $ lines a) ++ "\n")

saveToFile :: String -> String -> IO ()
saveToFile filename contents = do
    gi <- gitInfo
    writeFile filename $ gi ++ contents
    return ()

