module EvoGen  where

import Euterpea hiding(a,as,b,bs,c,cs,d,ds,e,es,f,fs,g,gs, left, right)
import Control.Monad.State (state, runState)
import System.Random
import Data.Ratio

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

data Piece = Piece { beats :: Int, -- how many beats per measure in the Piece
                     parts :: [Part] -- sequential ordered segments of the Piece
                   } deriving (Show, Read, Eq)

data Part = Part { repeats :: Int, -- how many times to repeat this part for within the piece
                   tracks  :: [Track] -- different instruments that play simultaneously for this part
                 } deriving (Show, Read, Eq)

-- TODO weights 
data Track = Track  { seed :: Int, -- seed used to generate notes for this track
                      inst  :: Int, -- the instrument used by this track
                      playWeight :: Int, -- make the track more/less likely to play notes
                      noteDurWeight :: Int, -- make the track tend towards long/short notes
                      restDurWeight :: Int -- make the track tend towards long/short rests
                    } deriving (Show, Read, Eq)

data Weight = Int -- TODO just a flat value applied to everything
            | Fun Int Int Int -- TODO (function determining weight distribution, start, end, phase)
    deriving (Show, Read, Eq)

data Fun = Linear
         | Sin
         | Square
    deriving (Show, Read, Eq)

durs :: [Dur]
durs = [sn,en,qn,hn,wn] -- supporting sixteenth to whole notes, no dots
{--
 - Generating and manipulating a Piece
 -
 - A Piece is either created from a base random seed gotten via IO and in response to
 - a user action or it is loaded from persistence as an object that had been created
 - previously and saved.
 -
 - Within a Piece there is no randomness, the same Piece will not generate a different
 - composition on two different executions if the code is the same.
 -
 - Mutation and inter-breeding
 -
 - Pieces can also be created from other existing pieces by mutating and
 - returning a new copy. Mutations can be introduced which provide some kind of
 - shading via for example changing thresholds or adding other random patterns.
 - Inter-breeding can be done between Pieces to copy over some mutations from
 - two pieces randomly into a new one.
 -
 - There is also potential for more targeted manipulation later if desired. Being able to
 - directly boost a certain instrument's threshold to hear it less frequently for example.
 -}
percussionRange :: (Int, Int)
percussionRange = (0, 46) -- 47 percussion instruments are defined in Euterpea

createPiece :: RandomGen g => g -> Piece
createPiece g = fst $ runState (do
        instCount <- randomInRange (1, 10) -- hard coded for now
        instruments <- randomsInRange instCount percussionRange

        partCount <- randomInRange (1,4) -- hard coded for now
        parseeds <- ras partCount
        return Piece {beats=4, -- TODO letz waltz ... or whatever else
                      parts=map ((createPart instruments) . mkStdGen) parseeds}
    ) g
    where randomInRange = state . randomR
          ras = state . randoms'
          randomsInRange i r = state $ randomRs' i r

-- cannot use the semigroup <> operator since I want the RandomGen to lead to different children from the same parents
{- Implementation notes:
 -
 - Track merging within a Part is relatively simple:
 -  match up instrument types between parents and choose one or further merge them when there are two
 -  if only one side has that instrument then choose between it and nothing
 -  random coin flips consistently generated from a seed do the choosing
 -
 - Part merging is a little more complicated:
 -  Bound the maximum number of repeats by some constant
 -  Divide that space into 3 (or whatever) spaces: small, medium, large
 -  Go through the list with a function for each size that can merge parts if they are the same size
 -  if they aren't the same size then the large function steps down to medium etc. for one or both of its args
 -  if only one is not "large" (or whatever) then it may group together multiple of the others
 -  priority is always given to the largest type
 -  to merge it may choose one or the other (the other can be "nothing" too) or
 -  half of one or the other or
 -  intersperse them or something else
 -}
merge :: RandomGen g => g-> Piece -> Piece -> Piece
merge g p1 p2 = fst $ runState (do
        p1Beats <- state random
        partsSeed <- state random

        return Piece {beats= beats(if p1Beats then p1 else p2),
                      parts= mergeParts (mkStdGen partsSeed) (parts p1) (parts p2)}
    ) g

mergeParts :: RandomGen g => g -> [Part] -> [Part] -> [Part]
mergeParts _ [] bs = bs
mergeParts _ as [] = as
mergeParts g (a:as) (b:bs) = (if heads then a else b):mergeParts ng as bs
    where (heads, ng) = random g

randoms' :: (RandomGen g, Random a) => Int -> g -> ([a], g)
randoms' i g = ((take i $ randoms g), fst $ split g)

randomRs' :: (RandomGen g, Random a) => Int -> (a,a) -> g -> ([a], g)
randomRs' i r g = ((take i $ randomRs r g), fst $ split g)

createPart :: RandomGen g => [Int] -> g -> Part
createPart insts gen =
    let (lg, sg) = split gen
    in  Part { repeats=rLimitedPowerOfTwo lg,
               tracks=createTracks insts sg}

createTracks :: RandomGen g => [Int] -> g -> [Track]
createTracks [] _ = []
createTracks (i:is) gen =
    let (val, nextGen) = random gen
        s = createTrack i val
    in  s : createTracks is nextGen

createTrack :: Int -> Int -> Track
createTrack i s = 
    let (pw:nw:rw:_) = randomRs (-10, 10) (mkStdGen s)
    in  Track {seed=s,
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

--splitPart :: RandomGen g => g -> Piece -> Piece
--splitPart g p = p{parts=}

addTrack :: RandomGen g => g -> Piece -> Piece
addTrack g p = -- adds the same track to each part changed
    let (lg, sg) = split g
        (i, nlg) = randomR percussionRange lg -- should probably check for collision
        nTrack = createTrack i $ fst $ random nlg
        adder _ pa = pa{tracks=nTrack:(tracks pa)}
    in  whichParts adder sg p

modifyRepeats :: RandomGen g => g -> Piece -> Piece
modifyRepeats = whichParts (\g pa -> pa{repeats=rLimitedPowerOfTwo g})

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
weighter g = fst $ randomR (-10, 10) g

rLimitedPowerOfTwo :: RandomGen g => g -> Int
rLimitedPowerOfTwo g = 2^((fst $ randomR (0,3) g)::Int)

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
loadPiece :: Piece -> [Music Pitch] -- this Music a should be at the top level a "line" of measures
loadPiece (Piece b p) = foldl (++) [] $ map (partToMeasures b) p

partToMeasures :: Int -> Part -> [Music Pitch]
partToMeasures b pa = take (repeats pa) $ repeat (chord $ map (trackToMeasure b) (tracks pa))

-- TODO triplets!
trackToMeasure :: Int -> Track -> Music Pitch
trackToMeasure b Track{seed=s, inst=i, playWeight=pw, noteDurWeight=nw, restDurWeight=rw} =
    let randomBounds@(lower,higher) = (1,100)
        sound = perc (toEnum i::PercussionSound)
        randomList = randomRs randomBounds (mkStdGen s)
        playThreshold = max (lower-1) $ min (higher-1) (round ((higher - lower) % 2) + pw)

        durator space weight r =
            let candidates = filter (\n -> (denominator $ space / n) == 1) durs
                percent = (toRational $ r + weight) / (toRational $ (higher-lower) % (length candidates))
            in  candidates!!(min ((length candidates) - 1) $ max 0 (round percent))

        consume _ []    = []
        consume _ [_]   = []
        consume space (r1:r2:rs)
            | space > 0 =
                   let isNote = r1 > playThreshold -- TODO + or - with avg
                       event = if isNote then sound else rest
                       d = durator space (if isNote then nw else rw) r2
                   in  event d : consume (space - d) rs
            | otherwise = []
    in  line $ consume (toRational $ b % 4) randomList

