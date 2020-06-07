#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
module Main where

import Euterpea hiding(a,as,b,bs,c,cs,d,ds,e,es,f,fs,g,gs, left, right)
import System.Random
import System.IO
import Control.Concurrent
import Control.Exception as E
import Control.Concurrent.STM
import Data.List (isPrefixOf)
import System.Process
import EvoGen
import Control.DeepSeq

import Debug.Trace

{--
 - EvoDrums
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
    (showMain, showPlayback) <- spawnUXChannel
    (sendToPlayer, sendControls, clearControls, stop) <- spawnPlaybackChannel showPlayback -- all instructions take effect after the current measure

    let queuePlay = sendToPlayer . loadPiece . traceShowId
        act :: [[Form]] -> Maybe Piece -> IO ()
        act changes playing = do -- TODO playing should not be set in here but instead from what is actually playing
            let continue = act changes playing
                pauseAndContinue = stop >> act changes Nothing
                playAndContinue chas p  = queuePlay p >> (act chas $ Just p)
                playChangesAndContinue chas =
                    case chas of []   -> act [] Nothing
                                 (x:_)-> case parseToPiece x of Nothing -> stop >> act chas Nothing
                                                                Just p  -> playAndContinue chas p

            c <- getChar
            showMain $ "Entered: " ++ show c --TODO have a change set displayed (like 10x mutations)
            -- Start Interface
            case c of 'n' -> do -- generate and play a new piece
                              stdGen <- newStdGen
                              let p = createPiece stdGen
                              playAndContinue ([Form 0 $ show stdGen]:changes) p
                      'p' -> do -- pause / unpause playing music
                              case playing of Just _  -> pauseAndContinue
                                              Nothing -> playChangesAndContinue changes
                      'm' -> do -- perform a random mutation of the playing track
                              case playing of Just x -> do
                                                          stdGen <- newStdGen
                                                          let p = mutate x stdGen
                                                              (y:ys) = changes
                                                              (Form lf _) =  last y -- mutated track has same fitness as its predecessor
                                                              cur = y ++ [(Form lf $ show stdGen)]
                                                          playAndContinue (cur:ys) p
                                              Nothing -> continue
                      'f' -> do -- TODO interbreed tracks
                              case playing of Just _  -> continue -- TODO combine current track with random other
                                              Nothing -> continue -- TODO combine two random tracks
                      'b' -> do -- go back one step
                              case changes of []    -> pauseAndContinue
                                              (_:xs)-> playChangesAndContinue xs
                      'e' -> continue -- TODO edit piece as text
                      'w' -> do -- write songs to file (takes more input)
                              filename <- getLine
                              saveToFile filename (unlines $ map show changes)
                              continue
                      'r' -> do -- read file and play (takes more input)
                              getLine >>= readFormFile >>= playChangesAndContinue
                      'u' -> continue -- TODO up vote for current piece makes it more used by 'f'
                      'd' -> continue -- TODO down vote for current piece makes it less used by 'f'
                      'c' -> do -- Control Euterpea "Control"s (takes more input)
                              ic <- getChar
                              if ic == 'c'
                                  then clearControls
                                  else case playing of  Just _  -> musicControl ic sendControls
                                                        Nothing -> return ()
                              continue
                      _   -> do -- nothing, default case
                              continue
            -- End Interface
    act [] Nothing

parseToPiece :: [Form] -> Maybe Piece
parseToPiece []              = Nothing
parseToPiece ((Form _ i):ms) =
    let root = createPiece $ (read i :: StdGen)
        folder p (Form _ m) = mutate p $ (read m :: StdGen)
    in  Just $ foldl folder root ms


readFormFile :: String -> IO [[Form]]
readFormFile filename= do
    contents <- readFile filename
    return $ map (\a -> (read a) :: [Form]) $ filter (isPrefixOf "[") $ lines contents

musicControl :: Char -> (Control -> IO ()) -> IO ()
musicControl c out =
    do
      case c of 't' -> do -- enter tempo like 1 % 4
                        at <- getLine
                        let t = read at :: Dur
                        out $ Tempo t
                _   -> return ()

data Form = Form Int String deriving (Read, Show) -- TODO add: | Int Piece

{--
 - Utility functions for IO
 -}
pp :: PlayParams
pp = defParams{closeDelay=0} -- may need to tune this but it is better then the 1 second delay

-- based off of https://wiki.haskell.org/Background_thread_example
spawnPlaybackChannel :: (Show a, ToMusic1 a, NFData a) => (String -> IO()) -> IO (Music a -> IO (), Control -> IO (), IO (), IO ())
spawnPlaybackChannel renderF = do
    workVar <- atomically newEmptyTMVar
    contQueue <- atomically newTQueue

    let write j = atomically(do
                              empty <- isEmptyTMVar workVar
                              if empty
                                  then putTMVar workVar j
                                  else swapTMVar workVar j >> return ())
        control j = atomically(writeTQueue contQueue j)
        clearControls = control Nothing
        stop      = write Nothing
        die err = do
                   tid <- myThreadId
                   print ("Playback Thread " ++ show tid ++ " died with exception " ++ show (err :: ErrorCall))
                   stop
        pieceWork _ ec [] = work 0 ec
        pieceWork i ec xs = do
                   noNewMessage <- atomically $ isEmptyTMVar workVar -- peek at the state
                   if noNewMessage
                      then do
                          -- play loop run on every measure start!!
                          let safe = if i < length xs then i else 0
                              bm = xs!!safe
                              nextI = (safe+1) `mod` (length xs)

                          -- see if there are new controls
                          incoming <- atomically(readAllFromTQueue contQueue)
                          let controls = takeTillFirstNothing $ (reverse incoming) ++ (map Just ec)

                          renderF $ renderDisplay bm (xs!!nextI)
                          playC pp $ foldl (\m c -> Modify c m) bm controls
                          -- TODO write playing back to boost up score of track
                          pieceWork nextI controls xs
                      else work i ec
        work :: Int -> [Control] -> IO ()
        work i c  =
            do
                mJob <- atomically(takeTMVar workVar) -- this should block on receiving a value but I don't think it does??
                case mJob of Nothing -> work 0 c -- waiting on a new job
                             Just music -> E.catch (pieceWork i c $ lineToList music) die

    _ <- forkIO $ work 0 []

    return (write . Just, control . Just, clearControls, stop)

readAllFromTQueue :: TQueue a -> STM [a]
readAllFromTQueue q = do
                        m <- tryReadTQueue q
                        case m of Just a  -> do
                                              sub <- readAllFromTQueue q -- I think this breaks tail recursion optimization but I don't know how else to do it :(
                                              return $ a:sub
                                  Nothing -> return []

takeTillFirstNothing :: [Maybe a] -> [a]
takeTillFirstNothing ((Just c):mcs) = c:(takeTillFirstNothing mcs)
takeTillFirstNothing (Nothing:_)  = []
takeTillFirstNothing [] = []

renderDisplay :: Show a => Music a -> Music a -> String
renderDisplay m n =
    let current  = map ("\x1b[1m" ++) $ histiogram m -- bolded
        upcoming = map ("\x1b[0m" ++) $ histiogram n -- not bolded
        cho = chordToList m
    in  (unlines $ map (\(a,b) -> a ++ b) $ zip current upcoming) ++ "\n" ++ show cho

histiogram :: Music a -> [String]
histiogram m =
    let s = foldl1 min durs
        trailingSpace o = round $ ((dur o) / s) - 1
        numberize o = lineForANote o : (take (trailingSpace o) $ repeat ' ')
    in  map (foldl1 (++)) $ map ((map numberize) . lineToList) $ chordToList m

lineForANote :: Music a -> Char
lineForANote (Prim (Rest _)) = ' '
lineForANote _               = '|'

chordToList :: Music a -> [Music a]
chordToList (Prim (Rest _))    = []
chordToList (n :=: ns)         = n : chordToList ns
chordToList _                  =
    error "chordToList: argument not created by function chord"

spawnUXChannel :: IO(String -> IO(), String -> IO ())
spawnUXChannel = do
    isDirty <- atomically (newTVar False)
    stateVar <- atomically (newTVar ("", ""))

    let render = do
                  d <- readTVarIO isDirty
                  if d == True
                    then do
                      (a,b) <- readTVarIO stateVar
                      callCommand "clear"
                      putStrLn $ a ++ "\n" ++ b
                      clean
                    else return ()
                  yield
                  render
        dirty = atomically $ writeTVar isDirty True
        clean = atomically $ writeTVar isDirty False
        write (Left m)  = atomically(modifyTVar stateVar (\(_,b) -> (m, b))) >> dirty
        write (Right m) = atomically(modifyTVar stateVar (\(a,_) -> (a, m))) >> dirty

    _ <- forkIO render

    return (write . Left, write . Right)

-- including git commit info so that files can be matched up against the code that can render them
gitInfo :: IO String
gitInfo = readProcess "git" [ "log", "-n", "1"] "" >>= (\a -> return $ (head $ lines a) ++ "\n")

saveToFile :: String -> String -> IO ()
saveToFile filename contents = do
    gi <- gitInfo
    writeFile filename $ gi ++ contents
    return ()

