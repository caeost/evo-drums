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
    (showMain, showPlayback) <- spawnUXChannel
    (sendToPlayer, stop) <- spawnPlaybackChannel showPlayback -- all instructions take effect after the current measure
    let queuePlay = sendToPlayer . loadPiece . traceShowId

        act :: [[String]] -> Maybe Piece -> IO ()
        act changes playing = do
            let continue = act changes playing
                pauseAndContinue = stop >> act changes Nothing
                playAndContinue chas p  = queuePlay p >> (act chas $ Just p)
                playChangesAndContinue chas = case chas of []   -> act [] Nothing
                                                           (x:_)-> playAndContinue chas (parseToPiece x)

            c <- getChar
            showMain $ "Entered: " ++ show c --TODO have a change set displayed (like 10x mutations)
            -- Start Interface
            case c of 'n' -> do -- generate and play a new piece
                              stdGen <- newStdGen
                              let p = createPiece stdGen
                              playAndContinue ([show stdGen]:changes) p
                      'p' -> do -- pause / unpause playing music
                              case playing of Just _  -> pauseAndContinue
                                              Nothing -> playChangesAndContinue changes
                      'm' -> do -- perform a random mutation of the playing track
                              case playing of Just x -> do
                                                          stdGen <- newStdGen
                                                          let p = mutate x stdGen
                                                          let (y:ys) = changes
                                                          playAndContinue ((y ++ [show stdGen]):ys) p
                                              Nothing -> continue
                      'b' -> do -- go back one step
                              case changes of []    -> pauseAndContinue
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
 - Utility functions for IO
 -}

pp :: PlayParams
pp = defParams{closeDelay=0} -- may need to tune this but it is better then the 1 second delay

-- based off of https://wiki.haskell.org/Background_thread_example
spawnPlaybackChannel :: (String -> IO()) -> IO (Music Pitch -> IO (), IO ())
spawnPlaybackChannel renderChannel = do
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
        pieceWork :: Int -> [Music Pitch] -> IO ()
        pieceWork _ [] = work
        pieceWork i xs = do
                   noNewMessage <- atomically(isEmptyTMVar workVar) -- peek at the state
                   if noNewMessage then do
                                          let m = xs!!i
                                          let nextI = (i+1) `mod` (length xs)
                                          renderChannel $ renderDisplay m (xs!!nextI)
                                          playC pp m
                                          pieceWork nextI xs
                                   else work
        work :: IO ()
        work    = do
                   mJob <- atomically(takeTMVar workVar) -- this should block on receiving a value
                   case mJob of Nothing -> work -- waiting on a new job
                                Just music -> E.catch (pieceWork 0 $ lineToList music) die

    _ <- forkIO work

    return (write . Just, stop)

renderDisplay :: Show a => Music a -> Music a -> String
renderDisplay m n =
    let current  = map ("\x1b[1m" ++) $ histiogram m-- bolded
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
chordToList (Prim (Rest 0))    = []
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

