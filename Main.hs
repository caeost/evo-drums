#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
module Main where

import Euterpea hiding(a,as,b,bs,c,cs,d,ds,e,es,f,fs,g,gs, left, right, merge)
import System.Random
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Exception as E
import Control.Concurrent.STM
import Data.List (isPrefixOf, sort)
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

    let queuePlay = sendToPlayer . loadPiece
        act :: [Form] -> Bool -> IO ()
        -- TODO playing should not be set in here but instead from what is actually playing
        act changes playing = do
            let continue = act changes playing
                playChanges [] = stop >> act [] False
                playChanges c@(x:_) = queuePlay (reify x) >> act c True

            c <- getChar
            showMain $ "Entered: " ++ show c --TODO have a change set displayed (like number of queued mutations)
            -- Start Interface
            case c of 'n' -> do -- generate and play a new piece
                              stdGen <- newStdGen
                              playChanges ((Form 0 (show stdGen) Self):changes)
                      'p' -> do -- pause / unpause playing music
                              if playing
                                  then stop >> act changes False
                                  else playChanges changes
                      'm' -> do -- perform a random mutation of the playing track
                              case changes of
                                  (Form f g _):ys -> do
                                      stdGen <- newStdGen
                                      playChanges ((Form f (show stdGen) (MutatedFrom g)):changes)
                                  _ -> continue
                      'f' -> do -- interbreed tracks
                              stdGen <- newStdGen
                              if playing
                                  then do
                                      case changes of
                                          (x:xs) -> playChanges ((birth stdGen x (findMate xs)):changes)
                                          _ -> continue
                                  else do
                                      case changes of
                                          (_:_:_) -> do
                                              let (fg, sg) = split stdGen
                                                  mFstPar = findMate fg changes
                                                  sndPar = findMate sg changes -- TODO filter out fstPar
                                                  mChild = urges ug [mFstPar, sndPar]
                                              case mChild of
                                                  Nothing -> continue
                                                  Just child -> playChanges (child:changes)
                                          _ -> continue
                      'b' -> do -- go back one step
                              case changes of
                                  (_:xs) -> playChanges xs
                                  _ -> playChanges changes
                      'e' -> continue -- TODO edit piece as text
                      'w' -> do -- write songs to file (takes more input)
                              filename <- getLine
                              saveToFile filename (unlines $ map show changes)
                              continue
                      'r' -> do -- read file and play (takes more input)
                              getLine >>= readFormFile >>= playChanges
                      'u' -> do -- up vote for current piece makes it more used by 'f'
                              case changes of
                                  (x:xs) ->
                                      act (x{fitness=(fitness x) + 1}:xs) playing
                                  _ -> continue
                      'd' -> do -- down vote for current piece makes it less used by 'f'
                              case changes of
                                  (x:xs) ->
                                      act (x{fitness=(fitness x) - 1}:xs) playing
                                  _ -> continue
                      'c' -> do -- control Euterpea "Control"s (takes more input)
                              ic <- getChar
                              if ic == 'c'
                                  then clearControls
                                  else musicControl ic >>= sendControls
                              continue
                      _   -> do -- nothing, default case
                              continue
            -- End Interface
    act [] False

reify :: Form -> Piece
reify f = reify' f
    where reify' (Form _ i Self)            = createPiece (read i :: StdGen)
          reify' (Form _ i (MutatedFrom p)) = mutate (reify' p) (read i :: StdGen)
          reify' (Form _ i (BirthedBy x y)) = merge (read i :: StdGen) (reify' x) (reify' y)  -- TODO reversed for each level of birthing flip whether to accept the incoming result or reflip

findMate :: RandomGen g => g -> [Form] -> Maybe Form
findMate g l = select (sort l) $ fst $ randomR (0, (length l)) g
    where select [] _ = Nothing
          select (x:xs) i  =
              let post = i - fitness x
              in if post <= 0 then Just x else select xs post

-- TODO move to EvoGen.hs
-- TODO block self fertilization?
birth :: RandomGen g => g -> Form -> Form -> Form
birth g x y = Form ((fitness x + fitness y) / 2) (show g) (BirthedBy x y)

readFormFile :: String -> IO [[Form]]
readFormFile filename= do
    contents <- readFile filename
    return $ map (\a -> (read a) :: [Form]) $ filter (isPrefixOf "[") $ lines contents

musicControl :: Char -> IO (Maybe Control)
musicControl 't' = liftM (Just . Tempo . reader) getLine -- enter tempo exactly of form "Int % Int" eg "1 % 4"
    where reader at = read at :: Dur
musicControl _ = return Nothing

data Form = Form {
        fitness :: Int,
        gener   :: String,
        genesis :: Genesis
    } deriving (Read, Show) -- TODO add: | Int Piece so reification can be done for perf
instance Eq Form where
    (==) (Form _ a _) (Form _ b _) = a == b
instance Ord Form where -- TODO sorting by longest Genesis list would decrease unbalance of evolution?
    compare (Form a _ _) (Form b _ _) = compare a b

data Genesis = MutatedFrom Form | BirthedBy Form Form | Self deriving (Read, Show)

{--
 - Utility functions for IO
 -}
pp :: PlayParams
pp = defParams{closeDelay=0} -- may need to tune this but it is better then the 1 second delay

-- based off of https://wiki.haskell.org/Background_thread_example
spawnPlaybackChannel :: (Show a, ToMusic1 a, NFData a) => (String -> IO()) -> IO ([Music a] -> IO (), Maybe Control -> IO (), IO (), IO ())
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
        stop = write Nothing
        die err = do
            tid <- myThreadId
            print ("Playback Thread " ++ show tid ++ " died with exception " ++ show (err :: ErrorCall))
            stop
        pieceWork _ ec [] = work 0 ec
        pieceWork i ec xs = do
            noNewMessage <- atomically $ isEmptyTMVar workVar -- peek at the state
            if noNewMessage
                -- play loop run on every measure start!!
                then do
                    let safe = if i < length xs then i else 0
                        bm = xs!!safe
                        nextI = (safe+1) `mod` (length xs)

                    -- see if there are new controls
                    incoming <- atomically(readAllFromTQueue contQueue)
                    let controls = takeFromLastNothing $ (map Just ec) ++ incoming

                    renderF $ renderDisplay bm (xs!!nextI)
                    playC pp $ foldl (\m c -> Modify c m) bm controls
                    -- TODO write playing back to boost up score of track
                    pieceWork nextI controls xs
                else work i ec
        work :: Int -> [Control] -> IO ()
        work i c  = do
            mJob <- atomically(takeTMVar workVar)
            case mJob of
                Nothing -> work 0 c -- waiting on a new job
                Just music -> E.catch (pieceWork i c music) die

    _ <- forkIO $ work 0 []

    return (write . Just, control, clearControls, stop)

readAllFromTQueue :: TQueue a -> STM [a]
readAllFromTQueue q = do
    m <- tryReadTQueue q
    case m of
        Just a  -> do
            sub <- readAllFromTQueue q -- I think this breaks tail recursion optimization but I don't know how else to do it :(
            return $ a:sub
        Nothing -> return []

takeFromLastNothing :: [Maybe a] -> [a]
takeFromLastNothing ((Just c):xs) = c:takeFromLastNothing xs
takeFromLastNothing (Nothing:xs)  = takeFromLastNothing xs
takeFromLastNothing [] = []

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

