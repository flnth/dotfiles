-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.ServerMode
-- Copyright   :  (c) Peter Olson 2013 and Andrea Rossato and David Roundy 2007
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  polson2@hawk.iit.edu
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is an 'EventHook' that will receive commands from an external
-- client. Also consider "XMonad.Hooks.EwmhDesktops" together with
-- > import Data.Char
-- > 
-- > main :: IO ()
-- > main = parse True "XMONAD_COMMAND" =<< getArgs
-- > 
-- > parse :: Bool -> String -> [String] -> IO ()
-- > parse input addr args = case args of
-- >         ["--"] | input -> repl addr
-- >                | otherwise -> return ()
-- >         ("--":xs) -> sendAll addr xs
-- >         ("-a":a:xs) -> parse input a xs
-- >         ("-h":_) -> showHelp
-- >         ("--help":_) -> showHelp
-- >         ("-?":_) -> showHelp
-- >         (a@('-':_):_) -> hPutStrLn stderr ("Unknown option " ++ a)
-- > 
-- >         (x:xs) -> sendCommand addr x >> parse False addr xs
-- >         [] | input -> repl addr
-- >            | otherwise -> return ()
-- > 
-- > 
-- > repl :: String -> IO ()
-- > repl addr = do e <- isEOF
-- >                case e of
-- >                 True -> return ()
-- >                 False -> do l <- getLine
-- >                             sendCommand addr l
-- >                             repl addr
-- > 
-- > sendAll :: String -> [String] -> IO ()
-- > sendAll addr ss = foldr (\a b -> sendCommand addr a >> b) (return ()) ss
-- > 
-- > sendCommand :: String -> String -> IO ()
-- > sendCommand addr s = do
-- >   d   <- openDisplay ""
-- >   rw  <- rootWindow d $ defaultScreen d
-- >   a <- internAtom d addr False
-- >   m <- internAtom d s False
-- >   allocaXEvent $ \e -> do
-- >                   setEventType e clientMessage
-- >                   setClientMessageEvent e rw a 32 m currentTime
-- >                   sendEvent d rw False structureNotifyMask e
-- >                   sync d False
-- > 
-- > showHelp :: IO ()
-- > showHelp = do pn <- getProgName
-- >               putStrLn ("Send commands to a running instance of xmonad. xmonad.hs must be configured with XMonad.Hooks.ServerMode to work.\n-a atomname can be used at any point in the command line arguments to change which atom it is sending on.\nIf sent with no arguments or only -a atom arguments, it will read commands from stdin.\nEx:\n" ++ pn ++ " cmd1 cmd2\n" ++ pn ++ " -a XMONAD_COMMAND cmd1 cmd2 cmd3 -a XMONAD_PRINT hello world\n" ++ pn ++ " -a XMONAD_PRINT # will read data from stdin.\nThe atom defaults to XMONAD_COMMAND.")
--
--
-- compile with: @ghc --make xmonadctl.hs@
--
-- run with
--
-- > xmonadctl command
--
-- or with
--
-- > $ xmonadctl
-- > command1
-- > command2
-- > .
-- > .
-- > .
-- > ^D
--
-- Usage will change depending on which event hook(s) you use. More examples are shown below.
--
-----------------------------------------------------------------------------

module ServerMode
    ( -- * Usage
      -- $usage
      fnServerModeEventHook 
    , myServerModeeventHook'
    , myServerModeeventHookCmd
    , myServerModeeventHookCmd'
    , myServerModeeventHookF
    ) where

import Control.Monad (when)
import Data.Maybe
import Data.Monoid
import System.IO

import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.Navigation2D

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.ServerMode
--
-- Then edit your @handleEventHook@ by adding the appropriate event hook from below

-- | Executes a command of the list when receiving its index via a special ClientMessageEvent
-- (indexing starts at 1). Sending index 0 will ask xmonad to print the list of command numbers
-- in stderr (so that you can read it in @~\/.xsession-errors@). Uses "XMonad.Actions.Commands#defaultCommands" as the default.
--
-- > main = xmonad def { handleEventHook = myServerModeeventHook }
--
-- > xmonadctl 0 # tells xmonad to output command list
-- > xmonadctl 1 # tells xmonad to switch to workspace 1
--

-- | A nice pre-defined list of commands.
myCommands :: X [(String, X ())]
myCommands = do
    return $
        [
        ("go-left"               , windowGo L False                               )
        , ("go-down"             , windowGo D False                               )
        , ("go-up"               , windowGo U False                               )
        , ("go-right"            , windowGo R False                               )
        , ("next-layout"         , sendMessage NextLayout                           )
        , ("default-layout"      , asks (layoutHook . config) >>= setLayout         )
        , ("run"                 , spawn "exe=`dmenu_path | dmenu -b` && exec $exe" )
        ]

fnServerModeEventHook :: Event -> X All
fnServerModeEventHook = myServerModeeventHook' myCommands

-- | myServerModeeventHook' additionally takes an action to generate the list of
-- commands.
myServerModeeventHook' :: X [(String,X ())] -> Event -> X All
myServerModeeventHook' cmdAction ev = myServerModeeventHookF "XMONAD_COMMAND" (sequence_ . map helper . words) ev
        where helper cmd = do cl <- cmdAction
                              case lookup cmd (zip (map show [1 :: Integer ..]) cl) of
                                Just (_,action) -> action
                                Nothing         -> mapM_ (io . hPutStrLn stderr) . listOfCommands $ cl
              listOfCommands cl = map (uncurry (++)) $ zip (map show ([1..] :: [Int])) $ map ((++) " - " . fst) cl


-- | Executes a command of the list when receiving its name via a special ClientMessageEvent.
-- Uses "XMonad.Actions.Commands#defaultCommands" as the default.
--
-- > main = xmonad def { handleEventHook = myServerModeeventHookCmd }
--
-- > xmonadctl run # Tells xmonad to generate a run prompt
--


myServerModeeventHookCmd :: Event -> X All
myServerModeeventHookCmd = myServerModeeventHookCmd' myCommands

-- | Additionally takes an action to generate the list of commands
myServerModeeventHookCmd' :: X [(String,X ())] -> Event -> X All
myServerModeeventHookCmd' cmdAction ev = myServerModeeventHookF "XMONAD_COMMAND" (sequence_ . map helper . words) ev
        where helper cmd = do cl <- cmdAction
                              fromMaybe (io $ hPutStrLn stderr ("Couldn't find command " ++ cmd)) (lookup cmd cl)

-- | Listens for an atom, then executes a callback function whenever it hears it.
-- A trivial example that prints everything supplied to it on xmonad's standard out:
--
-- > main = xmonad def { handleEventHook = myServerModeeventHookF "XMONAD_PRINT" (io . putStrLn) }
--
-- > xmonadctl -a XMONAD_PRINT "hello world"
--
myServerModeeventHookF :: String -> (String -> X ()) -> Event -> X All
myServerModeeventHookF key func (ClientMessageEvent {ev_message_type = mt, ev_data = dt}) = do
        d <- asks display
        atm <- io $ internAtom d key False
        when (mt == atm && dt /= []) $ do
         let atom = fromIntegral $ toInteger $ foldr1 (\a b -> a + (b*2^(32::Int))) dt
         cmd <- io $ getAtomName d atom
         case cmd of
              Just command -> func command
              Nothing -> io $    hPutStrLn stderr ("Couldn't retrieve atom " ++ (show atom))
        return (All True)
myServerModeeventHookF _ _ _ = return (All True)
