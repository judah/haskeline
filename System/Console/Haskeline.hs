{- |

A rich user interface for line input in command-line programs.  Haskeline is
Unicode-aware and runs both on POSIX-compatible systems and on Windows.

Users may customize the interface with a @~/.haskeline@ file; see
<https://github.com/judah/haskeline/wiki/UserPreferences> for more information.

An example use of this library for a simple read-eval-print loop (REPL) is the
following:

> import System.Console.Haskeline
>
> main :: IO ()
> main = runInputT defaultSettings loop
>    where
>        loop :: InputT IO ()
>        loop = do
>            minput <- getInputLine "% "
>            case minput of
>                Nothing -> return ()
>                Just "quit" -> return ()
>                Just input -> do outputStrLn $ "Input was: " ++ input
>                                 loop

-}


module System.Console.Haskeline(
                    -- * Interactive sessions
                    -- ** The InputT monad transformer
                    InputT,
                    runInputT,
                    haveTerminalUI,
                    mapInputT,
                    -- ** Behaviors
                    Behavior,
                    runInputTBehavior,
                    defaultBehavior,
                    useFileHandle,
                    useFile,
                    preferTerm,
                    -- * User interaction functions
                    -- ** Reading user input
                    -- $inputfncs
                    getInputLine,
                    getInputLineWithInitial,
                    getInputChar,
                    getPassword,
                    waitForAnyKey,
                    -- ** Outputting text
                    -- $outputfncs
                    outputStr,
                    outputStrLn,
                    getExternalPrint,
                    -- * Customization
                    -- ** Settings
                    Settings(..),
                    defaultSettings,
                    setComplete,
                    -- ** User preferences
                    Prefs(),
                    readPrefs,
                    defaultPrefs,
                    runInputTWithPrefs,
                    runInputTBehaviorWithPrefs,
                    withRunInBase,
                    -- ** History
                    -- $history
                    getHistory,
                    putHistory,
                    modifyHistory,
                    -- * Ctrl-C handling
                    withInterrupt,
                    Interrupt(..),
                    handleInterrupt,
                    -- * Additional submodules
                    module System.Console.Haskeline.Completion)
                     where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import System.Console.Haskeline.Vi
import System.Console.Haskeline.Emacs
import System.Console.Haskeline.Prefs
import System.Console.Haskeline.History
import System.Console.Haskeline.Monads
import System.Console.Haskeline.InputT
import System.Console.Haskeline.Completion
import System.Console.Haskeline.Term
import System.Console.Haskeline.Key
import System.Console.Haskeline.RunCommand

import Control.Monad.Catch (MonadMask, handle)
import Data.Char (isSpace, isPrint)
import Data.Maybe (isJust)
import Control.Monad (when)
import System.IO


-- | A useful default.  In particular:
--
-- @
-- defaultSettings = Settings {
--           complete = completeFilename,
--           historyFile = Nothing,
--           autoAddHistory = True
--           }
-- @
defaultSettings :: MonadIO m => Settings m
defaultSettings = Settings {complete = completeFilename,
                        historyFile = Nothing,
                        autoAddHistory = True}

{- $outputfncs
The following functions enable cross-platform output of text that may contain
Unicode characters.
-}

-- | Write a Unicode string to the user's standard output.
outputStr :: MonadIO m => String -> InputT m ()
outputStr xs = do
    putter <- InputT $ asks putStrOut
    liftIO $ putter xs

-- | Write a string to the user's standard output, followed by a newline.
outputStrLn :: MonadIO m => String -> InputT m ()
outputStrLn = outputStr . (++ "\n")


{- $inputfncs
The following functions read one line or character of input from the user.

They return `Nothing` if they encounter the end of input.  More specifically:

- When using terminal-style interaction, they return `Nothing` if the user
  pressed @Ctrl-D@ when the input text was empty.

- When using file-style interaction, they return `Nothing` if an @EOF@ was
  encountered before any characters were read.
-}


{- | Reads one line of input.  The final newline (if any) is removed.  When using terminal-style interaction, this function provides a rich line-editing user interface.

If @'autoAddHistory' == 'True'@ and the line input is nonblank (i.e., is not all
spaces), it will be automatically added to the history.
-}
getInputLine :: (MonadIO m, MonadMask m)
            => String -- ^ The input prompt
                            -> InputT m (Maybe String)
getInputLine = promptedInput (getInputCmdLine emptyIM) $ runMaybeT . getLocaleLine

{- | Reads one line of input and fills the insertion space with initial text. When using
terminal-style interaction, this function provides a rich line-editing user interface with the
added ability to give the user default values.

This function behaves in the exact same manner as 'getInputLine', except that
it pre-populates the input area. The text that resides in the input area is given as a 2-tuple
with two 'String's.   The string on the left of the tuple (obtained by calling 'fst') is
what will appear to the left of the cursor and the string on the right (obtained by
calling 'snd') is what will appear to the right of the cursor.

Some examples of calling of this function are:

> getInputLineWithInitial "prompt> " ("left", "") -- The cursor starts at the end of the line.
> getInputLineWithInitial "prompt> " ("left ", "right") -- The cursor starts before the second word.
 -}
getInputLineWithInitial :: (MonadIO m, MonadMask m)
                            => String           -- ^ The input prompt
                            -> (String, String) -- ^ The initial value left and right of the cursor
                            -> InputT m (Maybe String)
getInputLineWithInitial prompt (left,right) = promptedInput (getInputCmdLine initialIM)
                                                (runMaybeT . getLocaleLine) prompt
  where
    initialIM = insertString left $ moveToStart $ insertString right $ emptyIM

getInputCmdLine :: (MonadIO m, MonadMask m) => InsertMode -> TermOps -> Prefix -> InputT m (Maybe String)
getInputCmdLine initialIM tops prefix = do
    emode <- InputT $ asks editMode
    result <- runInputCmdT tops $ case emode of
                Emacs -> runCommandLoop tops prefix emacsCommands initialIM
                Vi -> evalStateT' emptyViState $
                        runCommandLoop tops prefix viKeyCommands initialIM
    maybeAddHistory result
    return result

maybeAddHistory :: forall m . MonadIO m => Maybe String -> InputT m ()
maybeAddHistory result = do
    settings :: Settings m <- InputT ask
    histDupes <- InputT $ asks historyDuplicates
    doFlush <- InputT $ asks incAppendHistory
    case result of
        Just line | autoAddHistory settings && not (all isSpace line)
            -> let adder = case histDupes of
                        AlwaysAdd -> addHistory
                        IgnoreConsecutive -> addHistoryUnlessConsecutiveDupe
                        IgnoreAll -> addHistoryRemovingAllDupes
               in do
                 modifyHistory (adder line)
                 when doFlush $ case historyFile settings of
                   Nothing -> pure ()
                   Just f -> liftIO $ appendFile f (line ++ "\n")
        _ -> return ()

----------

{- | Reads one character of input.  Ignores non-printable characters.

When using terminal-style interaction, the character will be read without waiting
for a newline.

When using file-style interaction, a newline will be read if it is immediately
available after the input character.
-}
getInputChar :: (MonadIO m, MonadMask m) => String -- ^ The input prompt
                    -> InputT m (Maybe Char)
getInputChar = promptedInput getInputCmdChar $ \fops -> do
                        c <- getPrintableChar fops
                        maybeReadNewline fops
                        return c

getPrintableChar :: FileOps -> IO (Maybe Char)
getPrintableChar fops = do
    c <- runMaybeT $ getLocaleChar fops
    case fmap isPrint c of
        Just False -> getPrintableChar fops
        _ -> return c

getInputCmdChar :: (MonadIO m, MonadMask m) => TermOps -> Prefix -> InputT m (Maybe Char)
getInputCmdChar tops prefix = runInputCmdT tops
        $ runCommandLoop tops prefix acceptOneChar emptyIM

acceptOneChar :: Monad m => KeyCommand m InsertMode (Maybe Char)
acceptOneChar = choiceCmd [useChar $ \c s -> change (insertChar c) s
                                                >> return (Just c)
                          , ctrlChar 'l' +> clearScreenCmd >|>
                                        keyCommand acceptOneChar
                          , ctrlChar 'd' +> failCmd]

----------
{- | Waits for one key to be pressed, then returns.  Ignores the value
of the specific key.

Returns 'True' if it successfully accepted one key.  Returns 'False'
if it encountered the end of input; i.e., an @EOF@ in file-style interaction,
or a @Ctrl-D@ in terminal-style interaction.

When using file-style interaction, consumes a single character from the input which may
be non-printable.
-}
waitForAnyKey :: (MonadIO m, MonadMask m)
    => String -- ^ The input prompt
    -> InputT m Bool
waitForAnyKey = promptedInput getAnyKeyCmd
            $ \fops -> fmap isJust . runMaybeT $ getLocaleChar fops

getAnyKeyCmd :: (MonadIO m, MonadMask m) => TermOps -> Prefix -> InputT m Bool
getAnyKeyCmd tops prefix = runInputCmdT tops
    $ runCommandLoop tops prefix acceptAnyChar emptyIM
  where
    acceptAnyChar = choiceCmd
                [ ctrlChar 'd' +> const (return False)
                , KeyMap $ const $ Just (Consumed $ const $ return True)
                ]

----------
-- Passwords

{- | Reads one line of input, without displaying the input while it is being typed.
When using terminal-style interaction, the masking character (if given) will replace each typed character.

When using file-style interaction, this function turns off echoing while reading
the line of input.

Note that if Haskeline is built against a version of the @Win32@ library
earlier than 2.5, 'getPassword' will incorrectly echo back input on MinTTY
consoles (such as Cygwin or MSYS).
-}

getPassword :: (MonadIO m, MonadMask m) => Maybe Char -- ^ A masking character; e.g., @Just \'*\'@
                            -> String -> InputT m (Maybe String)
getPassword x = promptedInput
                    (\tops prefix -> runInputCmdT tops
                                        $ runCommandLoop tops prefix loop
                                        $ Password [] x)
                    (\fops -> withoutInputEcho fops $ runMaybeT $ getLocaleLine fops)
 where
    loop = choiceCmd [ simpleChar '\n' +> finish
                     , simpleKey Backspace +> change deletePasswordChar
                                                >|> loop'
                     , useChar $ \c -> change (addPasswordChar c) >|> loop'
                     , ctrlChar 'd' +> \p -> if null (passwordState p)
                                                then failCmd p
                                                else finish p
                     , ctrlChar 'l' +> clearScreenCmd >|> loop'
                     ]
    loop' = keyCommand loop

{- $history
The 'InputT' monad transformer provides direct, low-level access to the user's line history state.

However, for most applications, it should suffice to just use the 'autoAddHistory'
and 'historyFile' flags.

-}


-------
-- | Wrapper for input functions.
-- This is the function that calls "wrapFileInput" around file backend input
-- functions (see Term.hs).
promptedInput :: MonadIO m => (TermOps -> Prefix -> InputT m a)
                        -> (FileOps -> IO a)
                        -> String -> InputT m a
promptedInput doTerm doFile prompt = do
    -- If other parts of the program have written text, make sure that it
    -- appears before we interact with the user on the terminal.
    liftIO $ hFlush stdout
    rterm <- InputT ask
    case termOps rterm of
        Right fops -> liftIO $ do
                        putStrOut rterm prompt
                        wrapFileInput fops $ doFile fops
        Left tops -> do
            -- Convert the full prompt to graphemes (not just the last line)
            -- to account for the `\ESC...STX` appearing anywhere in it.
            let prompt' = stringToGraphemes prompt
            -- If the prompt contains newlines, print all but the last line.
            let (lastLine,rest) = break (`elem` stringToGraphemes "\r\n")
                                    $ reverse prompt'
            outputStr $ graphemesToString $ reverse rest
            doTerm tops $ reverse lastLine

{- | If Ctrl-C is pressed during the given action, throw an exception
of type 'Interrupt'.  For example:

> tryAction :: InputT IO ()
> tryAction = handle (\Interrupt -> outputStrLn "Cancelled.")
>                $ withInterrupt $ someLongAction

The action can handle the interrupt itself; a new 'Interrupt' exception will be thrown
every time Ctrl-C is pressed.

> tryAction :: InputT IO ()
> tryAction = withInterrupt loop
>     where loop = handle (\Interrupt -> outputStrLn "Cancelled; try again." >> loop)
>                    someLongAction

This behavior differs from GHC's built-in Ctrl-C handling, which
may immediately terminate the program after the second time that the user presses
Ctrl-C.

-}
withInterrupt :: (MonadIO m, MonadMask m) => InputT m a -> InputT m a
withInterrupt act = do
    rterm <- InputT ask
    wrapInterrupt rterm act

-- | Catch and handle an exception of type 'Interrupt'.
--
-- > handleInterrupt f = handle $ \Interrupt -> f
handleInterrupt :: MonadMask m => m a -> m a -> m a
handleInterrupt f = handle $ \Interrupt -> f

{- | Return a printing function, which in terminal-style interactions is
thread-safe and may be run concurrently with user input without affecting the
prompt. -}
getExternalPrint :: MonadIO m => InputT m (String -> IO ())
getExternalPrint = do
    rterm <- InputT ask
    return $ case termOps rterm of
        Right _ -> putStrOut rterm
        Left tops -> externalPrint tops
