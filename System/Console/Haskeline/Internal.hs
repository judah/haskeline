module System.Console.Haskeline.Internal
    ( module System.Console.Haskeline.Monads
    , module System.Console.Haskeline.Term
    , module System.Console.Haskeline.Key
    , module System.Console.Haskeline.LineState
    , Behavior (..)
    , debugTerminalKeys ) where

import System.Console.Haskeline (defaultSettings, outputStrLn)
import System.Console.Haskeline.Command
import System.Console.Haskeline.InputT
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads
import System.Console.Haskeline.RunCommand
import System.Console.Haskeline.Term
import System.Console.Haskeline.Key

-- | This function may be used to debug Haskeline's input.
--
-- It loops indefinitely; every time a key is pressed, it will
-- print that key as it was recognized by Haskeline.
-- Pressing Ctrl-C will stop the loop.
--
-- Haskeline's behavior may be modified by editing your @~/.haskeline@
-- file.  For details, see: <https://github.com/judah/haskeline/wiki/CustomKeyBindings>
--
debugTerminalKeys :: IO a
debugTerminalKeys = runInputT defaultSettings $ do
    outputStrLn
        "Press any keys to debug Haskeline's input, or ctrl-c to exit:"
    rterm <- InputT ask
    case termOps rterm of
        Right _ -> error "debugTerminalKeys: not run in terminal mode"
        Left tops -> runInputCmdT tops $ runCommandLoop tops prompt
                                            loop emptyIM
  where
    loop = KeyMap $ \k -> Just $ Consumed $
            (const $ do
                effect (LineChange $ const ([],[]))
                effect (PrintLines [show k])
                setState emptyIM)
            >|> keyCommand loop
    prompt = stringToGraphemes "> "
