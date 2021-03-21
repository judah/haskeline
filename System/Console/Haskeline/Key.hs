module System.Console.Haskeline.Key(Key(..),
            Modifier(..),
            BaseKey(..),
            noModifier,
            simpleKey,
            simpleChar,
            metaChar,
            ctrlChar,
            metaKey,
            ctrlKey,
            parseKey,
            setControlBits
            ) where

import Data.Bits
import Data.Char
import Data.Maybe
import Data.List (intercalate)
import Control.Monad

data Key = Key Modifier BaseKey
            deriving (Eq,Ord)

instance Show Key where
    show (Key modifier base)
        | modifier == noModifier = show base
        | otherwise = show modifier ++ "-" ++ show base

data Modifier = Modifier {hasControl, hasMeta, hasShift :: Bool}
            deriving (Eq,Ord)

instance Show Modifier where
    show m = intercalate "-"
            $ catMaybes [maybeUse hasControl "ctrl"
                        , maybeUse hasMeta "meta"
                        , maybeUse hasShift "shift"
                        ]
        where
            maybeUse f str = if f m then Just str else Nothing

noModifier :: Modifier
noModifier = Modifier False False False

-- Note: a few of these aren't really keys (e.g., KillLine),
-- but they provide useful enough binding points to include.
data BaseKey = KeyChar Char
             | FunKey Int
             | LeftKey | RightKey | DownKey | UpKey
             | KillLine | Home | End | PageDown | PageUp
             | Backspace | Delete
             | SearchReverse | SearchForward
            deriving (Eq, Ord)

instance Show BaseKey where
    show (KeyChar '\n') = "Return"
    show (KeyChar '\t') = "Tab"
    show (KeyChar '\ESC') = "Esc"
    show (KeyChar c)
        | isPrint c = [c]
        | isPrint unCtrld = "ctrl-" ++ [unCtrld]
        | otherwise = show c
      where
        unCtrld = toEnum (fromEnum c .|. ctrlBits)
    show (FunKey n) = 'f' : show n
    show LeftKey = "Left"
    show RightKey = "Right"
    show DownKey = "Down"
    show UpKey = "Up"
    show KillLine = "KillLine"
    show Home = "Home"
    show End = "End"
    show PageDown = "PageDown"
    show PageUp = "PageUp"
    show Backspace = "Backspace"
    show Delete = "Delete"
    show SearchReverse = "SearchReverse"
    show SearchForward = "SearchForward"

simpleKey :: BaseKey -> Key
simpleKey = Key noModifier

metaKey :: Key -> Key
metaKey (Key m bc) = Key m {hasMeta = True} bc

ctrlKey :: Key -> Key
ctrlKey (Key m bc) = Key m {hasControl = True} bc

simpleChar, metaChar, ctrlChar :: Char -> Key
simpleChar = simpleKey . KeyChar
metaChar = metaKey . simpleChar

ctrlChar = simpleChar . setControlBits

setControlBits :: Char -> Char
setControlBits '?' = toEnum 127
setControlBits c = toEnum $ fromEnum c .&. complement ctrlBits

ctrlBits :: Int
ctrlBits = bit 5 .|. bit 6

specialKeys :: [(String,BaseKey)]
specialKeys = [("left",LeftKey)
              ,("right",RightKey)
              ,("down",DownKey)
              ,("up",UpKey)
              ,("killline",KillLine)
              ,("home",Home)
              ,("end",End)
              ,("pagedown",PageDown)
              ,("pageup",PageUp)
              ,("backspace",Backspace)
              ,("delete",Delete)
              ,("return",KeyChar '\n')
              ,("enter",KeyChar '\n')
              ,("tab",KeyChar '\t')
              ,("esc",KeyChar '\ESC')
              ,("escape",KeyChar '\ESC')
              ,("reversesearchhistory",SearchReverse)
              ,("forwardsearchhistory",SearchForward)
              ]

parseModifiers :: [String] -> BaseKey -> Key
parseModifiers strs = Key mods
    where mods = foldl1 (.) (map parseModifier strs) noModifier

parseModifier :: String -> (Modifier -> Modifier)
parseModifier str m = case map toLower str of
    "ctrl" -> m {hasControl = True}
    "control" -> m {hasControl = True}
    "meta" -> m {hasMeta = True}
    "shift" -> m {hasShift = True}
    _ -> m

breakAtDashes :: String -> [String]
breakAtDashes "" = []
breakAtDashes str = case break (=='-') str of
    (xs,'-':rest) -> xs : breakAtDashes rest
    (xs,_) -> [xs]

parseKey :: String -> Maybe Key
parseKey str = fmap canonicalizeKey $ 
    case reverse (breakAtDashes str) of
        [ks] -> liftM simpleKey (parseBaseKey ks)
        ks:ms -> liftM (parseModifiers ms) (parseBaseKey ks)
        [] -> Nothing

parseBaseKey :: String -> Maybe BaseKey
parseBaseKey ks = lookup (map toLower ks) specialKeys
                `mplus` parseFunctionKey ks
                `mplus` parseKeyChar ks
    where
        parseKeyChar [c] | isPrint c = Just (KeyChar c)
        parseKeyChar _ = Nothing

        parseFunctionKey (f:ns) | f `elem` "fF" = case reads ns of
            [(n,"")]    -> Just (FunKey n)
            _           -> Nothing
        parseFunctionKey _ = Nothing

canonicalizeKey :: Key -> Key
canonicalizeKey (Key m (KeyChar c))
    | hasControl m = Key m {hasControl = False}
                        (KeyChar (setControlBits c))
    | hasShift m = Key m {hasShift = False} (KeyChar (toUpper c))
canonicalizeKey k = k
