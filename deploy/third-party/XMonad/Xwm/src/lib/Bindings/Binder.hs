{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bindings.Binder
    ( Binder
    , (|/-)
    , (^>)
    , (...)
    , bind
    , bindAlias
    , bindZip
    , getBindings
    , mapBindings
    , storeBindings
    ) where



import Control.Arrow ((&&&))
import Control.Monad.Writer (MonadWriter(tell), Writer, WriterT(WriterT), execWriter)
import Data.Foldable (traverse_)
import qualified Data.Map as Map

import XMonad
import qualified XMonad.Util.ExtensibleState as XMES



newtype Binder a = Binder (Writer [Binding] a)
    deriving (Functor, Applicative, Monad)
newtype DocBindings = DocBindings String
    deriving (Eq, Semigroup, Monoid)


type KeyCombinationId = (ButtonMask, KeySym)
type KeyMap = Map.Map KeyCombinationId (X ())

data Binding = Binding
    { combination :: KeyCombination
    , explanation :: Explanation
    , action      :: X ()
    }
data KeyCombination = KeyCombination
    { modifier :: ButtonMask
    , key      :: KeySym
    }
    deriving (Eq, Ord)
data Explanation = Explanation String
                 | AliasExplanation KeyCombination String


-- Keys
keyCombinationToString :: KeyCombination -> String
keyCombinationToString c = take maxKeysStringLen $ modString <> keyString
  where
    modString = buttonMaskToString $ modifier c
    keyString = keysymToString $ key c

buttonMaskToString :: ButtonMask -> String
buttonMaskToString m = maybe "" (<> "-") $ Map.lookup m modMap
  where
    modMap = Map.fromList
        [ (mod1Mask                              , "Alt")
        , (mod1Mask .|. shiftMask                , "Alt-S")
        , (mod1Mask .|. controlMask              , "Alt-C")
        , (mod1Mask .|. controlMask .|. shiftMask, "Alt-C-S")
        , (mod2Mask                              , "M2")
        , (mod2Mask .|. shiftMask                , "M2-S")
        , (mod2Mask .|. controlMask              , "M2-C")
        , (mod2Mask .|. controlMask .|. shiftMask, "M2-C-S")
        , (mod3Mask                              , "M3")
        , (mod3Mask .|. shiftMask                , "M3-S")
        , (mod3Mask .|. controlMask              , "M3-C")
        , (mod3Mask .|. controlMask .|. shiftMask, "M3-C-S")
        , (mod4Mask                              , "Win")
        , (mod4Mask .|. shiftMask                , "Win-S")
        , (mod4Mask .|. controlMask              , "Win-C")
        , (mod4Mask .|. controlMask .|. shiftMask, "Win-C-S")
        , (mod4Mask .|. mod1Mask                 , "Win-Alt")
        , (mod5Mask                              , "M5")
        , (mod5Mask .|. shiftMask                , "M5-S")
        , (mod5Mask .|. controlMask              , "M5-C")
        , (mod5Mask .|. controlMask .|. shiftMask, "M5-C-S")
        ]

maxKeysStringLen :: Int
maxKeysStringLen = 20

-- Explanations and Descriptions
unexplanation :: Explanation -> String
unexplanation (Explanation e       ) = e
unexplanation (AliasExplanation _ e) = e

explain :: Explanation -> String
explain expl = case expl of
    Explanation e -> take paddingAmount padding <> e <> "\n"
    AliasExplanation kc e ->
        let alias = "(alias: " <> keyCombinationToString kc <> ")"
        in take paddingAmount (alias <> padding) <> e <> "\n"
  where
    paddingAmount = maxKeysStringLen + length "(alias: )"
    padding       = repeat ' '

describe :: Binding -> String
describe binding = keyString <> explain (explanation binding)
  where
    keyString =
        take maxKeysStringLen
            $  keyCombinationToString (combination binding)
            <> repeat ' '

describeBinds :: Foldable f => f Binding -> String
describeBinds = foldr ((<>) . describe) ""

-- Operators
(|/-) :: KeyCombination -> String -> X () -> Binding
(|/-) c e a =
    Binding { combination = c, explanation = Explanation e, action = a }
infix 3 |/-

(^>) :: (X () -> Binding) -> X () -> Binding
(^>) f = f
infixr 0 ^>

(...) :: ButtonMask -> KeySym -> KeyCombination
(...) m k = KeyCombination { modifier = m, key = k }
infix 4 ...

-- Binders
runBinder :: Binder a -> [Binding]
runBinder (Binder w) = execWriter w

bind :: Binding -> Binder ()
bind binding = Binder $ tell [binding]

bindAlias :: [KeyCombination] -> Binding -> Binder ()
bindAlias newCombinations binding = do
    bind binding
    traverse_ (bind . newBinding) newCombinations
  where
    newBinding c = Binding
        { combination = c
        , explanation = AliasExplanation
            (combination binding)
            (unexplanation $ explanation binding)
        , action      = action binding
        }

bindZip :: [KeyCombination] -> [String] -> [X ()] -> Binder ()
bindZip ks es as =
    traverse_ bind $ uncurry (uncurry (|/-)) <$> zip (zip ks es) as

instance ExtensionClass DocBindings
  where
    initialValue = DocBindings "no Bindings stored\n"

storeBindings :: X DocBindings -> XConfig a -> XConfig a
storeBindings explainableBindings xConfig = xConfig
    { startupHook = store <+> startupHook xConfig
    }  where
    store = do
        newDoc <- explainableBindings
        oldDoc <- XMES.get :: X DocBindings
        if oldDoc == initialValue
            then XMES.put newDoc
            else XMES.modify (<> newDoc)

getBindings :: X String
getBindings = do
    DocBindings doc <- XMES.get
    return doc

mapBindings
    :: (XConfig Layout -> Binder a) -> (XConfig Layout -> KeyMap, X DocBindings)
mapBindings binder =
    let
        bindMap xConfig = runBinder $ binder xConfig
        bindings =
            Map.fromList
                . fmap ((modifier &&& key) . combination &&& action)
                . bindMap
        doc = DocBindings . describeBinds . bindMap <$> reader config
    in (bindings, doc)

-- mapBindings' :: (XConfig Layout -> Binder a) -> XConfig Layout -> KeyMap
-- mapBindings' = fst . mapBindings
