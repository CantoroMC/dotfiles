module XMonad.Local.Manage.Util
    ( xmScratchpads
    , xmBigRect
    , xmMedRect
    , xmUpLeftRect
    , xmUpRightRect
    , xmDownLeftRect
    , xmDownRightRect
    ) where

import XMonad
import qualified XMonad.StackSet as XMSS

import XMonad.Util.NamedScratchpad
    ( NamedScratchpad (..)
    , customFloating
    )

xmScratchpads :: [NamedScratchpad]
xmScratchpads =
    [ NS "cmus"    cmdCmus    queryCmus    hookCmus
    , NS "yakuake" cmdYakuake queryYakuake hookYakuake
    , NS "orgenda" cmdOrgenda queryOrgenda hookOrgenda
    ] where cmdCmus   = "st -n cmus cmus"
            queryCmus = resource =? "cmus"
            hookCmus  = customFloating xmMedRect

            cmdYakuake   = "st -n yakuake"
            queryYakuake = resource =? "yakuake"
            hookYakuake  = customFloating xmBigRect

            cmdOrgenda   = "emacs --name='orgenda' ~/Documents/organization/Notes.org"
            queryOrgenda = resource =? "orgenda"
            hookOrgenda  = customFloating xmBigRect


xmBigRect :: XMSS.RationalRect
xmBigRect = XMSS.RationalRect 0.166 0.166 0.666 0.666

xmMedRect :: XMSS.RationalRect
xmMedRect = XMSS.RationalRect 0.25 0.25 0.5 0.5

xmUpLeftRect :: XMSS.RationalRect
xmUpLeftRect = XMSS.RationalRect 0 0.02 0.5 0.49

xmUpRightRect :: XMSS.RationalRect
xmUpRightRect = XMSS.RationalRect 0.5 0.02 0.5 0.49

xmDownLeftRect :: XMSS.RationalRect
xmDownLeftRect = XMSS.RationalRect 0 0.51 0.5 0.49

xmDownRightRect :: XMSS.RationalRect
xmDownRightRect = XMSS.RationalRect 0.5 0.51 0.5 0.49
