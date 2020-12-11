module XMonad.Local.Manage.Util
    ( xmScratchpads
    , xmBigRect
    , xmMedRect
    ) where

import XMonad
import qualified XMonad.StackSet as XMSS

import XMonad.Util.NamedScratchpad
    ( NamedScratchpad (..)
    , customFloating
    )

xmScratchpads :: [NamedScratchpad]
xmScratchpads =
    [ NS "ncmpcpp" cmdNcmcpp queryNcmcpp hookNcmcpp
    , NS "yakuake" cmdYakuake queryYakuake hookYakuake
    , NS "orgenda" cmdOrgenda queryOrgenda hookOrgenda
    ] where cmdNcmcpp   = "st -n ncmpcpp ncmpcpp"
            queryNcmcpp = resource =? "ncmpcpp"
            hookNcmcpp  = customFloating xmMedRect

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