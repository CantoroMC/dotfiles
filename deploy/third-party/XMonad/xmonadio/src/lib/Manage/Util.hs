module Manage.Util
    ( xmBigRect
    , xmMedRect
    , xmUpLeftRect
    , xmUpRightRect
    , xmDownLeftRect
    , xmDownRightRect
    ) where

import           XMonad
import qualified XMonad.StackSet               as XMSS

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
