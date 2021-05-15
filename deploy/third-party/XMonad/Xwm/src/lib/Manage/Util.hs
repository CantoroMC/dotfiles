module Manage.Util
    ( xwmBigRect
    , xwmMedRect
    , xwmSmallRect
    , xwmUpLeftRect
    , xwmUpRightRect
    , xwmDownLeftRect
    , xwmDownRightRect
    ) where



import qualified XMonad.StackSet as XMSS



xwmBigRect :: XMSS.RationalRect
xwmBigRect = XMSS.RationalRect 0.166 0.166 0.666 0.666

xwmMedRect :: XMSS.RationalRect
xwmMedRect = XMSS.RationalRect 0.25 0.25 0.5 0.5

xwmSmallRect :: XMSS.RationalRect
xwmSmallRect = XMSS.RationalRect 0.3 0.3 0.3 0.3

xwmUpLeftRect :: XMSS.RationalRect
xwmUpLeftRect = XMSS.RationalRect 0 0.02 0.5 0.49

xwmUpRightRect :: XMSS.RationalRect
xwmUpRightRect = XMSS.RationalRect 0.5 0.02 0.5 0.49

xwmDownLeftRect :: XMSS.RationalRect
xwmDownLeftRect = XMSS.RationalRect 0 0.51 0.5 0.49

xwmDownRightRect :: XMSS.RationalRect
xwmDownRightRect = XMSS.RationalRect 0.5 0.51 0.5 0.49
