{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Layout.Tatami
    ( Tatami(..)
    ) where

import Control.Monad ( msum )
import XMonad        ( IncMasterN(IncMasterN)
                     , LayoutClass ( description
                                   , handleMessage
                                   , pureLayout
                                   )
                     , Rectangle(Rectangle)
                     , Resize(Expand, Shrink)
                     , fromMessage
                     , splitHorizontallyBy
                     , splitVertically
                     )
import qualified XMonad.StackSet as XMSS

-------------------------------------------------------------------------------
    -- Tatami Layout

data Tatami a = Tatami
    { tatamiNMaster :: !Int
    , tatamiDelta   :: !Rational
    , tatamiFMaster :: !Rational
    } deriving (Show, Read)

instance LayoutClass Tatami a where
    pureLayout (Tatami nmaster _ frac) r s = zip ws rs
      where
        ws = XMSS.integrate s
        rs = tatami frac r nmaster (length ws)
    handleMessage l m = return $ msum
        [fmap resize (fromMessage m), fmap incmastern (fromMessage m)]
      where
        resize Shrink = l { tatamiFMaster = max 0 (frac - delta) }
        resize Expand = l { tatamiFMaster = min 1 (frac + delta) }
        incmastern (IncMasterN d) = l { tatamiNMaster = max 0 (nmaster + d) }
        nmaster = tatamiNMaster l
        delta   = tatamiDelta l
        frac    = tatamiFMaster l
    description _ = "Tatami"

-------------------------------------------------------------------------------
    -- Implementation of the Layout

tatami :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tatami f r nm n
    | nslave <= 0 || nm == 0 = splitVertically n r
    | otherwise              = splitVertically nm r1
                                ++ mat nns (head rms)
                                ++ concatMap matOfSix (tail rms)
  where
    (r1, r2) = splitHorizontallyBy f r
    nslave   = n - nm
    nmat     = div (nslave - 1) 6 + 1
    rms      = splitVertically nmat r2
    nns      = nslave - ((nmat - 1) * 6)

mat :: Int -> Rectangle -> [Rectangle]
mat nns r
    | nns == 3  = matOfThree r
    | nns == 4  = matOfFour r
    | nns == 5  = matOfFive r
    | nns == 6  = matOfSix r
    | otherwise = splitVertically nns r

matOfThree :: Rectangle -> [Rectangle]
matOfThree (Rectangle sx sy sw sh) =
    [ Rectangle sx
                sy
                smallw
                deltaY
    , Rectangle (sx + fromIntegral smallw)
                sy
                smallw
                deltaY
    , Rectangle sx
                (sy + fromIntegral deltaY)
                sw
                smallh
    ] where
    smallh = div sh 3
    smallw = div sw 2
    deltaY = 2 * smallh

matOfFour :: Rectangle -> [Rectangle]
matOfFour (Rectangle sx sy sw sh) =
    [ Rectangle sx
                sy
                sw
                smallh
    , Rectangle sx
                (sy + fromIntegral smallh)
                smallw
                deltaY
    , Rectangle (sx + fromIntegral smallw)
                (sy + fromIntegral smallh)
                smallw
                deltaY
    , Rectangle sx
                (sy + 3 * fromIntegral smallh)
                sw
                smallh
    ] where
    smallh = div sh 4
    smallw = div sw 2
    deltaY = 2 * smallh

matOfFive :: Rectangle -> [Rectangle]
matOfFive (Rectangle sx sy sw sh) =
    [ Rectangle sx
                sy
                smallw
                deltaY
    , Rectangle (sx + fromIntegral smallw)
                sy
                deltaX
                smallh
    , Rectangle (sx + fromIntegral smallw)
                (sy + fromIntegral smallh)
                smallw
                smallh
    , Rectangle (sx + fromIntegral deltaX)
                (sy + fromIntegral smallh)
                smallw
                deltaY
    , Rectangle sx
                (sy + fromIntegral deltaY)
                deltaX
                smallh
    ] where
    smallh = div sh 3
    smallw = div sw 3
    deltaX = 2 * smallw
    deltaY = 2 * smallh

matOfSix :: Rectangle -> [Rectangle]
matOfSix (Rectangle sx sy sw sh) =
    [ Rectangle sx
                sy
                deltaX
                smallh
    , Rectangle (sx + fromIntegral deltaX)
                sy
                deltaX
                smallh
    , Rectangle sx
                (sy + fromIntegral smallh)
                smallw
                deltaY
    , Rectangle (sx + fromIntegral smallw)
                (sy + fromIntegral smallh)
                deltaX
                smallh
    , Rectangle (sx + fromIntegral smallw)
                (sy + fromIntegral deltaY)
                deltaX
                smallh
    , Rectangle (sx + fromIntegral (deltaX+smallw))
                (sy + fromIntegral smallh)
                smallw
                deltaY
    ] where
    smallh = div sh 3
    smallw = div sw 4
    deltaY = 2 * smallh
    deltaX = 2 * smallw
