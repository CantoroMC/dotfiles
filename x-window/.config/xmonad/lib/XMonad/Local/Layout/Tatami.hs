{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Local.Layout.Tatami
    ( Tatami(..)
    ) where

import           Control.Monad                  ( msum )
import           XMonad                         ( IncMasterN(IncMasterN)
                                                , LayoutClass
                                                    ( description
                                                    , handleMessage
                                                    , pureLayout
                                                    )
                                                , Rectangle(Rectangle)
                                                , Resize(Expand, Shrink)
                                                , fromMessage
                                                , splitHorizontallyBy
                                                , splitVertically
                                                )
import qualified XMonad.StackSet               as XMSS

data Tatami a = Tatami
    { tatamiNMaster :: !Int
    , tatamiDelta   :: !Rational
    , tatamiFMaster :: !Rational
    }
    deriving (Show, Read)

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

tatami :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tatami f r nm n
    | nslave <= 0 || nm == 0 = splitVertically n r
    | otherwise = splitVertically nm r1 ++ mat nns (head rms) ++ concatMap
        matOfFive
        (tail rms)
  where
    (r1, r2) = splitHorizontallyBy f r
    nslave   = n - nm
    nmat     = div (nslave - 1) 5 + 1
    rms      = splitVertically nmat r2
    nns      = nslave - ((nmat - 1) * 5)

mat :: Int -> Rectangle -> [Rectangle]
mat nns (Rectangle sx sy sw sh)
    | nns == 3  = matOfThree (Rectangle sx sy sw sh)
    | nns == 4  = matOfFour (Rectangle sx sy sw sh)
    | nns == 5  = matOfFive (Rectangle sx sy sw sh)
    | otherwise = splitVertically nns (Rectangle sx sy sw sh)

matOfThree :: Rectangle -> [Rectangle]
matOfThree (Rectangle sx sy sw sh) =
    [ Rectangle sx sy smallw (2 * fromIntegral smallh)
    , Rectangle (sx + fromIntegral smallw) sy smallw (2 * fromIntegral smallh)
    , Rectangle sx (sy + 2 * fromIntegral smallh) sw smallh
    ]  where
    smallh = div sh 3
    smallw = div sw 2

matOfFour :: Rectangle -> [Rectangle]
matOfFour (Rectangle sx sy sw sh) =
    [ Rectangle sx sy                         sw     smallh
    , Rectangle sx (sy + fromIntegral smallh) smallw (2 * fromIntegral smallh)
    , Rectangle (sx + fromIntegral smallw)
                (sy + fromIntegral smallh)
                smallw
                (2 * fromIntegral smallh)
    , Rectangle sx (sy + 3 * fromIntegral smallh) sw smallh
    ]  where
    smallh = div sh 4
    smallw = div sw 2

matOfFive :: Rectangle -> [Rectangle]
matOfFive (Rectangle sx sy sw sh) =
    [ Rectangle sx sy smallw (2 * fromIntegral smallh)
    , Rectangle (sx + fromIntegral smallw) sy (2 * fromIntegral smallw) smallh
    , Rectangle (sx + fromIntegral smallw)
                (sy + fromIntegral smallh)
                smallw
                smallh
    , Rectangle (sx + 2 * fromIntegral smallw)
                (sy + fromIntegral smallh)
                smallw
                (2 * fromIntegral smallh)
    , Rectangle sx
                (sy + 2 * fromIntegral smallh)
                (2 * fromIntegral smallw)
                smallh
    ]  where
    smallh = div sh 3
    smallw = div sw 3
