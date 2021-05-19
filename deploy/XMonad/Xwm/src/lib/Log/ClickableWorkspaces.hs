module Log.ClickableWorkspaces
    ( clickablePP
    ) where



import XMonad.Prelude ((<&>), (>=>))
import XMonad
import XMonad.Hooks.StatusBar.PP (xmobarAction, PP(..))
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import qualified XMonad.StackSet as XMSS
import Data.List (elemIndex)



clickableWrap :: (Show a, Num a) => a -> String -> String
clickableWrap i =
    xmobarAction ("xdotool set_desktop " ++ show i) "1"
    . xmobarAction ("xdotool key 0xffeb+0xffe1+" ++ show (i+1)) "3"

getWsIndex :: X (WorkspaceId -> Maybe Int)
getWsIndex = do
    wSort <- getSortByIndex
    spaces <- gets (map XMSS.tag . wSort . XMSS.workspaces . windowset)
    return $ flip elemIndex spaces

getClickable :: X (String -> WindowSpace -> String)
getClickable = getWsIndex <&> \idx s w -> maybe id clickableWrap (idx (XMSS.tag w)) s

-- | Apply clickable wrapping to the given PP.
clickablePP :: PP -> X PP
clickablePP pp = getClickable <&> \ren -> pp{ ppRename = ppRename pp >=> ren }
