import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W

devLayout = Mirror $ Tall 3 (3/100) (3/4)

myLayout = devLayout ||| Full

main :: IO ()
main = xmonad $ withNavigation2DConfig def $ def
  { modMask = mod4Mask  -- se Command/Super for mod
  , terminal = "kitty"
  , layoutHook = myLayout
  }
 `additionalKeysP`
  [ ("C-w h", windowGo L False)
  , ("C-w j", windowGo D False)
  , ("C-w k", windowGo U False)
  , ("C-w l", windowGo R False)
  ]
