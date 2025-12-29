{-# LANGUAGE TypeFamilies #-}

import Data.Char (isSpace)
import Data.List (find, sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (lookupEnv)
import XMonad
import XMonad.Actions.FocusNth
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Decoration hiding (Theme)
import XMonad.Layout.DecorationEx
import XMonad.Layout.DecorationEx.Common
import XMonad.Layout.DecorationEx.Engine
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Util.EZConfig
import XMonad.Util.Font
import XMonad.Util.Paste
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
import qualified XMonad.StackSet as W

sigils = ["a", "b", "c", "d", "e", "g", "i", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

sigilMapping :: WindowSet -> [(Window, String)]
sigilMapping winds =
  let screens = sortBy (\x y -> compare (W.screen x) (W.screen y)) (W.current winds : W.visible winds)
      ws = concatMap (W.integrate' . W.stack . W.workspace) screens
  in zip ws sigils

windowSigil :: Window -> X String
windowSigil w = windowSigil' w `fmap` windowset `fmap` get

windowSigil' :: Window -> WindowSet -> String
windowSigil' w = fromMaybe "?" . fmap snd . find ((==w) . fst) . sigilMapping

sigilWindow' :: String -> WindowSet -> Maybe Window
sigilWindow' sigil = fmap fst . find ((==sigil) . snd) . sigilMapping

focusSigil :: String -> X ()
focusSigil sigil = do
  ws <- windowset `fmap` get
  case fmap fst $ find ((== sigil) . snd) (sigilMapping ws) of
    Just w  -> windows $ W.focusWindow w
    Nothing -> return ()

swapSigil :: String -> X ()
swapSigil sigil = windows $ swapSigil' sigil

swapSigil' :: String -> WindowSet -> WindowSet
swapSigil' sigil winSet =
  case (W.peek winSet, sigilWindow' sigil winSet) of
    (Just currentWin, Just sigilWin) -> W.focusWindow currentWin $
                                        mapWindows (swapWin currentWin sigilWin) winSet
    _ -> winSet
  where
    mapWindows f = W.mapWorkspace (mapWindows' f)
    mapWindows' f ws@W.Workspace{ W.stack = s } = ws { W.stack = mapWindows'' f <$> s }
    mapWindows'' f (W.Stack focused up down) = W.Stack (f focused) (map f up) (map f down)
    swapWin win1 win2 win
      | win == win1 = win2
      | win == win2 = win1
      | otherwise = win

------------------------------------------------------------------------------
-- Implementation of window sigil decorations
------------------------------------------------------------------------------

data SigilEngine widget a = SigilEngine deriving (Show, Read)
data SigilWidget = SigilWidget deriving (Show, Read)

instance DecorationWidget SigilWidget where
    type WidgetCommand SigilWidget = StandardCommand

    widgetCommand _ _ = def
    isShrinkable _ = False

instance (ClickHandler (GenericTheme SimpleStyle) SigilWidget)
  => DecorationEngine SigilEngine SigilWidget Window where
  type Theme SigilEngine = GenericTheme SimpleStyle
  type DecorationPaintingContext SigilEngine = XPaintingContext
  type DecorationEngineState SigilEngine = XMonadFont

  describeEngine _ = "SigilEngine"

  calcWidgetPlace _ dd widget = do
      str <- windowSigil (ddOrigWindow dd)
      let h = rect_height (ddDecoRect dd)
          font = ddEngineState dd
      withDisplay $ \dpy -> do
        width <- fi <$> textWidthXMF dpy (ddEngineState dd) str
        (a, d) <- textExtentsXMF font str
        let height = a + d
            y = fi $ (h - fi height) `div` 2
            y0 = y + fi a
            rect = Rectangle 0 y width (fi height)
        return $ WidgetPlace y0 rect

  paintWidget engine (dpy, pixmap, gc) place _ dd widget _ = do
      str <- windowSigil (ddOrigWindow dd)
      let style = ddStyle dd
          rect = wpRectangle place
          x = rect_x rect
          y = wpTextYPosition place
      printStringXMF dpy pixmap (ddEngineState dd) gc (sTextColor style) (sTextBgColor style) x y str

  paintDecoration = paintDecorationSimple

  initializeState _ _ theme = initXMF (themeFontName theme)
  releaseStateResources _ = releaseXMF

sigilDecoration :: (Shrinker shrinker)
                   => shrinker                -- ^ String shrinker, for example @shrinkText@
                   -> Theme SigilEngine SigilWidget  -- ^ Decoration theme (font, colors, widgets, etc)
                   -> l Window                -- ^ Layout to be decorated
                 -> ModifiedLayout (DecorationEx SigilEngine SigilWidget DefaultGeometry shrinker) l Window
sigilDecoration shrinker theme = decorationEx shrinker theme SigilEngine def

------------------------------------------------------------------------------
-- Implementation of DevLayout
------------------------------------------------------------------------------

data DevLayout a = DevLayout deriving (Read, Show)

instance LayoutClass DevLayout a where
    pureLayout DevLayout rect stack =
        zip windows rectangles
      where
        windows = W.integrate stack
        n = length windows
        rectangles = if n < 4
                     then splitHorizontally n rect
                     else let cols = splitHorizontally 4 rect
                          in (take 3 cols) ++ (splitVertically (n-3) (last cols))

    pureMessage _ _ = Nothing

------------------------------------------------------------------------------
-- Custom actions
------------------------------------------------------------------------------

repeatLastREPLCommand :: X ()
repeatLastREPLCommand = do
  ws <- W.integrate' `fmap` W.stack `fmap` W.workspace `fmap`  W.current `fmap` windowset `fmap` get
  whenJust (listToMaybe (reverse ws)) $ \replWindow -> do
    sendKeyWindow 0 xK_Up replWindow
    sendKeyWindow 0 xK_Return replWindow

------------------------------------------------------------------------------
-- Theme
------------------------------------------------------------------------------

myRed        = "#ff5370"
myBlack      = "#292d3e"
myYellow     = "#ffcb6b"
myDarkYellow = "#f78c6c"
myPurple     = "#c792ea"
myVisualGrey = "#3e4452"

-- myTheme :: Theme
myTheme = def { fontName            = "xft:mononoki-10"
              , activeColor         = myBlack
              , inactiveColor       = myBlack
              , urgentColor         = myBlack
              , activeTextColor     = myDarkYellow
              , inactiveTextColor   = myPurple
              , urgentTextColor     = myRed
              , decoWidth           = 25
              , decoHeight          = 16
              }

myThemeEx :: GenericTheme SimpleStyle SigilWidget
myThemeEx = (themeEx myTheme) { exWidgetsLeft = [SigilWidget]
                              }
myLayout = sigilDecoration shrinkText myThemeEx (DevLayout ||| Full)

main :: IO ()
main = do
  host <- fromMaybe "" <$> lookupEnv "HOST"
  xmonad $ withNavigation2DConfig def $ docks def
    { modMask            = mod4Mask  -- se Command/Super for mod
    , borderWidth        = 2
    , normalBorderColor  = myVisualGrey
    , focusedBorderColor = myDarkYellow
    , terminal           = "kitty"
    , layoutHook         = avoidStruts myLayout
    , logHook            = updatePointer (0.5, 0.5) (0, 0)
    , manageHook         = manageDocks <+> manageSpawn <> manageHook def
    , startupHook        =
        if host == "cnc.eraserhead.net"
        then do
          spawnOnOnce "1" "bCNC"
          spawnOnOnce "2" "firefox"
          spawnOnOnce "2" "kitty"
        else do
          spawnNOnOnce 5 "1" "kitty"
          spawnOnOnce "1" "firefox"
          spawnOnOnce "3" "slack"
          spawnOnOnce "3" "signal-desktop"
          spawnOnOnce "3" "discord"
    }
   `additionalKeysP`
    ([ ("<XF86AudioRaiseVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 10%+")
     , ("<XF86AudioLowerVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 10%-")
     , ("<XF86AudioPlay>", spawn "playerctl play-pause")

     , ("C-w h", windowGo L False)
     , ("C-w j", windowGo D False)
     , ("C-w k", windowGo U False)
     , ("C-w l", windowGo R False)
     , ("C-w M1-h", windowSwap L False)
     , ("C-w M1-j", windowSwap D False)
     , ("C-w M1-k", windowSwap U False)
     , ("C-w M1-l", windowSwap R False)
     , ("C-w ,", repeatLastREPLCommand)

     -- ("C-w .", pasteChar controlMask 'W') -- doesn't work
     ] ++
     [ ("C-w "++sigil, focusSigil sigil) | sigil <- sigils ] ++
     [ ("C-w M1-"++sigil, swapSigil sigil) | sigil <- sigils ])
