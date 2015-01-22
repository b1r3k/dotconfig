{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}

import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog

import Control.OldException(catchDyn,try)
import Control.Concurrent

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize

import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Tabbed

import XMonad.Actions.CycleWS

-- workaround for Java+xmonad issues
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus

import XMonad.Util.WindowProperties
import Control.Monad
import Data.Ratio

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import Data.Monoid (All (All))

import XMonad.Hooks.ManageHelpers (doCenterFloat,doFullFloat, isFullscreen, isDialog)

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    terminal1 <- spawn myTerminal
    terminal2 <- spawn myTerminal
    terminal3 <- spawn myTerminal

    
    xmonad $ gnomeConfig {
        modMask = myModMask,
        terminal = myTerminal,
        workspaces = myWorkspaces,
        layoutHook = myLayoutHook,
        keys = keys gnomeConfig <+> myKeys,
        manageHook = manageHook gnomeConfig <+> manageHook' <+> imManageHooks <+> manageDocks,
        logHook = dynamicLogWithPP (prettyPrinter dbus),
        handleEventHook = handleEventHook gnomeConfig <+> evHook,
        -- workaround for Java+xmonad issues
        startupHook = setWMName "LG3D" 
    }

myTerminal = "terminator"


myWorkspaces = ["terms", "web", "editor", "pdf", "web-dev"] ++ (miscs 2) ++ ["general","fullscreen", "im"]
                where miscs = map (("misc" ++) . show) . (flip take) [1..]

-- Mod4 is the Super / Windows key
myModMask = mod4Mask
altMask = mod1Mask
--shiftMask = 

myKeys conf = M.fromList $
    [((0, xK_Print), spawn "sleep 0.2; scrot '%Y-%m-%d-%H-%M-%S_$wx$h.png' -e 'mv $f ~/screenshots/'") -- print --> screenshot
    ,((controlMask, xK_Print), spawn "sleep 0.2; scrot -s '%Y-%m-%d-%H-%M-%S_$wx$h.png' -e 'mv $f ~/screenshots/'") -- print --> screenshot selection
    ,((myModMask .|. altMask, xK_l), spawn "gnome-screensaver-command -l") -- print --> screenshot selection
    ] ++
    -- Alt+F1..F10 switches to workspace
    -- (Alt is in a nicer location for the thumb than the Windows key,
    -- and 1..9 keys are already in use by Firefox, irssi, ...)
    [ ((altMask, k), windows $ W.greedyView i)
        | (i, k) <- zip myWorkspaces workspaceKeys
    ] ++
    -- mod+F1..F10 moves window to workspace and switches to that workspace
    [ ((myModMask, k), (windows $ W.shift i) >> (windows $ W.greedyView i))
        | (i, k) <- zip myWorkspaces workspaceKeys
    ] ++
    [
    ((myModMask, xK_m     ), withFocused $ sendMessage . maximizeRestore)
    ,((myModMask .|. shiftMask, xK_m     ), withFocused minimizeWindow)
    ,((myModMask, xK_Right), nextWS)
    ,((myModMask, xK_Left), prevWS)
    ,((myModMask, xK_Down), toggleWS)
    ]
    where workspaceKeys = [xK_F1 .. xK_F10]

-- layouts
basicLayout = Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2
tallLayout = named "tall" $ avoidStruts $ noBorders $ basicLayout
wideLayout = named "wide" $ avoidStruts $ noBorders $ Mirror basicLayout
singleLayout = named "single" $ avoidStruts $ noBorders Full
fullscreenLayout = named "fullscreen" $ noBorders Full
--gimpLayout = named "gimp-fullscreen" $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") $ noBorders Full
gimpLayout = withIM (11/64) (Role "gimp-toolbox") $ ResizableTall 2 (1/118) (11/20) [1] ||| Full

imLayout = avoidStruts $ reflectHoriz $ withIMs ratio rosters chatLayout where
    chatLayout      = Grid
    ratio           = 1%6
    rosters         = [skypeRoster, pidginRoster]
    pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
    skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm")) `And` (Not (Role "ConversationsWindow")) `And` (Not (Role "CallWindow"))
 
myLayoutHook = fullscreen $ im $ pdf $ gimp $ normal where
    normal     = tallLayout ||| wideLayout ||| singleLayout
    fullscreen = onWorkspace "fullscreen" fullscreenLayout
    im         = onWorkspace "im" imLayout
    pdf        = onWorkspace "pdf" (noBorders $ smartBorders $ simpleTabbedAlways)
    gimp       = onWorkspace "general" gimpLayout


-- Helper functions to fullscreen the window
fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ W.float w r
    where r = W.RationalRect 0 0 1 1
tileWin w = windows $ W.sink w

evHook :: Event -> X All
evHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  isFull <- runQuery isFullscreen win

  -- Constants for the _NET_WM_STATE protocol
  let remove = 0
      add = 1
      toggle = 2

      -- The ATOM property type for changeProperty
      ptype = 4

      action = head dat

  when (typ == state && (fromIntegral fullsc) `elem` tail dat) $ do
    when (action == add || (action == toggle && not isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace [fromIntegral fullsc]
         fullFloat win
    when (head dat == remove || (action == toggle && isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace []
         tileWin win

--  it shouldn't be necessary for xmonad to do anything more with this event
  return $ All False

evHook _ = return $ All True

-- Hooks {{{
--IM ManageHooks
imManageHooks = composeAll [isIM --> moveToIM] where
    isIM     = foldr1 (<||>) [isPidgin, isSkype]
    isPidgin = className =? "Pidgin"
    isSkype  = className =? "Skype"
    moveToIM = doF $ W.shift "im"

-- ManageHook {{{
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doIgnore            |   c   <- myClassIgnores] -- ignore desktop
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [className    =? f            --> doFloat             |   f   <- myBasicFloats ] -- float my floats
    , [className    =? c            --> doShift "pdf"       |   c   <- myReaders ] -- float my floats
    , [className    =? c            --> doShift "editor"       |   c   <- myDevs ] -- float my floats
    , [className    =? c            --> doShift "general"       |   c   <- myPlayers ] -- float my floats
    , [className    =? c            --> doShift "web"       |   c   <- myWebs ] -- float my floats
    , [className    =? c            --> doShift "web-dev"       |   c   <- myWebDevs ] -- float my floats
    --, [className    =? c            --> doShift "general"       |   c   <- myGraphics ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> doFullFloat                           ]
    , [isDialog                     --> doFloat                           ]
    ]) 

    where

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        --myBasicFloats = ["Unity-2d-shell", "jetbrains-pycharm"]
        myBasicFloats = ["Unity-2d-shell" ]
        myFloats  = ["MPlayer","Zenity","VirtualBox","Xmessage","Save As...","XFontSel","Downloads","Nm-connection-editor"]
        myWebs    = ["Navigator","Shiretoko","Firefox","Uzbl","uzbl","Uzbl-core","uzbl-core","Chromium","Shredder","Mail"]
        myWebDevs = [ "Google-chrome" ]
        myDevs    = ["Eclipse","eclipse","Netbeans","Gvim", "Sublime_text"]
        myClassIgnores   = ["Docky", "Unity-2d-panel", "Cairo-dock", "keepass2"]
        myReaders = ["Evince"]
        myPlayers = ["Rhythmbox"]
        myGraphics = [ "gimp-2.6" ]
        myLibreOffice = ["Soffice"]



        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer", "Conky"]

        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat
-- }}}

-- Boring DBus stuff

-- This retry is really awkward, but sometimes DBus won't let us get our
-- name unless we retry a couple times.

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

try_ :: MonadIO m => IO a -> m ()
try_ action = liftIO $ try action >> return ()

-- modified version of XMonad.Layout.IM --
 
-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)
 
instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"
 
-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props
 
-- | IM layout modifier applied to the Grid layout
gridIMs :: Rational -> [Property] -> ModifiedLayout AddRosters Grid a
gridIMs ratio props = withIMs ratio props Grid
 
hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w
 
-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> W.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack = W.stack wksp
    let ws = W.integrate' $ stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= W.filter (`notElem` rosters)
    (a,b) <- runLayout (wksp {W.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)
