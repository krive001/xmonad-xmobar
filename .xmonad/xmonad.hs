-- Core
import XMonad
import Control.Monad (liftM2, filterM)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Tree
import System.Exit
import Graphics.X11.Xlib
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama
import System.IO (Handle, hPutStrLn)
import qualified System.IO
import System.IO
import Data.List
-- Prompts
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Man
import XMonad.Prompt.Window
-- Actions
import XMonad.Actions.MouseGestures
import XMonad.Actions.UpdatePointer
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.OnScreen
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.TreeSelect
-- Utils
import XMonad.Util.Run 
import XMonad.Util.Loggers
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.Minimize
-- Layouts
import XMonad.Layout.Minimize

--import XMonad.Layout.Fullscreen (fullscreenFull)


import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.DragPane
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.DecorationMadness
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.Mosaic
import XMonad.Layout.LayoutHints
import XMonad.Layout.Circle
import XMonad.Layout.NoBorders
import Data.Ratio ((%))
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane
import XMonad.Layout.OneBig
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Magnifier
import XMonad.Layout.TrackFloating
import XMonad.Layout.IndependentScreens
import qualified XMonad.Layout.ToggleLayouts as Tog

--import XMonad.Layout.CenteredMaster(centerMaster)


--InternetBrowser
myModMask                     = mod4Mask
myBrowser                     = "google-chrome-stable"
browserClass                  = "Chromium"
terminalClass                 = "URxvt"
myShell                       = "bash"
myTerm = "urxvt"
--CORE
mydefaults = def {
        terminal              = myTerm
        , normalBorderColor   = "#303030"
        , focusedBorderColor  = "cyan"
        --, focusFollowsMouse  = True
        --, mouseBindings      = myMouseBindings
        , workspaces          = myWorkspaces
        , keys                = myKeys
        , modMask             = myModMask
        , borderWidth         = 1
        --, startupHook         = setWMName "LG3D" <+> onScr 1 W.greedyView "1:W"
        , layoutHook          = myLayoutHook
        , startupHook 		  = myStartupHook
        , manageHook          = myManageHook
        --, focusFollowsMouse  = myFocusFollowsMouse
        , handleEventHook     = fullscreenEventHook <+> docksEventHook <+> minimizeEventHook
        } 
        --`additionalKeysP` myKeys

-- Autostart
myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"


--OTHER
onScr :: ScreenId -> (WorkspaceId -> WindowSet -> WindowSet) -> WorkspaceId -> X ()
onScr n f i = screenWorkspace n >>= \sn -> windows (f i . maybe id W.view sn)
--font = 
role = stringProperty "WM_WINDOW_ROLE"
--encodeCChar = map fromIntegral . B.unpack

--COLOR NON-ACTUAL WINDOW
--xmobarTitleColor = "green"

--COLOR ACTUAL WINDOW
--xmobarCurrentWorkspaceColor = "orange"

myTitleColor = "red" -- color of window title
myTitleLength = 80 -- truncate window title to this length
myCurrentWSColor = "orange" -- color of active workspace
myVisibleWSColor = "red" -- color of inactive workspace
myUrgentWSColor = "red" -- color of workspace with 'urgent' window
myHiddenNoWindowsWSColor = "green"


--xmobarTitleFont = "xft:fontAwesomeClass:size=10:antialias=true"

--SCRATCHPAD
scratchPad = scratchpadSpawnActionTerminal "urxvtc -name scratchpad"
mynameScratchpads = [ NS "mocp" "urxvtc -name mocp -e mocp" (appName =? "MOCP") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                    , NS "htop" "Htop" (appName =? "htop") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                    , NS "lxappearance" "lxappearance" (appName =? "lxappearance") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                    , NS "xfce4-appfinder" "xfce4-appfinder" (appName =? "xfce4-appfinder") (customFloating $ W.RationalRect 0.001 0.03 0.3 0.6)
                    , NS "arandr" "arandr" (appName =? "arandr") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                    , NS "nano" "urxvtc -name nano -e nano" (appName =? "nano") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                    , NS "qbittorrent" "qbittorrent" (appName =? "qbittorrent") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                    , NS "gpicview" "Gpicview" (appName =? "gpicview") (customFloating $ W.RationalRect 0.2
                        0.2 0.6 0.6)
                    , NS "RosaImageWriter" "RosaImageWriter" (appName =? "RosaImageWriter") (customFloating $ W.RationalRect 0.3 0.3 0.4 0.4)
                    , NS "Nmtui" "Nmtui" (appName =? "nmtui") (customFloating $ W.RationalRect 0.3 0.3 0.4 0.4)
                    , NS "pavucontrol" "pavucontrol" (appName =? "pavucontrol") (customFloating $ W.RationalRect  0.2 0.2 0.6 0.6)
                    , NS "galculator" "galculator" (appName =? "galculator") (customFloating $ W.RationalRect  0.4 0.3 0.1 0.3)
                    , NS "gmrun" "gmrun" (className =? "Gmrun") (customFloating $ W.RationalRect  0.012 0.001 0.26 0.085)
                    ]

manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
                    where
                    h = 0.333   -- terminal height
                    w = 1       -- terminal width
                    t = 1 - h   -- distance from top edge
                    l = 1 - w   -- distance from left edge

--TILING
myLayoutHook = spacingRaw True (Border 0 4 4 4) True (Border 4 4 4 4) True
               $ avoidStruts
              -- $ mkToggle (single MIRROR) (tiled ||| Full) 
               $ toggleLayouts (noBorders Full)
               $ smartBorders
--               $ minimize
               $ tiled ||| mosaic 2 [3,2] ||| Full ||| magnifier (Tall 1 0.3 0.5)  
                    where
                    tiled   = Tall nmaster delta ratio
                    nmaster = 1
                    delta   = 0.01
                    ratio   = 0.5
                


--WORKSPACES
xmobarEscape = concatMap doubleLts
    where doubleLts '<' = "<<"
          doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ [" \61612","\61508","\61728","\61574","\61502","\61501","\61705","\61564","\62150", "\61832"]
    where
               clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" | (i,ws) <- zip [1, 2, 3, 4, 5, 6, 7, 8, 9, 0] l, let n = i ]

--WINDOWS RULES
myManageHook :: ManageHook
myManageHook = composeAll . concat $
        [ [className =? c --> doShift (myWorkspaces !! 0) <+> viewShift (myWorkspaces !! 0)        | c <- myWeb]
        , [className =? c --> doShift (myWorkspaces !! 1) <+> viewShift (myWorkspaces !! 1)        | c <- myDev]
        , [className =? c --> doShift (myWorkspaces !! 2) <+> viewShift (myWorkspaces !! 2)        | c <- myShell]
        , [className =? c --> doShift (myWorkspaces !! 3) <+> viewShift (myWorkspaces !! 3)        | c <- myVMs]
        , [className =? c --> doShift (myWorkspaces !! 4) <+> viewShift (myWorkspaces !! 4)        | c <- myMedia]
        , [className =? c --> doShift (myWorkspaces !! 5) <+> viewShift (myWorkspaces !! 5)        | c <- mySocial]
        , [className =? c --> doShift (myWorkspaces !! 6) <+> viewShift (myWorkspaces !! 6)        | c <- myGames]
        , [className =? c --> doShift (myWorkspaces !! 7) <+> viewShift (myWorkspaces !! 7)        | c <- myExt]
        , [className =? c --> doShift (myWorkspaces !! 8) <+> viewShift (myWorkspaces !! 8)        | c <- myMusic]
        , [className =? c --> doCenterFloat                                                        | c <- myFloatC]
        , [appName   =? a --> doCenterFloat                                                        | a <- myFloatA]
        , [title     =? t --> doCenterFloat                                                        | t <- myFloatT]
        , [role      =? r --> doCenterFloat                                                        | r <- myFloatR]

        , [className =? "Gis-weather.py"  --> doIgnore]
        , [manageDocks]
        , [isFullscreen   --> doFullFloat]
        , [isDialog       --> doCenterFloat]
        , [transience']
        ]
        where
        myWeb        = ["Google-chrome", "Chromium","Thunderbird"]
        myDev        = ["Subl3"]
        myShell     = ["QElectroTech","MyPaint","Xsane","libreoffice-startcenter","libreoffice-impress","libreoffice-writer", "libreoffice-calc","libreoffice-draw"]
        myVMs        = ["discord","VirtualBox","VBoxSDL","Virt-manager"]
        myMedia      = ["mpv","Shotwell","Gpicview","Totem"]
        mySocial     = ["ViberPC","TelegramDesktop"]
        myGames      = ["Crossover"]
        myExt        = ["TeamViewer"]
        myMusic      = ["Spotify"]

--FLOATING
        myFloatC     = ["Oblogout", "GParted","Bleachbit","Nm-connection-editor","System-config-printer.py"]
        myFloatA     = ["xarchiver","Update","teamviewer","engrampa"]
        myFloatT     = ["Software Update"]
        myFloatR     = ["task_dialog","messages","pop-up","^conversation$","About"]
        viewShift    = doF . liftM2 (.) W.greedyView W.shift

--KEYBINDINGS

--mkKeymap :: XConfig l -> [(String, X ())] -> Map (KeyMask, KeySym) (X ())

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- SUPER + FUNCTION KEYS

  [ 
  ((modMask, xK_a), spawn  "pamac-manager" )
  , ((modMask, xK_c), spawn  "discord" )
  , ((modMask, xK_d), spawn  "rofi -show run" )
  , ((modMask, xK_e), spawn "subl3")

  --, ((modMask, xK_f), sendMessage $ Toggle NBFULL)
  , ((modMask, xK_h), spawn $ "urxvt 'htop task manager' -e htop" )
  , ((modMask, xK_m), spawn $ "pragha" )
  , ((modMask, xK_r), spawn $ "rofi-theme-selector" )
  , ((modMask, xK_t), spawn $ "urxvt" )
  , ((modMask, xK_v), spawn $ "pavucontrol" )
  , ((modMask, xK_w), spawn myBrowser )
  , ((modMask, xK_q), kill )
  , ((modMask, xK_y), spawn $ "polybar-msg cmd toggle" )
  , ((modMask, xK_x), spawn $ "oblogout" )
  , ((modMask, xK_Escape), spawn $ "xkill" )
  , ((modMask, xK_Return), spawn $ "urxvt" )
  , ((modMask, xK_F1), spawn $ "vivaldi-stable" )
  , ((modMask, xK_F2), spawn $ "atom" )
  , ((modMask, xK_F3), spawn $ "inkscape" )
  , ((modMask, xK_F4), spawn $ "gimp" )
  , ((modMask, xK_F5), spawn $ "meld" )
  , ((modMask, xK_F6), spawn $ "vlc --video-on-top" )
  , ((modMask, xK_F7), spawn $ "virtualbox" )
  , ((modMask, xK_F8), spawn $ "thunar" )
  , ((modMask, xK_F9), spawn $ "evolution" )
  , ((modMask, xK_F10), spawn $ "spotify" )
  , ((modMask, xK_F11), spawn $ "rofi -show run -fullscreen" )
  , ((modMask, xK_F12), spawn $ "rofi -show run" )
  , ((modMask, xK_Up), spawn "amixer -D pulse set Master 5%+ unmute")
  , ((modMask, xK_Down), spawn "amixer -D pulse set Master 5%- unmute")
  , ((modMask, xK_Right), nextWS)
  , ((modMask, xK_Left), prevWS)

  -- SUPER + SHIFT KEYS

  , ((modMask .|. shiftMask , xK_Return ), spawn $ "thunar")
  , ((modMask .|. shiftMask , xK_d ), spawn $ "dmenu_run -i -nb '#191919' -nf '#fea63c' -sb '#fea63c' -sf '#191919' -fn 'NotoMonoRegular:bold:pixelsize=14'")
  , ((modMask .|. shiftMask , xK_r ), spawn  "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask , xK_q ), spawn $ "pkill xmonad")
  , ((modMask .|. shiftMask , xK_x ), io (exitWith ExitSuccess))

  -- CONTROL + ALT KEYS

  , ((controlMask .|. mod1Mask , xK_a ), spawn $ "xfce4-appfinder")
  , ((controlMask .|. mod1Mask , xK_b ), spawn $ "thunar")
  , ((controlMask .|. mod1Mask , xK_c ), spawn $ "catfish")
  , ((controlMask .|. mod1Mask , xK_e ), spawn $ "evolution")
  , ((controlMask .|. mod1Mask , xK_f ), spawn $ "firefox")
  , ((controlMask .|. mod1Mask , xK_g ), spawn $ "chromium -no-default-browser-check")
  , ((controlMask .|. mod1Mask , xK_i ), spawn $ "nitrogen")
  , ((controlMask .|. mod1Mask , xK_k ), spawn $ "slimlock")
  , ((controlMask .|. mod1Mask , xK_m ), spawn $ "xfce4-settings-manager")
  , ((controlMask .|. mod1Mask , xK_o ), spawn $ "$HOME/.xmonad/scripts/compton-toggle.sh")
  , ((controlMask .|. mod1Mask , xK_p ), spawn $ "pamac-manager")
  , ((controlMask .|. mod1Mask , xK_r ), spawn $ "rofi-theme-selector")
  , ((controlMask .|. mod1Mask , xK_s ), spawn $ "spotify")
  , ((controlMask .|. mod1Mask , xK_t ), spawn $ "urxvt")
  , ((controlMask .|. mod1Mask , xK_u ), spawn $ "pavucontrol")
  , ((controlMask .|. mod1Mask , xK_v ), spawn $ "vivaldi-stable")
  , ((controlMask .|. mod1Mask , xK_w ), spawn $ "atom")
  , ((controlMask .|. mod1Mask , xK_Return ), spawn $ "urxvt")

  -- ALT + ... KEYS

  , ((mod1Mask, xK_f), spawn $ "variety -f" )
  , ((mod1Mask, xK_n), spawn $ "variety -n" )
  , ((mod1Mask, xK_p), spawn $ "variety -p" )
  , ((mod1Mask, xK_r), spawn $ "xmonad --restart" )
  , ((mod1Mask, xK_t), spawn $ "variety -t" )
  , ((mod1Mask, xK_Up), spawn $ "variety --pause" )
  , ((mod1Mask, xK_Down), spawn $ "variety --resume" )
  , ((mod1Mask, xK_Left), spawn $ "variety -p" )
  , ((mod1Mask, xK_Right), spawn $ "variety -n" )
  , ((mod1Mask, xK_F2), spawn $ "gmrun" )
  , ((mod1Mask, xK_F3), spawn $ "xfce4-appfinder" )

  --VARIETY KEYS WITH PYWAL

  , ((mod1Mask .|. shiftMask , xK_f ), spawn $ "variety -f && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_n ), spawn $ "variety -n && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_p ), spawn $ "variety -p && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_t ), spawn $ "variety -t && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_u ), spawn $ "wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")

  --CONTROL + SHIFT KEYS

  , ((controlMask .|. shiftMask , xK_Escape ), spawn $ "xfce4-taskmanager")

  --SCREENSHOTS

  , ((0, xK_Print), spawn $ "scrot 'ArcoLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")
  , ((controlMask, xK_Print), spawn $ "xfce4-screenshooter" )
  , ((controlMask .|. shiftMask , xK_Print ), spawn $ "gnome-screenshot -i")


  --MULTIMEDIA KEYS

  -- Mute volume
  , ((0, xF86XK_AudioMute), spawn $ "amixer -q set Master toggle")

  -- Decrease volume
  , ((0, xF86XK_AudioLowerVolume), spawn $ "amixer -q set Master 10%-")

  -- Increase volume
  , ((0, xF86XK_AudioRaiseVolume), spawn $ "amixer -q set Master 10%+")

  -- Increase brightness
  , ((0, xF86XK_MonBrightnessUp),  spawn $ "xbacklight -inc 10")

  -- Decrease brightness
  , ((0, xF86XK_MonBrightnessDown), spawn $ "xbacklight -dec 10")

  , ((0, xF86XK_AudioPlay), spawn $ "mpc toggle")
  , ((0, xF86XK_AudioNext), spawn $ "mpc next")
  , ((0, xF86XK_AudioPrev), spawn $ "mpc prev")
  , ((0, xF86XK_AudioStop), spawn $ "mpc stop")

--  , ((0, xF86XK_AudioPlay), spawn $ "playerctl play-pause")
--  , ((0, xF86XK_AudioNext), spawn $ "playerctl next")
--  , ((0, xF86XK_AudioPrev), spawn $ "playerctl previous")
--  , ((0, xF86XK_AudioStop), spawn $ "playerctl stop")


  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space), sendMessage NextLayout)

  --Focus selected desktop
  , ((mod1Mask, xK_Tab), nextWS)

  --Focus selected desktop
  , ((modMask, xK_Tab), nextWS)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Move focus to the next window.
  , ((controlMask .|. mod1Mask , xK_Left ), windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j), windows W.focusDown)

  -- Move focus to the previous window.
  , ((controlMask .|. mod1Mask , xK_Right ), windows W.focusUp  )

  -- Move focus to the previous window.
  , ((modMask, xK_k), windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask .|. shiftMask, xK_m), windows W.focusMaster  )

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )

  -- Swap the focused window with the next window.
  , ((controlMask .|. modMask, xK_Down), windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )

  -- Swap the focused window with the previous window.
  , ((controlMask .|. modMask, xK_Up), windows W.swapUp  )

  -- Shrink the master area.
  , ((controlMask .|. shiftMask , xK_h), sendMessage Shrink)

  -- Expand the master area.
  , ((controlMask .|. shiftMask , xK_l), sendMessage Expand)

  -- Push window back into tiling.
  , ((controlMask .|. shiftMask , xK_t), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Left), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Right), sendMessage (IncMasterN (-1)))

  ]
  ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++
    [((m .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    
--myFocusFollowsMouse :: Bool
--myFocusFollowsMouse = True


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

--XMOBAR
main = do
        xmproc <- spawnPipe "xmobar"
        xmonad $ ewmh $ mydefaults {
        logHook =  dynamicLogWithPP $ def {
        ppOutput = System.IO.hPutStrLn xmproc
        , ppTitle = xmobarColor myTitleColor "" . ( \ str -> "")
        , ppCurrent = xmobarColor myCurrentWSColor "" . wrap """"
        , ppVisible = xmobarColor myVisibleWSColor "" . wrap """"
        --, ppVisibleNoWindows = wrap """"
        , ppHidden = wrap """"
        , ppHiddenNoWindows = xmobarColor myHiddenNoWindowsWSColor ""
        , ppUrgent = xmobarColor myUrgentWSColor ""
        , ppSep = "  "
        , ppWsSep = "  "
        , ppLayout = (\ x -> case x of
           "Spacing Tall"                 -> "<fn=1>Tall</fn>"
           "Spacing Full"                 -> "<fn=1>Full</fn>"
           "Spacing Mosaic"               -> "<fn=1>Mosaic</fn>"
           "Spacing Magnifier Tall"       -> "<fn=1>Magnifier Tall</fn>"           
           _                                         -> x )
 }
}
