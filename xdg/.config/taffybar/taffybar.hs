module Main where

import Text.Printf
import System.Environment
import Data.Char
import Text.Read  -- readMaybe, readEither
import qualified Text.StringTemplate as ST
import qualified Data.ByteString.Lazy.Char8 as B

import System.Information.CPU
import System.Information.Memory
import System.Taffybar
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Weather
import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.PollingLabel
import System.Taffybar.WorkspaceHUD

import qualified Data.Time.Clock as Clock

import Control.Exception

import Data.Time.Format
import Data.Time.LocalTime
import Data.IORef

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.Layout.HBox

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

--------------------------------------------------
-- Helper
roundDbl :: Double -> Integer -> String
roundDbl value digits = printf ("%." ++ (show digits) ++ "f") value

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- getUrl :: String -> IO String
-- getUrl url = simpleHTTP (getRequest url) >>= getResponseBody

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

wrapLabel :: Label -> IO Widget
wrapLabel label = do
  box <- hBoxNew False 0
  boxPackStart box label PackNatural 0
  widgetShowAll box
  return $ toWidget box

--------------------------------------------------
-- CPU Monitor Related
cpuData :: IO [Int]
cpuData = cpuParser `fmap` B.readFile "/proc/stat"

cpuParser :: B.ByteString -> [Int]
cpuParser = map (read . B.unpack) . tail . B.words . head . B.lines

parseCpu :: IORef [Int] -> IO [Double]
parseCpu cref =
    do a <- readIORef cref
       b <- cpuData
       writeIORef cref b
       let dif = zipWith (-) b a
           tot = fromIntegral $ sum dif
           percent = map ((/ tot) . fromIntegral) dif
       return percent

getCpuTextColor :: Double -> String
getCpuTextColor value
  | value < 0.33 = "#00FF00"
  | value < 0.67 = "#FFD700"
  | otherwise = "#FF0000"

textCpuMonitorNew :: String
                  -> Double
                  -> IO Widget
textCpuMonitorNew fmt period = do
  cref <- newIORef []
  label <- pollingLabelNew fmt period (callback cref)
  widgetShowAll label
  return label
  where
    callback cref = do
      c <- parseCpu cref
      let totalLoad = sum $ take 3 c
      let load = roundDbl (totalLoad * 100) 0
      let color = getCpuTextColor totalLoad
      let template = ST.newSTMP fmt
      let template' = ST.setManyAttrib [("total", "<span fgcolor='" ++ color ++ "'>" ++ load ++ "</span>")] template
      return $ ST.render template'

-- Data.Time.Format doesn't allow customizing the timezone offset,
-- so I'm implementing the clock manually.
-- (I also don't need the pop-up calendar of the default clock widget)
myClock :: IO Widget
myClock = do
  z <- getCurrentTimeZone
  l <- pollingLabelNew "" 1.0 (callback z)
  ebox <- eventBoxNew
  containerAdd ebox l
  eventBoxSetVisibleWindow ebox False
  widgetShowAll ebox
  return (toWidget ebox)
  where
    callback z = do
      time <- showFormat z "%a %Y-%m-%d<span fgcolor='grey'>T</span><span fgcolor='yellow'>%H:%M</span>"
      os <- offset z
      return $ time ++ "<span fgcolor='grey'>" ++ os ++ "</span>"
    offset z = do
      (x:y:z:xs) <- showFormat z "%z"
      return (x:y:z:':':xs)
showFormat z f = return . formatTime defaultTimeLocale f . utcToZonedTime z =<< Clock.getCurrentTime

iconImageWidgetNew :: FilePath -> IO Widget
iconImageWidgetNew path = do
  box <- hBoxNew False 0
  icon <- imageNewFromFile path
  boxPackStart box icon PackNatural 0
  widgetShowAll box
  return $ toWidget box

-- cpuIcon = iconImageWidgetNew $ "/home/fthevissen/system/xmonad/icons/cpu.xbm"
-- memIcon = iconImageWidgetNew $ "/home/fthevissen/system/xmonad/icons/Memory.ico"

green :: String
green = "#B7F924"
red :: String
red = "#FF2A24"
blue :: String
blue = "#12D5D5"
yellow = "#f4bf75"

memToGB :: Int -> Double -> String
memToGB dec size = printf ("%." ++ show dec ++ "f") $ size / 1024

memLabel :: IO Widget
memLabel = do
    label <- pollingLabelNew "" 2 $ do
        let color pct
              | pct < 0.5 = green
              | pct < 0.9 = yellow
              | otherwise = red
        mi <- parseMeminfo
        return $ "mem: <span fgcolor='" ++ color (memoryUsed mi / memoryTotal mi) ++ "'>"
            ++ memToGB 2 (memoryUsed mi)
            ++ "</span> / "
            ++ memToGB 2 (memoryTotal mi)
            ++ " GB"
    widgetShowAll label
    return $ toWidget label

-- monitor placement (and more:)  , [264..]:
-- https://github.com/travitch/taffybar/blob/master/src/System/Taffybar.hs

-- advanced GTK stuff:
-- https://github.com/Shou/dotfiles/blob/master/.config/taffybar/taffybar-light.hs

-- custom freedesktop notifications formatter (and more?):
-- https://github.com/GregorySchwartz/dotfiles/blob/master/.config/taffybar/taffybar.hs

-- very nice setup:
-- https://github.com/simlu/xmonad/tree/master/config/.config/taffybar

-- createIcon :: IO Widget
-- createIcon path size = do
--     pixbuf <- pixbufNewFromFileAtScale path (-1) size True
--     icon <- imageNewFromPixBuf pixbuf
--     -- miscSetPadding icon 5 0
--     hbox <- hBoxNew False 1
--     boxPackStart hbox icon 5 0
--     widgetShowAll hbox
--     return $ toWidget hbox

-- cpuIcon envDirSystem = createIcon (envDirSystem ++ "/system/xmonad/icons/cpu2.png") 20
-- memIcon envDirSystem = createIcon (envDirSystem ++ "/system/xmonad/icons/Memory.ico") 20

cpuIcon :: IO Widget
cpuIcon = do
    system_dir <- getEnv("DIR_SYSTEM")
    pixbuf <- pixbufNewFromFileAtScale (system_dir ++ "/xmonad/icons/cpu2.png") (-1) 16 True
    icon <- imageNewFromPixbuf pixbuf
    -- miscSetPadding icon 5 0
    hbox <- hBoxNew False 1
    boxPackStart hbox icon PackNatural 0
    widgetShowAll hbox
    return $ toWidget hbox

memIcon :: IO Widget
memIcon = do
    system_dir <- getEnv("DIR_SYSTEM")
    pixbuf <- pixbufNewFromFileAtScale (system_dir ++ "/xmonad/icons/Memory.ico") (-1) 20 True
    icon <- imageNewFromPixbuf pixbuf
    -- miscSetPadding icon 5 0
    hbox <- hBoxNew False 1
    boxPackStart hbox icon PackNatural 0
    widgetShowAll hbox
    return $ toWidget hbox


main = do
  -- let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
  --                                 , graphLabel = Just "mem"
  --                                 }
  --     cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
  --                                                     , (1, 0, 1, 0.5)
  --                                                     ]
  --                                 , graphLabel = Just "cpu"
  --                                 }

    let clock = textClockNew Nothing "<span fgcolor='#90a959'>%a %_d.%m %H:%M    </span>" 1
        -- pager = taffyPagerHUDNew defaultPagerConfig defaultWorkspaceHUDConfig
        note = notifyAreaNew defaultNotificationConfig
        -- wea = weatherNew (defaultWeatherConfig "KMSN") 10
        -- mpris = mprisNew defaultMPRISConfig
        -- mem = pollingGraphNew memCfg 1 memCallback
        -- cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
        tray = systrayNew
        -- cpu = textCpuMonitorNew ("Cpu: $total$" ++ colorize "#586E75" "" "%") 1
        cpu = textCpuMonitorNew ("cpu: $total$%" ) 1

    envMonitorNumber <- getEnv "TAFFYBAR_MONITOR"
    let envMonitorNumberI = read envMonitorNumber :: Int

    envDirSystem <- getEnv "DIR_SYSTEM"


    defaultTaffybar defaultTaffybarConfig { startWidgets = [ note ]
                                          , endWidgets = [ clock, tray, cpu, memLabel]
                                          , barHeight = 20
                                          , widgetSpacing = 20
                                          , monitorNumber = envMonitorNumberI
                                          }
