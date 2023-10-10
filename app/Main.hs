module Main where

-- import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Time.Calendar
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
-- import Debug.Trace
import System.IO (hPutStrLn, stderr)
import Text.Printf
import UI.HSCurses.Curses (Key(..))

import qualified System.Exit
import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as C

data State = State
  { currentDay :: Day
  , startDay :: Maybe Day
  , firstMonth :: Month
  , window :: C.Window
  }


attrReverse :: C.Attr
attrReverse = C.setReverse C.attr0 True

reverseOn :: C.Window -> IO ()
reverseOn win = do
  (a, p) <- C.wAttrGet win
  C.wAttrSet win (C.setReverse a True, p)

reverseOff :: C.Window -> IO ()
reverseOff win = do
  (a, p) <- C.wAttrGet win
  C.wAttrSet win (C.setReverse a False, p)
  
paint :: State -> IO State
paint state@(State currentDay maybeStartDay firstMonth win) = do
  C.wclear win
  C.wMove win 0 0
  C.gotoTop
  
  (height, _width) <- C.scrSize
  let count = fromIntegral $ div (height-1) monthHeight

  let currentMonth = dayPeriod currentDay
  let firstMonth' =
        if currentMonth < firstMonth
        then currentMonth
        else if currentMonth >= (addMonths count firstMonth)
             then (addMonths (1-count) currentMonth)
             else firstMonth

  forM_ [firstMonth' .. (addMonths (count-1) firstMonth')] $ \i -> do
    month i
  return state{ firstMonth = firstMonth' }
  
  where

    monthHeight = 8
    makeLeft m y = take (monthHeight-1) (monthNames!!m : show y : repeat "")
    
    monthNames :: [String]
    monthNames = [ "", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]
    
    isSelected :: Day -> Bool
    isSelected d = maybe
      (d == currentDay)
      (\startDay -> (startDay <= d && d <= currentDay) || (currentDay <= d && d <= startDay))
      maybeStartDay

    dayOfMonth :: Day -> DayOfMonth
    dayOfMonth day = let (_, _, dm) = toGregorian day in dm

    weekOfYear :: Day -> WeekOfYear
    weekOfYear day = let (_, wy, _) = toWeekDate day in wy
    
    month :: Month -> IO ()
    month p = do
      let (y, m, _) = toGregorian (periodFirstDay p)
      C.wAddStr win $ "            Mo  Tu  We  Th  Fr  Sa  Su \n"
      weeks (makeLeft m y) (periodAllDays p)

    weeks :: [String] -> [Day] -> IO ()
    weeks [] _ = return ()
    weeks (left:lefts) [] = do
      C.wAddStr win $ printf "%-5s\n" left
      weeks lefts []
    weeks (left:lefts) allDays = do
      let first = head allDays
      let skip = dayOfWeekDiff (dayOfWeek first) Monday
      let (days, rest) = splitAt (7-skip) allDays
      C.wAddStr win $ printf "%-5s  %2d " left (weekOfYear first)
      C.wAddStr win $ take (1 + 4*skip) $ repeat ' '
      forM_ days $ \day -> do
        when (isSelected day) $ reverseOn win
        C.wAddStr win $ printf " %2d " (dayOfMonth day)
        reverseOff win
      C.wAddStr win "\n"
      weeks lefts rest



keyHandler :: Key -> State -> IO State
keyHandler c state@(State currentDay maybeStartDay _firstMonth _win) =
  case c of
    KeyChar 'h' -> return $ adjCurrentDay (-1)
    KeyLeft     -> return $ adjCurrentDay (-1)
    KeyChar 'j' -> return $ adjCurrentDay 7
    KeyDown     -> return $ adjCurrentDay 7
    KeyChar 'k' -> return $ adjCurrentDay (-7)
    KeyUp       -> return $ adjCurrentDay (-7)
    KeyChar 'l' -> return $ adjCurrentDay 1
    KeyRight    -> return $ adjCurrentDay 1

    KeyChar '0' -> return $ updateCurrentDay $ weekFirstDay Monday
    KeyHome     -> return $ updateCurrentDay $ weekFirstDay Monday
    KeyChar '$' -> return $ updateCurrentDay $ weekLastDay Monday
    KeyEnd      -> return $ updateCurrentDay $ weekLastDay Monday

    KeyChar '\x04' -> return $ adjCurrentMonth 1        -- C-d
    KeyNPage       -> return $ adjCurrentMonth 1        -- C-d
    KeyChar '\x15' -> return $ adjCurrentMonth (-1)     -- C-u
    KeyPPage       -> return $ adjCurrentMonth (-1)     -- C-u

    KeyChar 'H' -> do
      now <- getCurrentTime
      return $ updateCurrentDay $ const (utctDay now)
      
    KeyChar ' ' -> return $ state{ startDay = Just currentDay }
    KeyChar 'v' -> return $ state{ startDay = maybe (Just currentDay) (const Nothing) maybeStartDay }

    KeyChar 'y' -> accept
    KeyChar '\n' -> accept
    KeyChar 'q' -> cancel 
    KeyChar '\x1b' -> cancel

    _ -> do
      -- traceShowM ("key", c)
      -- threadDelay 1000000
      return state

  where

    updateCurrentDay f = state{ currentDay = f currentDay }
    adjCurrentDay n = updateCurrentDay $ addDays n
    
    adjCurrentMonth n =
      let
        (p, i) = periodFromDay currentDay
        p' = addMonths n p
        day' = periodToDay p' (min i (periodLength p'))
      in state{ currentDay = day' }

    accept =
      let
        startDay = maybe currentDay id maybeStartDay
        firstDay = min currentDay startDay
        lastDay = max currentDay startDay
      in do
        hPutStrLn stderr $ (show firstDay) ++ " " ++ (show lastDay)
        exitSuccess

    cancel = exitFailure *> return state


exitSuccess :: IO a
exitSuccess = do
  C.endWin
  System.Exit.exitSuccess

exitFailure :: IO a
exitFailure = do
  C.endWin
  System.Exit.exitFailure

loop :: State -> IO ()
loop state@(State _ _ _ win) = do
  state' <- paint state
  C.wRefresh win
  c <- C.getCh
  state'' <- keyHandler c state'
  loop state''
  
main :: IO ()
main = do
  now <- getCurrentTime
  let currentDay = utctDay now
  let firstMonth = dayPeriod currentDay

  _scr <- C.initScr
  C.cBreak True
  C.echo True
  C.intrFlush False
  C.keypad C.stdScr True

  win <- C.newWin 0 0 0 0
  let initState = State currentDay Nothing firstMonth win
  loop initState
  

