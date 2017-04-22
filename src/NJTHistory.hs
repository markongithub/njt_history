module NJTHistory where

  import Control.Monad (liftM)
  import Data.Char (isDigit)
  import Data.Time.Clock (addUTCTime, UTCTime(..))
  import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
  import Data.Time.LocalTime (LocalTime(..), localTimeToUTC, TimeOfDay(..), TimeZone(..), utcToLocalTime)
  import Data.Time.LocalTime.TimeZone.Olson (getTimeZoneSeriesFromOlsonFile) 
  import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(..), localTimeToUTC', utcToLocalTime')
  import Text.HTML.TagSoup ((~/=), (~==), fromTagText, isTagText, parseTags, sections, Tag)
  import Data.List.Split (splitOn)
  import Data.List (sort)

  tzFile = "/usr/share/zoneinfo/US/Eastern"
  tzFileIO = getTimeZoneSeriesFromOlsonFile tzFile

  readStatusFiles :: [String] -> IO StatusPage
  readStatusFiles files = do
    tz <- getTimeZoneSeriesFromOlsonFile tzFile
    statuses <- sequence $ map (readStatusFile tz) (sort files)
    return $ foldl mergeStatusPage (head statuses) statuses

  data HTMLFile = HTMLFile String String

  readStatusFileDumb :: String -> IO HTMLFile
  readStatusFileDumb filename = do
    html <- readFile filename
    return $ HTMLFile filename html

  convertHTMLFile :: HTMLFile -> TimeZoneSeries -> StatusPage
  convertHTMLFile (HTMLFile filename html) tz =
    let timestamp = timestampFromFilename filename
     in parseStatusPage timestamp tz html

  readStatusFile :: TimeZoneSeries -> String -> IO StatusPage
  readStatusFile tz filename = do
    html <- readFile filename
    let timestamp = timestampFromFilename filename
    return $ parseStatusPage timestamp tz html

  timestampFromFilename :: String -> UTCTime
  timestampFromFilename filename =
    let base = last $ splitOn "/" filename
        afterLastHyphen = tail $ dropWhile (/= '-') $ tail $ dropWhile (/= '-') base
        timestampString = takeWhile (/= '.') afterLastHyphen
        timestamp = (read timestampString :: Int)
     in posixSecondsToUTCTime $ fromIntegral timestamp

  parseTrainNumber :: [Tag String] -> String
  -- first <b> tag of the page is the train number
  -- well it used to be. now maybe we can say it's the first <b> tag of the
  -- page after we see "TRAIN # "
  parseTrainNumber tags = let
    isTrainIntro tag = isTagText tag && (fromTagText tag == "TRAIN # ")
    candidates = dropWhile (not . isTrainIntro) tags
    in fromTagText $ head $ filter isTagText $ dropWhile (~/= "<b>") candidates

  splitWhen :: (Tag a -> Bool) -> [Tag a]  -> [[Tag a]]
  splitWhen p xs = map (takeWhile (not . p) . drop 1) (sections p xs)

  stopCells :: [Tag String] -> [[Tag String]]
  stopCells tags =
    let stopsTable = dropWhile (~/= "<table ID=table_stops>") tags
     in splitWhen (~== "<td>") stopsTable

  data TrainStatus = Scheduled | Departed (Maybe UTCTime) (Maybe UTCTime) (Maybe UTCTime)| OnTime | Expected UTCTime
       deriving (Eq, Show)

  parseTrainStatus :: UTCTime -> TimeZoneSeries -> String -> TrainStatus
  parseTrainStatus _ _ "DEPARTED" = Departed Nothing Nothing Nothing
  parseTrainStatus _ _ "DEPARTED Discharge Only" = Departed Nothing Nothing Nothing
  parseTrainStatus _ _ "ON TIME" = Scheduled
  parseTrainStatus _ _ "BOARDING" = Scheduled
  parseTrainStatus _ _ "ALL ABOARD" = Scheduled
  parseTrainStatus _ _ "STAND BY" = Scheduled
  parseTrainStatus hint zone status | take 5 status == "at   " = parseExpected hint zone status
  parseTrainStatus _ _ status = error ("I did not expect " ++ status)

  parseExpected :: UTCTime -> TimeZoneSeries -> String -> TrainStatus
  parseExpected hint zone status = Expected guess
    where time = drop 5 status
          hourStr = takeWhile (/= ':') time
          minuteStr = takeWhile isDigit $ tail $ dropWhile (/= ':') time
          hour = read hourStr :: Int
          minute = read minuteStr :: Int
          guess = guessUTCTime hint zone hour minute

  parseStopCell :: [Tag String] -> (String, String)
  parseStopCell tags =
    -- Some of the arrival statuses are italicized, which means we have two
    -- different TagTexts. So we just concatenate them.
    -- It should look like either "Dover\160\160<i>DEPARTED</i>" or
    -- "Dover\160\160at   7:49"
    let stopCellText = concat $ map fromTagText $ filter isTagText tags
    -- Now it looks like either "Dover\160\160DEPARTED" or
    -- "Dover\160\160at   1:41 "
        stationName = takeWhile (/= '\160') stopCellText
        statusString = dropWhile (== '\160') $ dropWhile (/= '\160') stopCellText
        in (stationName, statusString)

  parseStopCells :: [Tag String] -> [(String, String)]
  parseStopCells html = map parseStopCell $ init $ stopCells html

  parseStatusPage :: UTCTime -> TimeZoneSeries -> String -> StatusPage
  parseStatusPage hint zone html = StatusPage {
        trainName = parseTrainNumber tags
      , stops     = map fixTuple $ parseStopCells tags
      , timestamp = hint
      } where fixTuple (station, status) = (station, parseTrainStatus hint zone status)
              tags = parseTags html

  data StatusPage = StatusPage
      { trainName           :: String -- probably a number but whatever
      , stops          :: [(String, TrainStatus)]
      , timestamp          :: UTCTime
      } deriving (Eq, Show)


  guessUTCTime :: UTCTime -> TimeZoneSeries -> Int -> Int -> UTCTime
  -- If we have a 12-hour timestamp like "7:49" we need to know if it's AM or PM  -- and we need to know the day. If we know the time the page was retrieved we
  -- can guess it from that. But that will be wrong if we retrieved the page at
  -- 11:50 PM and saw something like "12:05".
  guessUTCTime hint zone hour minute =
    let local = utcToLocalTime' zone hint
        day = localDay local
        minTime = addUTCTime (-30 * 60) hint
        candidateHours = if hour > 11 then [hour-12, hour] else [hour, hour+12]
        makeCandidateTime cHour = localTimeToUTC' zone $ LocalTime day (TimeOfDay cHour minute 0)
        candidateTimes = map makeCandidateTime candidateHours
        isAcceptable t = t > minTime
     in head $ filter isAcceptable candidateTimes
        
  updateTrainStatus :: TrainStatus -> UTCTime -> TrainStatus -> UTCTime -> TrainStatus
  updateTrainStatus _ oldT _ newT | oldT > newT = error "new time is older"
  -- DN -> DN = DN
  updateTrainStatus (Departed Nothing a b) _ (Departed Nothing _ _) _ = Departed Nothing a b
  -- DN -> anything else = unexpected
  updateTrainStatus (Departed Nothing _ _) _ x _ = error ("DN -> " ++ (show x))
  -- Dt -> DN = Dt
  updateTrainStatus (Departed (Just t) a b) _ (Departed Nothing _ _) _ = Departed (Just t) a b
  -- Dt -> anything else = unexpected
  updateTrainStatus (Departed (Just t) _ _) _ x _ = error ("Dt -> " ++ (show x))
  -- Et -> DN = Dt (assume the expected thing was right?)
  updateTrainStatus (Expected et) oldT (Departed Nothing _ _) newT = Departed (Just oldT) (Just newT) (Just et)
  -- Et -> Et = the newer Et
  updateTrainStatus (Expected _) _ (Expected t) _ = Expected t
  -- Et -> Scheduled seems to happen at NYP sometimes. Weird.
  updateTrainStatus (Expected _) _ Scheduled _ = Scheduled
  -- Et -> anything else = unexpected
  updateTrainStatus (Expected _) _ x _ = error ("Et -> " ++ (show x))
  -- Other -> DN = Dt with time from last one?
  updateTrainStatus _ oldT (Departed Nothing _ _) newT = Departed (Just oldT) (Just newT) Nothing
  -- Other -> Et = Et
  updateTrainStatus _ _ (Expected t) _ = Expected t
  -- Other -> Other = newer
  updateTrainStatus _ _ x _ = x

  mergeStopPair :: UTCTime -> UTCTime -> (String, TrainStatus) -> (String, TrainStatus) -> (String, TrainStatus)
  mergeStopPair t1 t2 (n1, s1) (n2, s2)
    | n1 /= n2 = error ("Mismatched stops: " ++ show [n1, n2])
    | otherwise = (n1, updateTrainStatus s1 t1 s2 t2)

  mergeStatusPage :: StatusPage -> StatusPage -> StatusPage
  mergeStatusPage p1 p2
    | trainName p1 /= trainName p2 = error ("Trains do not match: " ++ show [p1, p2])
    | otherwise = StatusPage (trainName p1) (zipWith (mergeStopPair (timestamp p1) (timestamp p2)) (stops p1) (stops p2)) (timestamp p2)
