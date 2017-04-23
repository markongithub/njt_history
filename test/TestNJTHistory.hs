module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)
import NJTHistory
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (addUTCTime, secondsToDiffTime, UTCTime(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

testDay = fromGregorian 2015 10 22 
testDiff = secondsToDiffTime (17 * 3600 + 41 * 60)
expectedDiff = secondsToDiffTime (17 * 3600 + 52 * 60)
testUTC = UTCTime testDay testDiff
sampleTime = posixSecondsToUTCTime 1445708047
emptyStatusPage = StatusPage "wut" [("wut", Departed Nothing Nothing Nothing)]
sampleIO = readFile "test/testdata/6924-1445708047.html"

testGuess = testCase "guess time" $ do
  tzs <- tzFileIO
  assertEqual "guess time" (UTCTime testDay expectedDiff) (guessUTCTime testUTC tzs 1 52)

testFromFile = testCase "read data from file" $ do
  html <- sampleIO
  tzs <- tzFileIO
  let result = parseStatusPage sampleTime tzs html
  assertEqual "trainName" "6924" (trainName result)
  assertEqual "first stop" ("Dover", Departed Nothing Nothing Nothing) (head $ stops result)
  let expectedTime = UTCTime (fromGregorian 2015 10 24) (secondsToDiffTime (18 * 3600 + 39 * 60))
  assertEqual "last stop" ("New York Penn Station", Expected expectedTime) (last $ stops result)
 

main = defaultMain [testGuess, testFromFile]
