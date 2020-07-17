{-# LANGUAGE OverloadedStrings, QuasiQuotes, LambdaCase #-}

module Main where

import Control.Concurrent
import Control.Monad.Except
import Data.IORef
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Web.Slack.ProgressBar

mySlackConfig :: SlackConfig
mySlackConfig = SlackConfig { slackApiToken = "my-slack-api-token" }

main :: IO ()
main = runProgressBar >>= \case
  Left err -> error [i|Progress bar failed: '#{err}'|]
  Right () -> return ()

runProgressBar :: IO (Either T.Text ())
runProgressBar = runExceptT $ do
  let progressBarInfo = ProgressBarInfo {
        progressBarInfoTopMessage = Just "Top message"
        , progressBarInfoBottomMessage = Just "Bottom message"
        , progressBarInfoSize = Just 0
        , progressBarInfoAttachments = Just []
        }

  progressBar <- ExceptT $ createProgressBar mySlackConfig "test-channel" progressBarInfo

  attachmentsRef <- liftIO $ newIORef []
  forM_ [10, 20, 30, 40, 50, 60, 70, 80, 90, 100] $ \size -> do
    liftIO $ threadDelay 1000000

    -- Add attachments on a few examples
    when (size `elem` [30, 60, 90]) $
      liftIO $ modifyIORef attachmentsRef (<> [[i|Got failure at #{size}|]])

    attachments <- liftIO $ readIORef attachmentsRef

    ExceptT $ updateProgressBar mySlackConfig progressBar (
      progressBarInfo {
          progressBarInfoSize = Just size
          , progressBarInfoAttachments = Just [ProgressBarAttachment x "#ff4136" | x <- attachments]
          , progressBarInfoBottomMessage = Just [i|Currently running at #{size}|]
          })
