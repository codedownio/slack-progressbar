{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.Except
import Web.Slack.ProgressBar

mySlackConfig :: SlackConfig
mySlackConfig = SlackConfig { slackApiToken = "TODO" }

main :: IO ()
main = void $ runExceptT $ do
  let progressBarInfo = ProgressBarInfo {
        progressBarInfoTopMessage = Just "Top message"
        , progressBarInfoBottomMessage = Just "Bottom message"
        , progressBarInfoSize = Just 0
        }

  progressBar <- ExceptT $ createProgressBar mySlackConfig "test-channel" progressBarInfo

  forM_ [10, 20, 30, 40, 50, 60, 70, 80, 90, 100] $ \size -> do
    liftIO $ threadDelay 1000000
    ExceptT $ updateProgressBar mySlackConfig progressBar (progressBarInfo { progressBarInfoSize = Just size })
