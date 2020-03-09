{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TypeFamilies, DataKinds, GADTs, QuasiQuotes, FlexibleContexts, LambdaCase, RecordWildCards #-}

module Web.Slack.ProgressBar where

import Control.Concurrent
import Control.Lens hiding ((??))
import Control.Monad.Error
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wreq as W
import Web.Slack
import Web.Slack.WebAPI

mySlackConfig :: SlackConfig
mySlackConfig = SlackConfig { _slackApiToken = "TODO" }

data ProgressBarInfo = ProgressBarInfo { progressBarInfoTopMessage :: Maybe T.Text
                                       , progressBarInfoBottomMessage :: Maybe T.Text
                                       , progressBarInfoSize :: Double }

data ProgressBar = ProgressBar { progressBarTs :: T.Text
                               , progressBarChannel :: T.Text }

type ChannelName = T.Text

instance Error T.Text where
  noMsg = ""

go :: IO ()
go = void $ runExceptT $ do
  let progressBarInfo = ProgressBarInfo {
        progressBarInfoTopMessage = Just "Top message"
        , progressBarInfoBottomMessage = Just "Bottom message"
        , progressBarInfoSize = 0
        }

  progressBar <- ExceptT $ createProgressBar mySlackConfig "test-channel" progressBarInfo

  forM_ [10, 20, 30, 40, 50, 60, 70, 80, 90, 100] $ \size -> do
    liftIO $ threadDelay 1000000
    ExceptT $ updateProgressBar mySlackConfig progressBar (progressBarInfo { progressBarInfoSize = size })


-- * Exported

createProgressBar :: SlackConfig -> ChannelName -> ProgressBarInfo -> IO (Either T.Text ProgressBar)
createProgressBar slackConfig channel (ProgressBarInfo {..}) =
  (runExceptT $ postMessage slackConfig (Id channel) (barSized 25) []) >>= \case
    Left err -> return $ Left [i|Failed to send initial result: '#{err}'|]
    Right resp -> case (resp ^? key "ts" . _String, resp ^? key "channel" . _String) of
      (Just ts, Just chan) -> return $ Right $ ProgressBar ts chan
      _ -> return $ Left [i|Couldn't find timestamp and/or channel in response|]

updateProgressBar :: SlackConfig -> ProgressBar -> ProgressBarInfo -> IO (Either T.Text ())
updateProgressBar slackConfig (ProgressBar {..}) pbi@(ProgressBarInfo {..}) =
  (runExceptT $ updateMessage slackConfig (Id progressBarChannel) progressBarTs (getMessage pbi) []) >>= \case
    Left err -> return $ Left [i|Failed to update progress bar: '#{err}'|]
    Right _ -> return $ Right ()

-- * Internal

getMessage :: ProgressBarInfo -> T.Text
getMessage (ProgressBarInfo {..}) =
  T.intercalate "\n" $ catMaybes [progressBarInfoTopMessage
                                 , Just $ barSized progressBarInfoSize
                                 , progressBarInfoBottomMessage]

barSized :: Double -> T.Text
barSized n = T.replicate (round $ n / 2.0) $ T.singleton $ chr 9608

postMessage :: (MonadError T.Text m, MonadIO m) => SlackConfig -> ChannelId -> T.Text -> [Attachment] -> m Value
postMessage conf (Id cid) msg as =
  makeSlackCall conf "chat.postMessage" $
    (W.param "channel"     .~ [cid])
    . (W.param "text"        .~ [msg])
    . (W.param "attachments" .~ [encode' as])
    . (W.param "as_user"     .~ ["true"])

updateMessage :: (MonadError T.Text m, MonadIO m) => SlackConfig -> ChannelId -> T.Text -> T.Text -> [Attachment] -> m ()
updateMessage conf (Id cid) ts msg as =
  void $ makeSlackCall conf "chat.update" $
    (W.param "channel"     .~ [cid]) .
    (W.param "text"        .~ [msg]) .
    (W.param "attachments" .~ [encode' as]) .
    (W.param "as_user"     .~ ["true"]) .
    (W.param "ts"          .~ [ts])

encode' :: ToJSON a => a -> T.Text
encode' = T.decodeUtf8 . BL.toStrict . encode
