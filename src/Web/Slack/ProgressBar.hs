{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, QuasiQuotes, FlexibleContexts, LambdaCase, RecordWildCards #-}

{-|
Module:      Web.Slack.ProgressBar
Copyright:   (c) 2020 Tom McLaughlin
License:     MIT
Stability:   experimental
Portability: portable

This is a simple library for creating and updating Slack messages that contain progressbars.
It can be used to display the progress of a long-running task in a Slack channel, for example as part of CI tooling.

@
main :: IO ()
main = do
  let mySlackConfig = SlackConfig { slackApiToken = "my-slack-api-token" }

  let progressBarInfo = def { progressBarInfoTopMessage = Just "Top message"
                            , progressBarInfoSize = Just 0 }

  result <- runExceptT $ do
    progressBar <- ExceptT $ createProgressBar mySlackConfig "test-channel" progressBarInfo

    forM_ [10, 20, 30, 40, 50, 60, 70, 80, 90, 100] $ \\size -> do
      threadDelay 1000000
      ExceptT $ updateProgressBar mySlackConfig progressBar (progressBarInfo { progressBarInfoSize = Just size })

  putStrLn [i|Result: '#{result}'|]
@

-}

module Web.Slack.ProgressBar (
  createProgressBar
  , updateProgressBar
  , ProgressBarInfo (..)
  , ProgressBarAttachment (..)
  , ProgressBar
  , ChannelName

  -- * Re-exports
  , SlackConfig (..)
  ) where

import Control.Lens hiding ((??))
import Control.Monad.Except
import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wreq as W


-- | The state of a progress bar message.
data ProgressBarInfo = ProgressBarInfo { progressBarInfoTopMessage :: Maybe T.Text
                                       -- ^ Message to show above the progress bar
                                       , progressBarInfoBottomMessage :: Maybe T.Text
                                       -- ^ Message to show below the progress bar
                                       , progressBarInfoSize :: Maybe Double
                                       -- ^ Size of the progress bar, a 'Double' from 0 to 100
                                       , progressBarInfoAttachments :: Maybe [ProgressBarAttachment]
                                       -- ^ Slack attachments for the message
                                       }

-- | A Slack attachment.
data ProgressBarAttachment = ProgressBarAttachment { progressBarAttachmentText :: T.Text
                                                   -- ^ Attachment text
                                                   , progressBarAttachmentColor :: T.Text
                                                   -- ^ Attachment color
                                                   }
instance ToJSON ProgressBarAttachment
  where toJSON (ProgressBarAttachment {..}) = A.object [("text", A.String progressBarAttachmentText)
                                                       , ("color", A.String progressBarAttachmentColor)]

-- | An opaque type representing an existing Slack message.
data ProgressBar = ProgressBar { progressBarTs :: T.Text
                               , progressBarChannel :: T.Text }

type ChannelName = T.Text

-- * Exported

-- | Create a progress bar message on the given channel.
-- Returns a 'ProgressBar' which can be used to update the message by calling 'updateProgressBar'.
createProgressBar :: SlackConfig -> ChannelName -> ProgressBarInfo -> IO (Either T.Text ProgressBar)
createProgressBar slackConfig channel pbi =
  (runExceptT $ postMessage slackConfig channel (getMessage pbi) (getAttachments pbi)) >>= \case
    Left err -> return $ Left [i|Failed to send initial result: '#{err}'|]
    Right resp -> case (resp ^? key "ts" . _String, resp ^? key "channel" . _String) of
      (Just ts, Just chan) -> return $ Right $ ProgressBar ts chan
      _ -> return $ Left [i|Couldn't find timestamp and/or channel in response|]

-- | Update an existing progress bar.
updateProgressBar :: SlackConfig -> ProgressBar -> ProgressBarInfo -> IO (Either T.Text ())
updateProgressBar slackConfig (ProgressBar {..}) pbi@(ProgressBarInfo {..}) =
  (runExceptT $ updateMessage slackConfig progressBarChannel progressBarTs (getMessage pbi) (getAttachments pbi)) >>= \case
    Left err -> return $ Left [i|Failed to update progress bar: '#{err}'|]
    Right _ -> return $ Right ()

-- * Internal

getMessage :: ProgressBarInfo -> T.Text
getMessage (ProgressBarInfo {..}) =
  T.intercalate "\n" $ catMaybes [progressBarInfoTopMessage
                                 , barSized <$> progressBarInfoSize
                                 , progressBarInfoBottomMessage]

getAttachments :: ProgressBarInfo -> [A.Value]
getAttachments (ProgressBarInfo {..}) = maybe [] (fmap A.toJSON) progressBarInfoAttachments

barSized :: Double -> T.Text
barSized n = (T.replicate darkBlocks $ T.singleton $ chr 9608)
             <> (T.replicate lightBlocks $ T.singleton $ chr 9617)
             <> [i| #{roundTo 2 n}%|]
  where darkBlocks = round $ n * multiplier
        lightBlocks = round $ (100 - n) * multiplier
        multiplier = 0.5

        roundTo :: (Fractional a, RealFrac a) => Integer -> a -> a
        roundTo places num = (fromInteger $ round $ num * (10^places)) / (10.0^^places)

postMessage :: (MonadError T.Text m, MonadIO m) => SlackConfig -> ChannelName -> T.Text -> [A.Value] -> m Value
postMessage conf cid msg as =
  makeSlackCall conf "chat.postMessage" $
    (W.param "channel"     .~ [cid])
    . (W.param "text"        .~ [msg])
    . (W.param "attachments" .~ [encode' as])
    . (W.param "as_user"     .~ ["true"])

updateMessage :: (MonadError T.Text m, MonadIO m) => SlackConfig -> ChannelName -> T.Text -> T.Text -> [A.Value] -> m ()
updateMessage conf cid ts msg as =
  void $ makeSlackCall conf "chat.update" $
    (W.param "channel"     .~ [cid]) .
    (W.param "text"        .~ [msg]) .
    (W.param "attachments" .~ [encode' as]) .
    (W.param "as_user"     .~ ["true"]) .
    (W.param "ts"          .~ [ts])

encode' :: ToJSON a => a -> T.Text
encode' = T.decodeUtf8 . BL.toStrict . encode

-- Inlined and modified slightly from slack-api
-- Didn't seem worth it to add that entire library as a dependency just for these

-- | Configuration options needed to connect to the Slack API
newtype SlackConfig = SlackConfig { slackApiToken :: String
                                  -- ^ Slack API token
                                  } deriving (Show)

makeSlackCall :: (MonadError T.Text m, MonadIO m) => SlackConfig -> String -> (W.Options -> W.Options) -> m Value
makeSlackCall conf method setArgs = do
  let url = "https://slack.com/api/" ++ method
  let setToken = W.param "token" .~ [T.pack (slackApiToken conf)]
  let opts = W.defaults & setToken & setArgs
  rawResp <- liftIO $ W.getWith opts url
  resp <- rawResp ^? W.responseBody . _Value ?? "Couldn't parse response"
  case resp ^? key "ok" . _Bool of
    Just True -> return resp
    Just False -> throwError $ resp ^. key "error" . _String
    Nothing -> throwError "Couldn't parse key 'ok' from response"

infixl 7 ??
(??) :: MonadError e m => Maybe a -> e -> m a
x ?? e = maybe (throwError e) return x
