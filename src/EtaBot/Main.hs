{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString    as B
import           Data.Maybe
import qualified Data.Text          as T
import           Data.Text.Encoding
import           Network.SimpleIRC
import           System.IO

botName :: String
botName = "faybot"

checkChans :: [B.ByteString]
checkChans = ["#haskell","#snapframework"]
sendChan :: B.ByteString
sendChan = "#fay"
ignoreSpeakers :: [B.ByteString]
ignoreSpeakers = ["etabot"]

config :: IrcConfig
config = defaultConfig
    { cAddr = "irc.freenode.net"
    , cPort = 6667
    , cNick = botName
    , cUsername = botName
    , cRealname = botName
    , cChannels = ["#fay-test1", "#fay-test2", "#fay-test3"]
    , cEvents = [notifySnap]
    }


onDecodeError :: a -> b -> Maybe c
onDecodeError _ _ = Nothing

subst :: Eq a => [a] -> [a] -> [a] -> [a]
subst _    _  [       ] = []
subst from to xs@(a:as) =
    if isPrefixOf from xs
        then to ++ subst from to (drop (length from) xs)
        else a : subst from to as
    where isPrefixOf as' bs = and $ zipWith (==) as' bs

------------------------------------------------------------------------------
notifySnap :: IrcEvent
notifySnap = Privmsg $ \mirc imsg -> do
    let actualMsg = decodeUtf8With onDecodeError $ mMsg imsg
        msg = T.concat $ T.splitOn "snapshot" $ T.toLower $ decodeUtf8With onDecodeError $ mMsg imsg
        speaker = fromMaybe "Someone" (mNick imsg)
        chan = fromMaybe "<unknown>" (mChan imsg)
        checkNotify str = when (str `T.isInfixOf` msg) $ do
            let out = B.concat
                    [ speaker
                    , " is talking about "
                    , encodeUtf8 str
                    , " in "
                    , chan
                    , ": "
                    , encodeUtf8 actualMsg
                    ]
            sendMsg mirc sendChan out
            putStrLn $ T.unpack $ decodeUtf8 out
    when (chan `elem` checkChans && not (speaker `elem` ignoreSpeakers))
          (checkNotify "fay")

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Connecting to server"
    emirc <- connect config False False
    either (error . show) return emirc
    putStrLn "Connection closed."
    main
