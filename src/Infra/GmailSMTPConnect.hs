{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Infra.GmailSMTPConnect
  (
    sendEmailReport,
  ) where

import qualified Data.Text as DT
import Network.Mail.Mime
import Network.Mail.SMTP

import System.IO.Error
import Data.Aeson
import Data.Aeson.TH

import GHC.Generics
--  AMQ ----
import Data.ByteString.Internal

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TT
import           qualified  Data.ByteString.Lazy.Internal as B8 


data JsonMailReport = JsonMailReport {
      recipientname :: String,
      recipientemail :: String,
      sendername :: String,
      senderemail :: String,
      subject :: String,
      plaintext :: String,
      htmltext :: String,
      filecontent :: String,
      filename :: String
  } deriving (Generic, Show)
$(deriveJSON defaultOptions ''JsonMailReport)

data JsonMailCode = JsonMailCode {
      recipientnamec :: String,
      recipientemailc :: String,
      sendernamec :: String,
      senderemailc :: String,
      subjectc :: String,
      plaintextc :: String,
      htmltextc :: String
  } deriving (Generic, ToJSON, FromJSON)
-- Configuration
gmailApiUsername :: String
gmailApiUsername = ""

gmailApiPassword :: String
gmailApiPassword = ""

hostName :: String
hostName = "smtp.gmail.com"

-- Sending E-Mail
sendEmailReport :: String -> String -> String -> String -> DT.Text -> TT.Text ->  TT.Text -> B8.ByteString -> String -> IO Bool
sendEmailReport  nomdestinataire emaildestinataire nomexpediteur emailexpediteur sujet plaintext htmltext filecontent filename = do  
  putStrLn "000000000000000000000000000"
  --let connectionOpts = fromURI "amqp:/admin:Ange1308!!@localhost:5672/"
  --conn <- openConnection'' connectionOpts
  conn <- openConnection "127.0.0.1" "/" "admin" "Ange2310!!"
  putStrLn "11111111111111111111111111"
  chan <- openChannel conn
  putStrLn "222222222222222222222222222"
  -- declare a queue, exchange and binding
  declareQueue chan newQueue {queueName = "EmailQueue"}
  declareExchange chan newExchange {exchangeName = "EmailExchange", exchangeType = "direct"}
  bindQueue chan "EmailQueue" "EmailExchange" "myKey"
  
  
  -- subscribe to the queue
  consumeMsgs chan "EmailQueue" Ack sendEmailCallback
  
  -- publish a message to our new exchange
  let mailData = JsonMailReport 
                      nomdestinataire
                      emaildestinataire
                      nomexpediteur 
                      emailexpediteur 
                      (DT.unpack sujet) 
                      (TT.unpack plaintext) 
                      (TT.unpack htmltext) 
                      (B8.unpackChars filecontent)
                      filename
  publishMsg chan "EmailExchange" "myKey"
    newMsg {msgBody = encode mailData{-(BL.pack "hello world")-},
                  msgDeliveryMode = Just Persistent}
  
  closeConnection conn
  putStrLn "connection closed"
  
  return True
  
errorHandler :: IOError -> IO ()

errorHandler e | isDoesNotExistError e = putStrLn "The file doesn't exist!"
               | isFullError e = putStrLn "Please try to free space and retry"
               | isIllegalOperation e = putStrLn "Please verify your data input"
               | otherwise = ioError e
-- 

sendEmailCallback :: (Message,Envelope) -> IO ()
sendEmailCallback (msg, env) = do
    let mailData = decode $ (msgBody msg) :: Maybe JsonMailReport
    case mailData of 
      Just x -> do
         putStrLn $ "received message: " ++ show x
         let monmail = Mail
                              (Address (Just $ DT.pack (sendername x)) (DT.pack (senderemail x)))
                              [(Address (Just $ DT.pack (recipientname x)) (DT.pack (recipientemail x)))]
                              []
                              []
                              [(Data.ByteString.Internal.packChars "subject",  DT.pack (subject x))]
                              [[(Part (DT.pack "text/plain") QuotedPrintableText DefaultDisposition [] (PartContent $ B8.packChars $ TT.unpack (TT.pack $ plaintext x)))], [(Part (DT.pack "text/html") QuotedPrintableText DefaultDisposition [] (NestedParts [Network.Mail.Mime.htmlPart (TT.pack $ htmltext x)]))], [(Part (DT.pack "application/pdf") QuotedPrintableBinary (AttachmentDisposition $ DT.pack (filename x)) [] (PartContent (B8.packChars $ filecontent x)))]]
         putStrLn "7777777777777777777777777777777777"
         sendMailWithLoginTLS hostName  gmailApiUsername gmailApiPassword monmail
         --catchIOError (sendMailWithLoginTLS hostName  gmailApiUsername gmailApiPassword monmail) errorHandler 
         putStrLn "888888888888888888888888888888888888888"
      Nothing -> putStrLn "Error On decode"
    
    -- acknowledge receiving the message
    ackEnv env




