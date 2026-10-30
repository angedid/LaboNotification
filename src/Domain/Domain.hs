module Domain.Domain 
  (
    sendMailReport,
    sendMailCode
  )where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TT
import Infra.GmailSMTPConnect
import           qualified  Data.ByteString.Lazy.Internal as B8 
import Network.Mail.Mime
import Data.ByteString.Internal

nomExpediteur :: String
nomExpediteur = "SOFT INNOV ++"

emailExpediteur :: String
emailExpediteur = "softinovplus@gmail.com"

sendMailReport :: String -> String -> T.Text -> TT.Text ->  TT.Text -> B8.ByteString -> String -> IO Bool
sendMailReport nomdestinataire emaildestinataire sujet plaintext htmltext filecontent filename = do
  {-let lemail = Mail
        (Address (Just $ T.pack nomExpediteur) (T.pack emailExpediteur))
        [(Address (Just $ T.pack nomdestinataire) (T.pack emaildestinataire))]
        []
        []
        [(Data.ByteString.Internal.packChars "subject",  sujet)]
        [[(Part (T.pack "text/plain") QuotedPrintableText DefaultDisposition [] (PartContent $ B8.packChars $ TT.unpack plaintext))], [(Part (T.pack "text/html") QuotedPrintableText DefaultDisposition [] (NestedParts [Network.Mail.Mime.htmlPart htmltext]){-(PartContent $ B8.packChars $ TT.unpack htmltext)-})], [(Part (T.pack "application/pdf") QuotedPrintableBinary (AttachmentDisposition $ T.pack filename) [] (PartContent filecontent))] {-[textVersion, htmlVersion], [attachment1], [attachment1]-}]
   -}
  sendEmailReport nomdestinataire emaildestinataire nomExpediteur emailExpediteur sujet plaintext htmltext filecontent filename


sendMailCode :: String -> String -> T.Text -> TT.Text ->  TT.Text -> IO Bool
sendMailCode nomdestinataire emaildestinataire sujet plaintext htmltext = do
  {-let monmail = Mail
        (Address (Just $ T.pack nomExpediteur) (T.pack emailExpediteur))
        [(Address (Just $ T.pack nomdestinataire) (T.pack emaildestinataire))]
        []
        []
        [(Data.ByteString.Internal.packChars "subject",  sujet)]
        [[(Part (T.pack "text/plain") QuotedPrintableText DefaultDisposition [] (PartContent $ B8.packChars $ TT.unpack plaintext))], [(Part (T.pack "text/html") QuotedPrintableText DefaultDisposition [] (NestedParts [Network.Mail.Mime.htmlPart htmltext]))]]
   -}
  sendEmailReport nomdestinataire emaildestinataire nomExpediteur emailExpediteur sujet plaintext htmltext (B8.packChars "") ""
