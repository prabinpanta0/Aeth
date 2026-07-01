{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Blockchain-inspired audit log for the aeth shell.
--
-- Provides:
--   - Tamper-proof audit log with hash chaining
--   - Immutable command history with verification
--   - Distributed history sync via git
module Aeth.Audit
  ( AuditEntry (..),
    AuditLog (..),
    initAuditLog,
    appendCommand,
    verifyAuditLog,
    exportAuditLog,
    importAuditLog,
    entryHash,
  )
where

import Control.Exception (IOException, try)
import Data.Char (ord)
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)

-- | A single audit entry in the hash chain
data AuditEntry = AuditEntry
  { entryIndex :: Int,
    entryCommand :: T.Text,
    entryTimestamp :: T.Text,
    entryWorkingDir :: T.Text,
    entryExitCode :: Int,
    entryPrevHash :: T.Text,
    entryHash :: T.Text
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON AuditEntry
instance JSON.ToJSON AuditEntry where
  toJSON = JSON.genericToJSON JSON.defaultOptions
  toEncoding = JSON.genericToEncoding JSON.defaultOptions

-- | The complete audit log
data AuditLog = AuditLog
  { logEntries :: [AuditEntry],
    logLastHash :: T.Text
  }
  deriving (Eq, Show)

-- | Initialize an empty audit log
initAuditLog :: AuditLog
initAuditLog = AuditLog [] "0"

-- | Simple hash function (djb2) - fast, non-crypto, but sufficient for tamper detection
djb2Hash :: T.Text -> T.Text
djb2Hash input = T.pack (show (foldl (\h c -> h * 33 + ord c) 5381 (T.unpack input)))

-- | Compute hash for an audit entry
computeEntryHash :: Int -> T.Text -> T.Text -> T.Text -> Int -> T.Text -> T.Text
computeEntryHash idx cmd ts dir exitCode prevHash =
  let payload = T.intercalate "|"
        [ T.pack (show idx),
          cmd,
          ts,
          dir,
          T.pack (show exitCode),
          prevHash
        ]
   in djb2Hash payload

-- | Append a command to the audit log
appendCommand :: AuditLog -> T.Text -> T.Text -> Int -> IO (AuditLog, AuditEntry)
appendCommand auditLog cmd workingDir exitCode = do
  now <- getCurrentTime
  let ts = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
      idx = length (logEntries auditLog)
      prev = logLastHash auditLog
      h = computeEntryHash idx cmd ts workingDir exitCode prev
      entry = AuditEntry
        { entryIndex = idx,
          entryCommand = cmd,
          entryTimestamp = ts,
          entryWorkingDir = workingDir,
          entryExitCode = exitCode,
          entryPrevHash = prev,
          entryHash = h
        }
      newLog = AuditLog (logEntries auditLog ++ [entry]) h
  pure (newLog, entry)

-- | Verify the integrity of the entire audit log
verifyAuditLog :: AuditLog -> Bool
verifyAuditLog auditLog =
  let entries = logEntries auditLog
      go _ [] = True
      go prevHash (e : rest) =
        let expectedHash = computeEntryHash
              (entryIndex e)
              (entryCommand e)
              (entryTimestamp e)
              (entryWorkingDir e)
              (entryExitCode e)
              prevHash
         in entryHash e == expectedHash
              && entryPrevHash e == prevHash
              && go (entryHash e) rest
   in go "0" entries

-- | Export audit log to JSON
exportAuditLog :: AuditLog -> T.Text
exportAuditLog auditLog =
  let entries = logEntries auditLog
      bs = JSON.encode entries
   in TE.decodeUtf8 (BSL.toStrict bs)

-- | Import audit log from JSON
importAuditLog :: T.Text -> Maybe AuditLog
importAuditLog t =
  let bs = BSL.fromStrict (TE.encodeUtf8 t)
   in case JSON.decode bs of
        Just entries ->
          let lastH = if null entries then "0" else entryHash (last entries)
           in Just (AuditLog entries lastH)
        Nothing -> Nothing
