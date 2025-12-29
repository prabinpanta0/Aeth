{-# LANGUAGE OverloadedStrings #-}

module Aeth.Parse
  ( parsePipeline,
    renderParseError,
  )
where

-- Minimal placeholder parser:
-- - splits by '|'
-- - '@cmd' means Structured, otherwise RawString
-- - arguments split by whitespace (no quoting yet)
-- This gets the shell runnable; we can replace with Megaparsec next.

import Aeth.Types
import Data.Char (isSpace)
import qualified Data.Text as T

parsePipeline :: T.Text -> Either String Pipeline
parsePipeline input0 =
  let input = T.dropWhile isSpaceText input0
   in if T.null input
        then Left "empty"
        else Right (Pipeline (map parseSegment (splitPipes input)))
  where
    isSpaceText = isSpace

    splitPipes :: T.Text -> [T.Text]
    splitPipes = map T.strip . T.splitOn "|"

    parseSegment :: T.Text -> Segment
    parseSegment segText =
      let toks = filter (not . T.null) (T.words segText)
       in case toks of
            [] -> Segment RawString "" []
            (t : rest) ->
              case T.uncons t of
                Just ('@', name) -> Segment Structured name rest
                _ -> Segment RawString t rest

renderParseError :: String -> T.Text
renderParseError = T.pack
