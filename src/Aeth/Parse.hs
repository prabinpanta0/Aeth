{-# LANGUAGE OverloadedStrings #-}

module Aeth.Parse
  ( parseCommandList,
    parsePipeline,
    renderParseError,
  )
where

-- \| Improved parser with proper quoting support:
-- - Supports double quotes: "hello world" -> single argument
-- - Supports single quotes: 'hello world' -> single argument (no var expansion)
-- - Supports escape: hello\ world -> single argument
-- - Splits by '|' for pipes
-- - Splits by '&&', '||', ';' for command lists
-- - '@cmd' means Structured, otherwise RawString

import Aeth.Types
import Data.Char (isSpace)
import qualified Data.Text as T

-- | Parse input into a command list (handling &&, ||, ;)
parseCommandList :: T.Text -> Either String CommandList
parseCommandList input0 =
  let input = T.dropWhile isSpace input0
   in if T.null input
        then Left "empty"
        else
          let parts = splitByListOps input
           in Right (CommandList (map parsePipelineWithOp parts))

-- | Split by &&, ||, ; while respecting quotes
-- Returns list of (command text, operator that follows it)
splitByListOps :: T.Text -> [(T.Text, Maybe ListOperator)]
splitByListOps input = go (T.unpack input) "" [] Nothing
  where
    go :: String -> String -> [(T.Text, Maybe ListOperator)] -> Maybe Char -> [(T.Text, Maybe ListOperator)]
    go [] acc parts _ =
      let cmd = T.strip (T.pack (reverse acc))
       in if T.null cmd then parts else parts ++ [(cmd, Nothing)]
    go ('\\' : c : rest) acc parts inQuote =
      go rest (c : '\\' : acc) parts inQuote
    go ('"' : rest) acc parts Nothing =
      go rest ('"' : acc) parts (Just '"')
    go ('"' : rest) acc parts (Just '"') =
      go rest ('"' : acc) parts Nothing
    go ('\'' : rest) acc parts Nothing =
      go rest ('\'' : acc) parts (Just '\'')
    go ('\'' : rest) acc parts (Just '\'') =
      go rest ('\'' : acc) parts Nothing
    -- && operator (outside quotes)
    go ('&' : '&' : rest) acc parts Nothing =
      let cmd = T.strip (T.pack (reverse acc))
       in go rest "" (parts ++ [(cmd, Just OpAnd)]) Nothing
    -- \|| operator (outside quotes)
    go ('|' : '|' : rest) acc parts Nothing =
      let cmd = T.strip (T.pack (reverse acc))
       in go rest "" (parts ++ [(cmd, Just OpOr)]) Nothing
    -- ; operator (outside quotes)
    go (';' : rest) acc parts Nothing =
      let cmd = T.strip (T.pack (reverse acc))
       in go rest "" (parts ++ [(cmd, Just OpSeq)]) Nothing
    go (c : rest) acc parts inQuote =
      go rest (c : acc) parts inQuote

-- | Parse a single pipeline part with its operator
parsePipelineWithOp :: (T.Text, Maybe ListOperator) -> (Pipeline, Maybe ListOperator)
parsePipelineWithOp (txt, op) =
  (Pipeline (map parseSegment (splitByPipe txt)), op)

parsePipeline :: T.Text -> Either String Pipeline
parsePipeline input0 =
  let input = T.dropWhile isSpace input0
   in if T.null input
        then Left "empty"
        else Right (Pipeline (map parseSegment (splitByPipe input)))

-- | Split text by '|' while respecting quotes
splitByPipe :: T.Text -> [T.Text]
splitByPipe input = go (T.unpack input) "" [] Nothing
  where
    go :: String -> String -> [T.Text] -> Maybe Char -> [T.Text]
    go [] acc segments _ = segments ++ [T.strip (T.pack (reverse acc))]
    go ('\\' : c : rest) acc segments inQuote =
      go rest (c : '\\' : acc) segments inQuote
    go ('"' : rest) acc segments Nothing =
      go rest ('"' : acc) segments (Just '"')
    go ('"' : rest) acc segments (Just '"') =
      go rest ('"' : acc) segments Nothing
    go ('\'' : rest) acc segments Nothing =
      go rest ('\'' : acc) segments (Just '\'')
    go ('\'' : rest) acc segments (Just '\'') =
      go rest ('\'' : acc) segments Nothing
    go ('|' : rest) acc segments Nothing =
      go rest "" (segments ++ [T.strip (T.pack (reverse acc))]) Nothing
    go (c : rest) acc segments inQuote =
      go rest (c : acc) segments inQuote

-- | Parse a single segment (command + args)
parseSegment :: T.Text -> Segment
parseSegment segText =
  let toks = tokenize (T.unpack segText)
      -- Separate redirections from regular args
      (args, redirs, isBg) = parseTokens toks
   in case args of
        [] -> Segment RawString "" [] redirs isBg
        (t : rest) ->
          case T.uncons (T.pack t) of
            Just ('@', name) -> Segment Structured name (map T.pack rest) redirs isBg
            _ -> Segment RawString (T.pack t) (map T.pack rest) redirs isBg

-- | Parse tokens into args, redirections, and background flag
parseTokens :: [String] -> ([String], [Redirection], Bool)
parseTokens = go [] [] False
  where
    go args redirs bg [] = (reverse args, reverse redirs, bg)
    go args redirs _ ("&" : rest) = go args redirs True rest
    go args redirs bg (">>" : file : rest) =
      go args (Redirection RedirectAppend (T.pack file) : redirs) bg rest
    go args redirs bg (">" : file : rest) =
      go args (Redirection RedirectOut (T.pack file) : redirs) bg rest
    go args redirs bg ("<" : file : rest) =
      go args (Redirection RedirectIn (T.pack file) : redirs) bg rest
    go args redirs bg ("2>>" : file : rest) =
      go args (Redirection RedirectErrAppend (T.pack file) : redirs) bg rest
    go args redirs bg ("2>" : file : rest) =
      go args (Redirection RedirectErr (T.pack file) : redirs) bg rest
    go args redirs bg ("2>&1" : rest) =
      go args (Redirection RedirectErrToOut T.empty : redirs) bg rest
    go args redirs bg ("&>" : file : rest) =
      go args (Redirection RedirectOutAndErr (T.pack file) : redirs) bg rest
    -- Handle attached redirections like ">file" or ">>file"
    go args redirs bg (tok : rest)
      | ">>" `isPrefixOf` tok =
          go args (Redirection RedirectAppend (T.pack (drop 2 tok)) : redirs) bg rest
      | ">" `isPrefixOf` tok =
          go args (Redirection RedirectOut (T.pack (drop 1 tok)) : redirs) bg rest
      | "<" `isPrefixOf` tok =
          go args (Redirection RedirectIn (T.pack (drop 1 tok)) : redirs) bg rest
      | "2>>" `isPrefixOf` tok =
          go args (Redirection RedirectErrAppend (T.pack (drop 3 tok)) : redirs) bg rest
      | "2>" `isPrefixOf` tok =
          go args (Redirection RedirectErr (T.pack (drop 2 tok)) : redirs) bg rest
      | otherwise = go (tok : args) redirs bg rest

    isPrefixOf prefix str = take (length prefix) str == prefix

-- | Tokenize a string with proper quoting support
tokenize :: String -> [String]
tokenize = go "" [] Nothing
  where
    go :: String -> [String] -> Maybe Char -> String -> [String]
    go acc toks _ [] =
      if null acc then toks else toks ++ [reverse acc]
    -- Escape handling
    go acc toks inQuote ('\\' : c : rest) =
      let escaped = case c of
            'n' -> '\n'
            't' -> '\t'
            'r' -> '\r'
            _ -> c
       in go (escaped : acc) toks inQuote rest
    -- Double quote handling
    go acc toks Nothing ('"' : rest) =
      go acc toks (Just '"') rest
    go acc toks (Just '"') ('"' : rest) =
      go acc toks Nothing rest
    -- Single quote handling (no escape processing inside)
    go acc toks Nothing ('\'' : rest) =
      go acc toks (Just '\'') rest
    go acc toks (Just '\'') ('\'' : rest) =
      go acc toks Nothing rest
    -- Whitespace outside quotes - token boundary
    go acc toks Nothing (c : rest)
      | isSpace c =
          if null acc
            then go "" toks Nothing rest
            else go "" (toks ++ [reverse acc]) Nothing rest
    -- Regular character
    go acc toks inQuote (c : rest) =
      go (c : acc) toks inQuote rest

renderParseError :: String -> T.Text
renderParseError = T.pack
