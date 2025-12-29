{-# LANGUAGE OverloadedStrings #-}

module Aeth.Structured
  ( StructuredValue (..),
    renderStructured,
    lsStructured,
    pwdStructured,
    filterStructured,
  )
where

import Data.Char (isDigit, toLower)
import Data.Int (Int64)
import qualified Data.List as List
import qualified Data.Text as T
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified System.Posix.Files as Posix

-- Keep it simple: a small structured algebra we can extend.

data StructuredValue
  = SText T.Text
  | STable [T.Text] [[T.Text]]
  deriving (Eq, Show)

-- | Filter a structured table with a tiny expression language.
--
-- Supported forms (spaces required around operator):
--   - .size > 1MB
--   - .size >= 1024
--   - .name == foo
--   - .kind != dir
--   - .name contains kde
--
-- Braces are allowed and ignored, e.g. "{ .size > 1MB }".
filterStructured :: T.Text -> StructuredValue -> Either T.Text StructuredValue
filterStructured rawExpr v =
  case v of
    SText _ -> Left "filter: expected table input"
    STable headers rows -> do
      (field, op, lit) <- parsePredicate rawExpr
      idx <- maybe (Left ("filter: unknown field: " <> field)) Right (List.elemIndex field headers)
      let keepRow r =
            let cell = if idx < length r then r !! idx else ""
             in evalPredicate field op lit cell
      pure (STable headers (filter keepRow rows))

data Op = OpEq | OpNe | OpGt | OpGe | OpLt | OpLe | OpContains

parsePredicate :: T.Text -> Either T.Text (T.Text, Op, T.Text)
parsePredicate expr0 =
  let expr1 = stripBraces (T.strip expr0)
      toks = T.words expr1
   in case toks of
        (f : o : rest) | not (null rest) -> do
          op <- parseOp o
          pure (normalizeField f, op, T.unwords rest)
        _ -> Left "filter: expected: <field> <op> <value>"
  where
    stripBraces t =
      let t1 = T.dropWhile (== '{') (T.dropWhileEnd (== '}') t)
       in T.strip t1

    normalizeField f =
      let f1 = T.strip f
       in if T.isPrefixOf "." f1 then T.drop 1 f1 else f1

    parseOp t =
      case T.strip t of
        "==" -> Right OpEq
        "!=" -> Right OpNe
        ">" -> Right OpGt
        ">=" -> Right OpGe
        "<" -> Right OpLt
        "<=" -> Right OpLe
        "contains" -> Right OpContains
        _ -> Left ("filter: unknown operator: " <> t)

evalPredicate :: T.Text -> Op -> T.Text -> T.Text -> Bool
evalPredicate field op lit cell =
  case field of
    "size" ->
      case (parseSizeBytes cell, parseSizeBytes lit) of
        (Just a, Just b) -> cmpNum op a b
        _ -> False
    _ ->
      let a = cell
          b = stripQuotes (T.strip lit)
       in case op of
            OpEq -> a == b
            OpNe -> a /= b
            OpContains -> T.toCaseFold b `T.isInfixOf` T.toCaseFold a
            OpGt -> a > b
            OpGe -> a >= b
            OpLt -> a < b
            OpLe -> a <= b
  where
    stripQuotes t =
      case (T.uncons t, T.unsnoc t) of
        (Just ('\"', _), Just (_, '\"')) -> T.drop 1 (T.dropEnd 1 t)
        (Just ('\'', _), Just (_, '\'')) -> T.drop 1 (T.dropEnd 1 t)
        _ -> t

cmpNum :: Op -> Int64 -> Int64 -> Bool
cmpNum op a b =
  case op of
    OpEq -> a == b
    OpNe -> a /= b
    OpGt -> a > b
    OpGe -> a >= b
    OpLt -> a < b
    OpLe -> a <= b
    OpContains -> False

parseSizeBytes :: T.Text -> Maybe Int64
parseSizeBytes t0 =
  let t = T.strip t0
      (numTxt, unitTxt) = T.span (\c -> isDigit c) t
   in case reads (T.unpack numTxt) :: [(Int64, String)] of
        [(n, "")] ->
          let unit = map toLower (T.unpack (T.strip unitTxt))
           in Just (n * unitMultiplier unit)
        _ -> Nothing

unitMultiplier :: String -> Int64
unitMultiplier u =
  case u of
    "" -> 1
    "b" -> 1
    "k" -> 1000
    "kb" -> 1000
    "m" -> 1000 * 1000
    "mb" -> 1000 * 1000
    "g" -> 1000 * 1000 * 1000
    "gb" -> 1000 * 1000 * 1000
    "kib" -> 1024
    "mib" -> 1024 * 1024
    "gib" -> 1024 * 1024 * 1024
    _ -> 1

renderStructured :: StructuredValue -> T.Text
renderStructured v =
  case v of
    SText t -> t
    STable headers rows ->
      let cols = headers
          allRows = headers : rows
          widths = map (max 1) (colWidths (List.transpose (padTo (length cols) allRows)))
          renderRow r =
            T.intercalate "  " (zipWith pad widths (take (length cols) (r ++ repeat "")))
          headerLine = renderRow headers
          sepLine = T.intercalate "  " (map (\w -> T.replicate w "-") widths)
       in T.unlines (headerLine : sepLine : map renderRow rows)
  where
    padTo n rs = map (\r -> take n (r ++ repeat "")) rs

    colWidths :: [[T.Text]] -> [Int]
    colWidths cols = map (maximum . map T.length) cols

    pad :: Int -> T.Text -> T.Text
    pad w t = t <> T.replicate (w - T.length t) " "

lsStructured :: FilePath -> IO StructuredValue
lsStructured dirPath0 = do
  dirPath <- expandTilde dirPath0
  names <- Dir.listDirectory dirPath
  let sorted = List.sort names
  rows <-
    mapM
      ( \n -> do
          let fullPath = dirPath FP.</> n
          isDir <- Dir.doesDirectoryExist fullPath
          st <- Posix.getFileStatus fullPath
          let kind = if isDir then "dir" else "file"
              sz = Posix.fileSize st
          pure [T.pack n, T.pack kind, T.pack (show sz)]
      )
      sorted
  pure (STable ["name", "kind", "size"] rows)

pwdStructured :: FilePath -> IO StructuredValue
pwdStructured dirPath = pure (SText (T.pack dirPath))

expandTilde :: FilePath -> IO FilePath
expandTilde p
  | p == "~" || p == "~/" = Dir.getHomeDirectory
  | List.isPrefixOf "~/" p = do
      home <- Dir.getHomeDirectory
      pure (home FP.</> drop 2 p)
  | otherwise = pure p
