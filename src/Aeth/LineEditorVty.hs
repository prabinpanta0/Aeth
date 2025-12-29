{-# LANGUAGE OverloadedStrings #-}

module Aeth.LineEditorVty
  ( LineEditor,
    withLineEditor,
    getLineEdited,
  )
where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Config as VC
import qualified Graphics.Vty.Platform.Unix as VU

-- Fullscreen Vty-based editor (optional mode)

newtype LineEditor = LineEditor
  { vty :: V.Vty
  }

withLineEditor :: (LineEditor -> IO a) -> IO a
withLineEditor action = do
  v <- VU.mkVty VC.defaultConfig
  let ed = LineEditor v
  r <- action ed
  V.shutdown v
  pure r

getLineEdited :: LineEditor -> [T.Text] -> String -> [String] -> IO (Maybe String)
getLineEdited ed scrollback prompt history = loop "" 0
  where
    loop :: String -> Int -> IO (Maybe String)
    loop buf cursor = do
      render buf cursor
      ev <- V.nextEvent (vty ed)
      case ev of
        V.EvKey (V.KChar 'd') [V.MCtrl] -> pure Nothing
        V.EvKey V.KEnter [] -> pure (Just buf)
        V.EvKey V.KLeft [] -> loop buf (max 0 (cursor - 1))
        V.EvKey V.KRight [] -> loop buf (min (length buf) (cursor + 1))
        V.EvKey V.KHome [] -> loop buf 0
        V.EvKey V.KEnd [] -> loop buf (length buf)
        V.EvKey V.KBS [] ->
          if cursor <= 0
            then loop buf cursor
            else loop (take (cursor - 1) buf <> drop cursor buf) (cursor - 1)
        V.EvKey V.KDel [] ->
          if cursor >= length buf
            then loop buf cursor
            else loop (take cursor buf <> drop (cursor + 1) buf) cursor
        V.EvKey (V.KChar '\t') [] ->
          let s = suggestion buf history
           in case s of
                Nothing -> loop buf cursor
                Just sug -> loop sug (length sug)
        V.EvKey (V.KChar c) [] ->
          if c == '\n' || c == '\r'
            then loop buf cursor
            else
              let (a, b) = splitAt cursor buf
                  newBuf = a <> [c] <> b
               in loop newBuf (cursor + 1)
        _ -> loop buf cursor

    render :: String -> Int -> IO ()
    render buf cursor = do
      (w, h) <- V.displayBounds (V.outputIface (vty ed))
      let bufT = T.pack buf
      let sug = suggestion buf history
      let sugRemainder = case sug of
            Nothing -> ""
            Just s -> drop (length buf) s
      let promptImg = V.string (V.defAttr) prompt
      let inputImg = highlight bufT
      let sugImg = V.string (V.defAttr `V.withStyle` V.dim) sugRemainder
      let promptLine = promptImg V.<|> inputImg V.<|> sugImg

      let maxLines = max 0 (h - 1)
      let visible = takeLast maxLines scrollback
      let outImgs = map (renderOutLine w) visible

      let blanksNeeded = max 0 (maxLines - length outImgs)
      let blankLine = V.charFill V.defAttr ' ' (max 0 w) 1
      let filler = replicate blanksNeeded blankLine

      let outBlock = V.vertCat (filler ++ outImgs)
      let fullImg = outBlock V.<-> promptLine

      let cursorX = length prompt + cursor
      let cursorY = length filler + length outImgs
      let pic = (V.picForImage fullImg) {V.picCursor = V.Cursor cursorX cursorY}
      V.update (vty ed) pic

    renderOutLine :: Int -> T.Text -> V.Image
    renderOutLine w t =
      let s = T.unpack t
          clipped = take (max 0 w) s
       in V.string V.defAttr clipped

    takeLast :: Int -> [a] -> [a]
    takeLast n xs
      | n <= 0 = []
      | otherwise = drop (length xs - min n (length xs)) xs

highlight :: T.Text -> V.Image
highlight t = V.horizCat (map tokenToImg (lexTokens t))
  where
    tokenToImg (TokPipe s) = V.string (V.defAttr `V.withForeColor` V.yellow) (T.unpack s)
    tokenToImg (TokCmd s) =
      let str = T.unpack s
       in if "@" `isPrefixOf` str
            then V.string (V.defAttr `V.withForeColor` V.green) str
            else V.string (V.defAttr `V.withForeColor` V.cyan) str
    tokenToImg (TokArg s) = V.string (V.defAttr) (T.unpack s)
    tokenToImg (TokSpace s) = V.string V.defAttr (T.unpack s)

suggestion :: String -> [String] -> Maybe String
suggestion prefix hs
  | all isSpace prefix = Nothing
  | otherwise =
      case filter (isPrefixOf prefix) hs of
        (x : _) -> Just x
        [] -> Nothing

data Tok
  = TokPipe T.Text
  | TokSpace T.Text
  | TokCmd T.Text
  | TokArg T.Text

lexTokens :: T.Text -> [Tok]
lexTokens = go True
  where
    go _ s | T.null s = []
    go isCmdPos s =
      case T.uncons s of
        Just ('|', rest) -> TokPipe "|" : go True rest
        Just (c, _)
          | isSpace c ->
              let (sp, rest) = T.span isSpace s
               in TokSpace sp : go isCmdPos rest
        _ ->
          let (w, rest) = breakToken s
              tok = if isCmdPos then TokCmd w else TokArg w
           in tok : go False rest

    breakToken :: T.Text -> (T.Text, T.Text)
    breakToken = T.break (\c -> isSpace c || c == '|')
