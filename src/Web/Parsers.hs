{-# LANGUAGE OverloadedStrings #-}
module Web.Parsers where

import Data.Attoparsec.Text as A
import Data.Text
import Control.Applicative
import Data.Char


convertUnicode :: Text -> Either String Text
convertUnicode = parseOnly unicode


unicode :: Parser Text
unicode = pack <$> many' unicodeChar


unicodeChar :: Parser Char
unicodeChar = escapedChar <|> anyChar


escapedChar :: Parser Char
escapedChar = do
    _ <- string "\\u"
    h <- A.take 4
    case chr <$> parseOnly hexadecimal h of
        Right c -> return c
        Left l -> fail l
