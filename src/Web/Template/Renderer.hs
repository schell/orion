{-# LANGUAGE OverloadedStrings #-}
module Web.Template.Renderer (
  blaze,
  blazePretty
) where

import           Web.Orion.Types
import           Web.Scotty.Trans
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as B

-- | Render some Blaze Html
--
blaze :: Html -> ActionOM ()
blaze h = do
  setHeader "Content-Type" "text/html"
  raw $ Utf8.renderHtml h

blazePretty :: Html -> ActionOM ()
blazePretty h = do
    setHeader "Content-Type" "text/html"
    raw $ B.pack $ Pretty.renderHtml h


