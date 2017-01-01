module Web.Gathering.Html where

import qualified Lucid as L
import Lucid.Html5

import Web.Gathering.Model
import Web.Gathering.Utils


template :: String -> L.Html () -> L.Html () -> L.Html () -> L.Html ()
template title header nav body =
  html_ $ do
    head_ $ do
      title_ (L.toHtml title)
      link_ [rel_ "stylesheet", type_ "text/css", href_ "static/style.css"]
    body_ $ do
      div_ [id_ "header"] header
      nav_ [id_ "nav"] nav
      div_ [id_ "main"] body
      footer_ [id_ "footer"] $
        p_ $ do
          "Powered by "
          a_ [href_ "https://gitlab.com/gilmi/gathering"] "Gathering"

main :: String -> L.Html () -> L.Html () -> L.Html () -> L.Html ()
main title header nav body = undefined
