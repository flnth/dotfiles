{-# LANGUAGE OverloadedStrings #-}
module MyAntigen where

import Antigen (
                -- Rudimentary imports
                AntigenConfig (..)
              , defaultConfig
              , bundle
              , antigen
                -- If you want to source a bit trickier plugins
              , ZshPlugin (..)
              , antigenSourcingStrategy
              )

bundles =
  [
    bundle "zsh-users/zsh-completions"
  , bundle "Tarrasch/zsh-bd"
  , bundle "Tarrasch/zsh-command-not-found"
  , bundle "frmendes/geometry"
  , bundle "willghatch/zsh-hooks"
  , bundle "zsh-users/zsh-syntax-highlighting"
  , bundle "zsh-users/zsh-history-substring-search"
  , bundle "zdharma/zsh-diff-so-fancy"
  ]

config = defaultConfig { plugins = bundles }

main :: IO ()
main = antigen config
