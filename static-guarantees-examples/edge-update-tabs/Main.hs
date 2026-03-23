-- | Haskell language pragma
{-# LANGUAGE CPP               #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.FFI
import Miso.String

-- | JSAddle import
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class

-- | Other imports
import           Data.Kind                        (Type)
import           Data.Maybe                       (catMaybes, fromMaybe)
import           Network.HTTP
import Text.Read (readMaybe)

-- | Type synonym for an application model
data Model a where
  Statistics :: Int -> Model StatisticsAction
  Notes :: String -> Model NotesAction

instance Show (Model a) where
  show (Statistics score) = "Statistics " ++ show score
  show (Notes text)       = "Notes " ++ show text

instance Eq (Model a) where
  (Statistics score1) == (Statistics score2) = score1 == score2
  (Notes text1)       == (Notes text2)       = text1 == text2

-- | Sum types for application events
data StatisticsAction
  = NoOp
  | FetchScore
  | UpdateScore Int
  | GoToNotes
  deriving (Show, Eq)

data NotesAction
  = UpdateNotes String
  | GoToStatistics
  deriving (Show, Eq)

#ifndef ghcjs_HOST_OS
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 8080 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp
    model  = Statistics 0
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing
    logLevel = Off

-- | Updates model, optionally introduces side effects
updateModel :: Model action -> action -> AnyEffect Model
updateModel (Statistics score) a = case a of
    NoOp -> noEff $ Statistics score
    FetchScore -> Statistics score
                         <# do
                           res <- liftIO $ simpleHTTP (getRequest "http://localhost:3456")
                           _ <- consoleLog $ ms (show res)
                           pure (UpdateScore (score + 1))
    UpdateScore newScore -> noEff $ Statistics newScore
    GoToNotes -> noEff $ Notes ""
updateModel (Notes text) a = case a of
    UpdateNotes newNotes -> noEff $ Notes newNotes
    GoToStatistics -> noEff $ Statistics 0

-- | Constructs a virtual DOM from a model
viewModel :: Model action -> View action
viewModel (Statistics score) =
  div_
    []
    [ h1_
      []
      [ text "Statistics"
      ]
    , div_
      []
      [ button_
        [ disabled_ True ]
        [ text "Statistics" ]
      , button_
        [ onClick GoToNotes ]
        [ text "Notes" ]
      ]
    , div_
      []
      [ text (ms ("Score: " ++ show score))]
    , button_
      [ onClick FetchScore
      ]
      [ text "Update Score" ]
    ]
viewModel (Notes notes) =
  div_
    []
    [ h1_
      []
      [ text "Notes"
      ]
    , div_
      []
      [ button_
        [ onClick GoToStatistics ]
        [ text "Statistics" ]
      , button_
        [ disabled_ True ]
        [ text "Notes" ]
      ]
    , input_
      [ type_ "text"
      , placeholder_ "Your Notes"
      , value_ (ms notes)
      , onInput (UpdateNotes . fromMisoString)
      ]
    ]
