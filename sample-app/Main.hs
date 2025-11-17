----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso hiding (set)
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens
import Data.Maybe (catMaybes)
----------------------------------------------------------------------------
-- | Component model state
newtype CharacterOutline = CharacterOutline
  { _class :: String
  } deriving (Show, Eq)

data FullCharacter = FullCharacter
  { _selectedClass :: String
  , _strength      :: Int
  } deriving (Show, Eq)

data Model
  = NewCharacter CharacterOutline
  | CharacterInCreation FullCharacter
  | CreatedCharacter FullCharacter
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = SetClass String
  | ConfirmClass
  | IncrementStrength
  | DecrementStrength
  | FinalizeCharacter
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run (startApp app)
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = component emptyModel updateModel viewModel
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: Model
emptyModel = NewCharacter (CharacterOutline "")
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel = \case
  SetClass cls          -> modify $ \case
    NewCharacter char -> NewCharacter (char { _class = cls })
    x                 -> x
  ConfirmClass          -> modify $ \case
    NewCharacter char -> CharacterInCreation (FullCharacter { _selectedClass = _class char
                                                            , _strength      = 5
                                                            })
    x                 -> x
  IncrementStrength     -> modify $ \case
    CharacterInCreation char -> CharacterInCreation (char { _strength = _strength char + 1 })
    x                        -> x
  DecrementStrength     -> modify $ \case
    CharacterInCreation char -> CharacterInCreation (char { _strength = _strength char - 1 })
    x                        -> x
  FinalizeCharacter     -> modify $ \case
    CharacterInCreation char -> CreatedCharacter char
    x                        -> x
----------------------------------------------------------------------------
-- | List of character attributes with their accessors and actions
attributes :: [(String, FullCharacter -> Int, Action, Action)]
attributes = [ ("Strength", _strength, IncrementStrength, DecrementStrength)
             ]

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel x =
  H.div_
    [ P.className "character-creator"
    ]
    (case x of
      NewCharacter char ->
        [ H.h1_
          []
          [ text "Create Your Character"
          ]
        , H.input_
          [ P.type_ "text"
          , P.placeholder_ "Enter class"
          , P.value_ (ms (_class char))
          , H.onInput (SetClass . fromMisoString)
          ]
        , H.button_
          (catMaybes [ Just (H.onClick ConfirmClass)
          , if _class char == "" then Just P.disabled_ else Nothing
          ])
          [ text "Confirm Class" ]
        ]
      CharacterInCreation char ->
        [ H.h1_
          []
          [ text $ ms (_selectedClass char)
          ]
        , H.ul_
          []
          (map (\(name, acc, incAction, decAction) ->
            H.li_ []
              [ H.strong_ [] [ text $ ms (name ++ ": ") ]
              , H.button_ [ H.onClick decAction ] [ text "-" ]
              , H.span_ [] [ text $ ms (acc char) ]
              , H.button_ [ H.onClick incAction ] [ text "+" ]
              ]
          ) attributes)
        , H.button_
          [ H.onClick FinalizeCharacter ]
          [ text "Finalize Character" ]
        ]
      CreatedCharacter char ->
        [ H.h1_
          []
          [ text $ ms (_selectedClass char)
          ]
        , H.ul_
          []
          (map (\(name, acc, _, _) ->
            H.li_ []
              [ H.strong_ [] [ text $ ms (name ++ ": ") ]
              , H.span_ [] [ text $ ms (acc char) ]
              ]
          ) attributes)
        ]
    )
----------------------------------------------------------------------------
