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
  , _dexterity     :: Int
  , _constitution  :: Int
  , _intelligence  :: Int
  } deriving (Show, Eq)

data Model
  = NewCharacter CharacterOutline
  | CharacterInCreation FullCharacter
  | CreatedCharacter FullCharacter
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- class' :: Lens Model String
-- class' = lens _class $ \record field -> record { _class = field }

-- selectedClass :: Lens Model String
-- selectedClass = lens _selectedClass $ \record field -> record { _selectedClass = field }

-- strength :: Lens Model Int
-- strength = lens _strength $ \record field -> record { _strength = field }

-- dexterity :: Lens Model Int
-- dexterity = lens _dexterity $ \record field -> record { _dexterity = field }

-- constitution :: Lens Model Int
-- constitution = lens _constitution $ \record field -> record { _constitution = field }

-- intelligence :: Lens Model Int
-- intelligence = lens _intelligence $ \record field -> record { _intelligence = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = SetClass String
  | ConfirmClass
  | IncrementStrength
  | DecrementStrength
  | IncrementDexterity
  | DecrementDexterity
  | IncrementConstitution
  | DecrementConstitution
  | IncrementIntelligence
  | DecrementIntelligence
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
                                                            , _dexterity     = 5
                                                            , _constitution  = 5
                                                            , _intelligence  = 5
                                                            })
    x                 -> x
  IncrementStrength     -> modify $ \case
    CharacterInCreation char -> CharacterInCreation (char { _strength = _strength char + 1 })
    x                        -> x
  DecrementStrength     -> modify $ \case
    CharacterInCreation char -> CharacterInCreation (char { _strength = _strength char - 1 })
    x                        -> x
  IncrementDexterity    -> modify $ \case
    CharacterInCreation char -> CharacterInCreation (char { _dexterity = _dexterity char + 1 })
    x                        -> x
  DecrementDexterity    -> modify $ \case
    CharacterInCreation char -> CharacterInCreation (char { _dexterity = _dexterity char - 1 })
    x                        -> x
  IncrementConstitution -> modify $ \case
    CharacterInCreation char -> CharacterInCreation (char { _constitution = _constitution char + 1 })
    x                        -> x
  DecrementConstitution -> modify $ \case
    CharacterInCreation char -> CharacterInCreation (char { _constitution = _constitution char - 1 })
    x                        -> x
  IncrementIntelligence -> modify $ \case
    CharacterInCreation char -> CharacterInCreation (char { _intelligence = _intelligence char + 1 })
    x                        -> x
  DecrementIntelligence -> modify $ \case
    CharacterInCreation char -> CharacterInCreation (char { _intelligence = _intelligence char - 1 })
    x                        -> x
  FinalizeCharacter     -> modify $ \case
    CharacterInCreation char -> CreatedCharacter char
    x                        -> x
----------------------------------------------------------------------------
-- | List of character attributes with their accessors and actions
attributes :: [(String, FullCharacter -> Int, Action, Action)]
attributes = [ ("Strength", _strength, IncrementStrength, DecrementStrength)
             , ("Dexterity", _dexterity, IncrementDexterity, DecrementDexterity)
             , ("Constitution", _constitution, IncrementConstitution, DecrementConstitution)
             , ("Intelligence", _intelligence, IncrementIntelligence, DecrementIntelligence)
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
