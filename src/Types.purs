module Types where

import Control.Monad.Eff.Exception (Error)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Generic.Rep (class Generic)
import Presto.Core.Flow (class Interact, defaultInteract)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)

-- | This is our screen which takes a state
data MainScreen = MainScreen MainScreenState

-- | These are the possible states our MainScreen could be in
data MainScreenState
  = MainScreenInit
  | MainScreenAbort
  | MainScreenAddToList String String

-- | Here we list the possible actions from the screen. For now we will just add few dummy actions
data MainScreenAction = MainScreenAddTodo String | MainScreenAbortAction

instance interactMainScreen :: Interact Error MainScreen MainScreenAction where
  interact = defaultInteract

derive instance genericMainScreen :: Generic MainScreen _
instance encodeMainScreen :: Encode MainScreen where encode = genericEncode (defaultOptions {unwrapSingleConstructors = false})

derive instance genericMainScreenState :: Generic MainScreenState _
instance encodeMainScreenState :: Encode MainScreenState where encode = defaultEncode

derive instance genericMainScreenAction :: Generic MainScreenAction _
instance decodeMainScreenAction :: Decode MainScreenAction where decode = defaultDecode
