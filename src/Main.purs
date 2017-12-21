module Main where

import Prelude

import Control.Monad.Aff (launchAff, makeAff)
import Control.Monad.Aff.AVar (makeVar')
import Control.Monad.State.Trans (evalStateT)
import Data.Either (Either(..))
import Data.StrMap (empty)
import Data.Tuple (Tuple(..))
import Presto.Core.Flow (APIRunner, Flow, PermissionCheckRunner, PermissionRunner(PermissionRunner), PermissionTakeRunner, callAPI, runUI)
import Presto.Core.Language.Runtime.Interpreter (Runtime(..), UIRunner, run)
import Presto.Core.Types.API (Header(..), Headers(..))
import Presto.Core.Types.Permission (PermissionStatus(..))
import Remote as API
import Runner (callAPI', logAny, mkNativeRequest, showUI')
import Types (MainScreen(..), MainScreenAction(..), MainScreenState(..))

main = do
  let runtime = Runtime uiRunner (PermissionRunner permissionCheckRunner permissionTakeRunner) apiRunner
      freeFlow = evalStateT (run runtime appFlow)
  launchAff (makeVar' empty >>= freeFlow)
  where
    uiRunner :: UIRunner
    uiRunner a = makeAff (\err sc -> showUI' sc a)

    apiRunner :: APIRunner
    apiRunner req = makeAff (\err sc -> callAPI' err sc (mkNativeRequest req))

    permissionCheckRunner :: PermissionCheckRunner
    permissionCheckRunner perms = pure PermissionGranted

    permissionTakeRunner :: PermissionTakeRunner
    permissionTakeRunner perms =  pure $ map (\x -> Tuple x PermissionDeclined) perms

appFlow :: Flow Unit
appFlow = do
  action <- runUI (MainScreen MainScreenInit)
  handleMainScreenAction action

handleMainScreenAction :: MainScreenAction -> Flow Unit
handleMainScreenAction action =
  case action of
    MainScreenAddTodo todoItem -> addTodoFlow todoItem
    _ -> pure unit

addTodoFlow :: String -> Flow Unit
addTodoFlow todoItem = do
  response <- callAPI (Headers [Header "Content-Type" "application/json"]) (API.AddTodoReq {todoItem: todoItem})
  case response of
    Left err -> pure unit
    Right (API.AddTodoRes {response: todoObj}) -> do
      action <- runUI (MainScreen (MainScreenAddToList todoObj))
      handleMainScreenAction action
