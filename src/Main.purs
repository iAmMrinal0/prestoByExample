module Main where

import Prelude

import Control.Monad.Aff (launchAff, makeAff)
import Control.Monad.Aff.AVar (makeVar')
import Control.Monad.State.Trans (evalStateT)
import Data.StrMap (empty)
import Data.Tuple (Tuple(..))
import Presto.Core.Flow (APIRunner, PermissionCheckRunner, PermissionRunner(PermissionRunner), PermissionTakeRunner, Flow)
import Presto.Core.Language.Runtime.Interpreter (Runtime(..), UIRunner, run)
import Presto.Core.Types.Permission (PermissionStatus(..))
import Runner (callAPI', mkNativeRequest, showUI')

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
appFlow = pure unit
