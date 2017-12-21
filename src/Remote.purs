module Remote where

import Data.Foreign.Class (class Decode, class Encode)
import Data.Generic.Rep (class Generic)
import Presto.Core.Types.API (class RestEndpoint, Method(..), defaultDecodeResponse, defaultMakeRequest, defaultMakeRequest_)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)

data TodoReq = TodoReq
newtype TodoRes = TodoRes
  { code :: Int
  , status :: String
  , response :: Array TodoItem
  }

newtype TodoItem = TodoItem
  { id :: Number
  , value :: String
  }

instance makeTodoReq :: RestEndpoint TodoReq TodoRes where
  makeRequest _ headers = defaultMakeRequest_ GET "http://localhost:3000" headers
  decodeResponse body = defaultDecodeResponse body

derive instance genericTodoReq :: Generic TodoReq _
instance encodeTodoReq :: Encode TodoReq where encode = defaultEncode

derive instance genericTodoRes :: Generic TodoRes _
instance decodeTodoRes :: Decode TodoRes where decode = defaultDecode

derive instance genericTodoItem :: Generic TodoItem _
instance encodeTodoItem :: Encode TodoItem where encode = defaultEncode
instance decodeTodoItem :: Decode TodoItem where decode = defaultDecode

newtype AddTodoReq = AddTodoReq
  { todoItem :: String
  }

newtype AddTodoRes = AddTodoRes
  { code :: Int
  , status :: String
  , response :: TodoItem
  }

instance makeAddTodoReq :: RestEndpoint AddTodoReq AddTodoRes where
  makeRequest reqBody headers = defaultMakeRequest POST "http://localhost:3000/add" headers reqBody
  decodeResponse body = defaultDecodeResponse body

derive instance genericAddTodoReq :: Generic AddTodoReq _
instance encodeAddTodoReq :: Encode AddTodoReq where encode = defaultEncode

derive instance genericAddTodoRes :: Generic AddTodoRes _
instance decodeAddTodoRes :: Decode AddTodoRes where decode = defaultDecode
