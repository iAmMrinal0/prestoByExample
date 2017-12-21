module Remote where

import Data.Foreign.Class (class Decode, class Encode)
import Data.Generic.Rep (class Generic)
import Presto.Core.Types.API (class RestEndpoint, Method(GET), defaultDecodeResponse, defaultMakeRequest_)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)

data TodoReq = TodoReq
newtype TodoRes = TodoRes
  { code :: Int
  , status :: String
  , response :: String
  }

instance makeTodoReq :: RestEndpoint TodoReq TodoRes where
  makeRequest _ headers = defaultMakeRequest_ GET "http://localhost:3000" headers
  decodeResponse body = defaultDecodeResponse body

derive instance genericTodoReq :: Generic TodoReq _
instance encodeTodoReq :: Encode TodoReq where encode = defaultEncode

derive instance genericTodoRes :: Generic TodoRes _
instance decodeTodoRes :: Decode TodoRes where decode = defaultDecode
